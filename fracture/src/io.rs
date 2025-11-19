use std::io::{self, Result, Error, ErrorKind};
use std::pin::Pin;
use std::task::{Context, Poll};
use std::future::Future;
pub trait AsyncRead {
    fn poll_read(self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &mut ReadBuf<'_>) -> Poll<Result<()>>;
}

pub trait AsyncWrite {
    fn poll_write(self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &[u8]) -> Poll<Result<usize>>;
    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>>;
    fn poll_shutdown(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>>;
}

pub trait AsyncSeek {
    fn start_seek(self: Pin<&mut Self>, position: io::SeekFrom) -> Result<()>;
    fn poll_complete(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<u64>>;
}

pub trait AsyncBufRead: AsyncRead {
    fn poll_fill_buf(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<&[u8]>>;
    fn consume(self: Pin<&mut Self>, amt: usize);
}

#[derive(Debug)]
pub struct ReadBuf<'a> {
    buf: &'a mut [u8],
    filled: usize,
    initialized: usize
}

impl<'a> ReadBuf<'a> {
    pub fn new(buf: &'a mut [u8]) -> Self {
        let len = buf.len();
        Self { buf, filled: 0, initialized: len }
    }

    pub fn capacity(&self) -> usize {
        self.buf.len()
    }

    pub fn filled(&self) -> &[u8] {
        &self.buf[..self.filled]
    }

    pub fn filled_mut(&mut self) -> &mut [u8] {
        &mut self.buf[..self.filled]
    }

    pub fn remaining(&self) -> usize {
        self.capacity() - self.filled
    }

    pub fn clear(&mut self) {
        self.filled = 0;
    }

    pub fn advance(&mut self, n: usize) {
        self.filled += n;
    }

    pub fn put_slice(&mut self, buf: &[u8]) {
        let len = buf.len();
        self.buf[self.filled..self.filled + len].copy_from_slice(buf);
        self.filled += len;
    }
}

pub fn split<T>(stream: T) -> (ReadHalf<T>, WriteHalf<T>)
where T: AsyncRead + AsyncWrite {
    let (r, w) = std::sync::Arc::new(std::sync::Mutex::new(stream)).into();

    (ReadHalf { inner: r}, WriteHalf { inner: w })
}

pub struct ReadHalf<T> {
    inner: std::sync::Arc<std::sync::Mutex<T>>
}
pub struct WriteHalf<T> {
    inner: std::sync::Arc<std::sync::Mutex<T>>
}

impl<T: AsyncRead> AsyncRead for ReadHalf<T> {
    fn poll_read(self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &mut ReadBuf<'_>) -> Poll<Result<()>> {
        let mut inner = self.inner.lock().unwrap();

        unsafe { Pin::new_unchecked(&mut *inner).poll_read(cx, buf) }
    }
}

impl<T: AsyncWrite> AsyncWrite for WriteHalf<T> {
    fn poll_write(self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &[u8]) -> Poll<Result<usize>> {
        let mut inner = self.inner.lock().unwrap();

        unsafe { Pin::new_unchecked(&mut *inner).poll_write(cx, buf) }
    }

    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>> {
        let mut inner = self.inner.lock().unwrap();

        unsafe { Pin::new_unchecked(&mut *inner).poll_flush(cx) }
    }

    fn poll_shutdown(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>> {
        let mut inner = self.inner.lock().unwrap();

        unsafe { Pin::new_unchecked(&mut *inner).poll_shutdown(cx) }
    }
}

pub async fn copy<'a, R, W>(reader: &'a mut R, writer: &'a mut W) -> Result<u64>
where R: AsyncRead + Unpin + ?Sized, W: AsyncWrite + Unpin + ?Sized {
    let buf = vec![0u8; 8192];
    let mut total = 0;

    loop {
        let mut read_buf = ReadBuf::new(&mut buf);
        
        match std::future::poll_fn(|cx| Pin::new(&mut *reader).poll_read(cx, &mut read_buf)).await {
            Ok(()) => {
                let n = read_buf.filled.len();
                if n == 0 {
                    return Ok(total)
                }

                let mut pos = 0;
                while pos < n {
                    let written = std::future::poll_fn(|cx| Pin::new(&mut *writer).poll_write(cx, &read_buf.filled()[pos..])).await;
                    if written == 0 {
                        return Err(Error::new(ErrorKind::WriteZero, "fracture: Write zero"));
                    }

                    pos += written;
                    total += written;
                }
            }
            Err(e) => return Err(e)
        }
    }
}

pub trait AsyncReadExt: AsyncRead {
    fn read<'a>(&'a mut self, buf: &'a mut [u8]) -> ReadFuture<'a, Self>
    where Self: Unpin + Sized {
        ReadFuture {
            reader: self,
            buf
        }
    }

    fn read_exact<'a>(&'a mut self, buf: &'a mut [u8]) -> ReadExactFuture<'a, Self>
    where Self: Unpin + Sized {
        ReadExactFuture {
            reader: self,
            buf,
            pos: 0
        }
    }

    fn read_to_end<'a>(&'a mut self, buf: &'a mut Vec<u8>) -> ReadToEndFuture<'a, Self>
    where Self: Unpin + Sized {
        ReadToEndFuture {
            reader: self,
            buf
        }
    }
}

impl<R: AsyncRead + ?Sized> AsyncReadExt for R {}

pub trait AsyncWriteExt: AsyncWrite {
    fn write<'a>(&'a mut self, buf: &'a [u8]) -> WriteFuture<'a, Self>
    where Self: Unpin + Sized {
        WriteFuture {
            writer: self,
            buf
        }
    }

    fn write_all<'a>(&'a mut self, buf: &'a [u8]) -> WriteAllFuture<'a, Self>
    where Self: Unpin + Sized {
        WriteAllFuture { writer: self,
            buf,
            pos: 0
        }
    }

    fn flush(&mut self) -> FlushFuture<'_, Self>
    where Self: Unpin + Sized {
        FlushFuture {
            writer: self
        }
    }

    fn shutdown(&mut self) -> ShutdownFuture<'_, Self>
    where Self: Unpin + Sized {
        ShutdownFuture {
            writer: self
        }
    }
}

impl<W: AsyncWrite + ?Sized> AsyncWriteExt for W {}

pub struct ReadFuture<'a, R: ?Sized> {
    reader: &'a mut R,
    buf: &'a mut [u8]
}

impl<R: AsyncRead + Unpin + ?Sized> Future for ReadFuture<'_, R> {
    type Output = Result<usize>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut buf = ReadBuf::new(self.buf);

        match Pin::new(&mut *self.reader).poll_read(cx, &mut buf) {
            Poll::Ready(Ok(())) => Poll::Ready(Ok(buf.filled().len())),
            Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
            Poll::Pending => Poll::Pending
        }
    }
}

pub struct ReadExactFuture<'a, R: ?Sized> {
    reader: &'a mut R,
    buf: &'a mut [u8],
    pos: usize
}

impl<R: AsyncRead + Unpin + ?Sized> Future for ReadExactFuture<'_, R> {
    type Output = Result<usize>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        if self.pos >= self.buf.len() {
            return Poll::Ready(Ok(self.pos));
        }

        let mut buf = ReadBuf::new(&mut self.buf[self.pos..]);

        match Pin::new(&mut *self.reader).poll_read(cx, &mut buf) {
            Poll::Ready(Ok(())) => {
                let n = buf.filled().len();
                if n == 0 {
                    return Poll::Ready(Err(Error::new(ErrorKind::UnexpectedEof, "fracture: Unexpected EOF")));
                }

                self.pos += n;
            }
            Poll::Ready(Err(e)) => return Poll::Ready(Err(e)),
            Poll::Pending => return Poll::Pending
        }
    }
}

pub struct ReadToEndFuture<'a, R: ?Sized> {
    reader: &'a mut R,
    buf: &'a mut Vec<u8>
}

impl<R: AsyncRead + Unpin + ?Sized> Future for ReadToEndFuture<'_, R> {
    type Output = Result<usize>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let start_len = self.buf.len();

        loop {
            if self.buf.capacity() == self.buf.len() {
                self.buf.reserve(32);
            }

            let len = self.buf.len();
            let cap = self.buf.capacity();
            self.buf.resize(cap, 0);
            let mut buf = ReadBuf::new(&mut self.buf[len..]);

            match Pin::new(&mut *self.reader).poll_read(cx, &mut buf) {
                Poll::Ready(Ok(())) => {
                    let n = buf.filled().len();
                    self.buf.truncate(len + n);
                    if n == 0 {
                        return Poll::Ready(Ok(self.buf.len() - start_len));
                    }
                }
                Poll::Ready(Err(e)) => {
                    self.buf.truncate(len);
                    return Poll::Ready(Err(e));
                }
                Poll::Pending => {
                    self.buf.truncate(len);
                    return Poll::Pending;
                }
            }
        }
    }
}

pub struct WriteFuture<'a, W: ?Sized> {
    writer: &'a mut W,
    buf: &'a [u8]
}

impl<W: AsyncWrite + Unpin + ?Sized> Future for WriteFuture<'_, W> {
    type Output = Result<usize>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        Pin::new(&mut *self.writer).poll_write(cx, self.buf)
    }
}

pub struct WriteAllFuture<'a, W: ?Sized> {
    writer: &'a mut W,
    buf: &'a [u8],
    pos: usize
}

impl<W: AsyncWrite + Unpin + ?Sized> Future for WriteAllFuture<'_, W> {
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        while self.pos < self.buf.len() {
            match Pin::new(&mut *self.writer).poll_write(cx, &self.buf[self.pos..]) {
                Poll::Ready(Ok(0)) => Poll::Ready(Err(Error::new(ErrorKind::WriteZero, "fracture: Write zero"))),
                Poll::Ready(Ok(n)) => self.pos += n,
                Poll::Ready(Err(e)) => return Poll::Ready(Err(e)),
                Poll::Pending => return Poll::Pending
            }
        }

        Poll::Ready(Ok(()))
    }
}

pub struct FlushFuture<'a, W: ?Sized> {
    writer: &'a mut W
}

impl<W: AsyncWrite + Unpin + ?Sized> Future for FlushFuture<'_, W> {
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        Pin::new(&mut *self.writer).poll_flush(cx)
    }
}

pub struct ShutdownFuture<'a, W: ?Sized> {
    writer: &'a mut W
}

impl<W: AsyncWrite + Unpin + ?Sized> Future for ShutdownFuture<'_, W> {
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        Pin::new(&mut *self.writer).poll_shutdown(cx)
    }
}