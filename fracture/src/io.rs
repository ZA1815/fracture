use std::io::{self, Result, Error, ErrorKind, SeekFrom};
use std::pin::Pin;
use std::task::{Context, Poll};
use std::future::Future;
use pin_project::pin_project;

use crate::chaos::{self, ChaosOperation};

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

impl<T: AsyncRead + Unpin + ?Sized> AsyncRead for Box<T> {
    fn poll_read(mut self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &mut ReadBuf<'_>) -> Poll<Result<()>> {
        Pin::new(&mut **self).poll_read(cx, buf)
    }
}

impl<T: AsyncWrite + Unpin + ?Sized> AsyncWrite for Box<T> {
    fn poll_write(mut self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &[u8]) -> Poll<Result<usize>> {
        Pin::new(&mut **self).poll_write(cx, buf)
    }
    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>> {
        Pin::new(&mut **self).poll_flush(cx)
    }
    fn poll_shutdown(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>> {
        Pin::new(&mut **self).poll_shutdown(cx)
    }
}

impl AsyncRead for &[u8] {
    fn poll_read(mut self: Pin<&mut Self>, _cx: &mut Context<'_>, buf: &mut ReadBuf<'_>) -> Poll<Result<()>> {
        let amt = std::cmp::min(self.len(), buf.remaining());
        let (a, b) = self.split_at(amt);
        buf.put_slice(a);
        *self = b;
        Poll::Ready(Ok(()))
    }
}

impl<T: AsRef<[u8]> + Unpin> AsyncRead for io::Cursor<T> {
    fn poll_read(self: Pin<&mut Self>, _cx: &mut Context<'_>, buf: &mut ReadBuf<'_>) -> Poll<Result<()>> {
        let reader = self.get_mut();
        let pos = reader.position() as usize;
        let inner = reader.get_ref().as_ref();
        
        if pos >= inner.len() {
            return Poll::Ready(Ok(()));
        }
        
        let amt = std::cmp::min(inner.len() - pos, buf.remaining());
        buf.put_slice(&inner[pos..pos + amt]);
        reader.set_position((pos + amt) as u64);
        
        Poll::Ready(Ok(()))
    }
}

impl<T: AsRef<[u8]> + Unpin> AsyncBufRead for io::Cursor<T> {
    fn poll_fill_buf(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<&[u8]>> {
        let reader = self.get_mut();
        let pos = reader.position() as usize;
        let inner = reader.get_ref().as_ref();
        
        if pos >= inner.len() {
            return Poll::Ready(Ok(&[]));
        }
        
        Poll::Ready(Ok(&inner[pos..]))
    }

    fn consume(self: Pin<&mut Self>, amt: usize) {
        let reader = self.get_mut();
        let pos = reader.position();
        reader.set_position(pos + amt as u64);
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
    if chaos::should_fail(ChaosOperation::IoCopy) {
        return Err(Error::new(ErrorKind::Other, "fracture: Copy failed (chaos)"));
    }

    let mut buf = vec![0u8; 8192];
    let mut total = 0;

    loop {
        let mut read_buf = ReadBuf::new(&mut buf);
        
        match std::future::poll_fn(|cx| Pin::new(&mut *reader).poll_read(cx, &mut read_buf)).await {
            Ok(()) => {
                let n = read_buf.filled().len();
                if n == 0 {
                    return Ok(total)
                }

                let mut pos = 0;
                while pos < n {
                    let written = std::future::poll_fn(|cx| Pin::new(&mut *writer).poll_write(cx, &read_buf.filled()[pos..])).await?;
                    if written == 0 {
                        return Err(Error::new(ErrorKind::WriteZero, "fracture: Write zero"));
                    }

                    pos += written;
                    total += written as u64;
                }
            }
            Err(e) => return Err(e)
        }
    }
}

pub async fn copy_bidirectional<A, B>(a: &mut A, b: &mut B) -> Result<(u64, u64)>
where
    A: AsyncRead + AsyncWrite + Unpin + ?Sized,
    B: AsyncRead + AsyncWrite + Unpin + ?Sized,
{
    if chaos::should_fail(ChaosOperation::IoCopyBidirectional) {
         return Err(Error::new(ErrorKind::Other, "fracture: CopyBidirectional failed (chaos)"));
    }

    let mut a_to_b = copy(a, b);
    let mut b_to_a = copy(b, a);

    crate::select! {
        r1 = &mut a_to_b => {
            let r2 = b_to_a.await;
            Ok((r1?, r2?))
        },
        r2 = &mut b_to_a => {
            let r1 = a_to_b.await;
            Ok((r1?, r2?))
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

    fn read_to_string<'a>(&'a mut self, buf: &'a mut String) -> ReadToStringFuture<'a, Self>
    where Self: Unpin + Sized {
        ReadToStringFuture {
            reader: self,
            buf
        }
    }

    fn chain<R: AsyncRead>(self, next: R) -> Chain<Self, R>
    where Self: Sized {
        Chain {
            first: self,
            second: next,
            done_first: false
        }
    }

    fn take(self, limit: u64) -> Take<Self>
    where Self: Sized {
        Take {
            inner: self,
            limit
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

pub trait AsyncBufReadExt: AsyncBufRead {
    fn read_until<'a>(&'a mut self, byte: u8, buf: &'a mut Vec<u8>) -> ReadUntilFuture<'a, Self>
    where Self: Unpin + Sized {
        ReadUntilFuture {
            reader: self,
            delimiter: byte,
            buf,
            read: 0
        }
    }

    fn read_line<'a>(&'a mut self, buf: &'a mut String) -> ReadLineFuture<'a, Self>
    where Self: Unpin + Sized {
        ReadLineFuture {
            reader: self,
            buf,
            bytes: Vec::new(),
            read: 0
        }
    }

    fn lines(self) -> Lines<Self> 
    where Self: Sized {
        Lines {
            reader: self
        }
    }
}

impl<R: AsyncBufRead + ?Sized> AsyncBufReadExt for R {}

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

pub struct ReadToStringFuture<'a, R: ?Sized> {
    reader: &'a mut R,
    buf: &'a mut String
}

impl<R: AsyncRead + Unpin + ?Sized> Future for ReadToStringFuture<'_, R> {
    type Output = Result<usize>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // Simplistic implementation, upgrade to UTF-8 validation incrementally later
        let mut bytes = std::mem::take(unsafe { self.buf.as_mut_vec() });
        let start_len = bytes.len();
        
        let mut read_fut = ReadToEndFuture {
            reader: self.reader,
            buf: &mut bytes
        };
        
        match Pin::new(&mut read_fut).poll(cx) {
            Poll::Ready(Ok(n)) => {
                match String::from_utf8(bytes) {
                    Ok(s) => {
                        *self.buf = s;
                        Poll::Ready(Ok(n))
                    }
                    Err(e) => {
                        *unsafe { self.buf.as_mut_vec() } = e.into_bytes();
                        Poll::Ready(Err(Error::new(ErrorKind::InvalidData, "stream did not contain valid UTF-8")))
                    }
                }
            }
            Poll::Ready(Err(e)) => {
                *unsafe { self.buf.as_mut_vec() } = bytes;
                Poll::Ready(Err(e))
            }
            Poll::Pending => {
                *unsafe { self.buf.as_mut_vec() } = bytes;
                Poll::Pending
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
                Poll::Ready(Ok(0)) => return Poll::Ready(Err(Error::new(ErrorKind::WriteZero, "fracture: Write zero"))),
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

pub trait AsyncSeekExt: AsyncSeek {
    fn seek(&mut self, pos: SeekFrom) -> SeekFuture<'_, Self>
    where Self: Unpin + Sized {
        SeekFuture {
            seeker: self,
            pos
        }
    }
}

impl<S: AsyncSeek + ?Sized> AsyncSeekExt for S {}

pub struct SeekFuture<'a, S: ?Sized> {
    seeker: &'a mut S,
    pos: SeekFrom
}

impl<S: AsyncSeek + Unpin + ?Sized> Future for SeekFuture<'_, S> {
    type Output = Result<u64>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match Pin::new(&mut *self.seeker).start_seek(self.pos) {
            Ok(_) => {},
            Err(e) => return Poll::Ready(Err(e)),
        }
        Pin::new(&mut *self.seeker).poll_complete(cx)
    }
}

#[pin_project]
pub struct BufReader<R> {
    #[pin]
    inner: R,
    buf: Box<[u8]>,
    pos: usize,
    cap: usize,
}

impl<R: AsyncRead> BufReader<R> {
    pub fn new(inner: R) -> Self {
        Self::with_capacity(8 * 1024, inner)
    }

    pub fn with_capacity(cap: usize, inner: R) -> Self {
        Self {
            inner,
            buf: vec![0; cap].into_boxed_slice(),
            pos: 0,
            cap: 0,
        }
    }

    pub fn buffer(&self) -> &[u8] {
        &self.buf[self.pos..self.cap]
    }
}

impl<R: AsyncRead> AsyncRead for BufReader<R> {
    fn poll_read(self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &mut ReadBuf<'_>) -> Poll<Result<()>> {
        let this = self.project();

        if *this.pos < *this.cap {
            let len = std::cmp::min(buf.remaining(), *this.cap - *this.pos);
            buf.put_slice(&this.buf[*this.pos..*this.pos + len]);
            *this.pos += len;
            return Poll::Ready(Ok(()));
        }

        if buf.remaining() >= this.buf.len() {
            return this.inner.poll_read(cx, buf);
        }

        match this.inner.poll_read(cx, &mut ReadBuf::new(this.buf)) {
            Poll::Ready(Ok(())) => {
                let mut read_buf = ReadBuf::new(this.buf);
                match this.inner.poll_read(cx, &mut read_buf) {
                     Poll::Ready(Ok(())) => {
                        let n = read_buf.filled().len();
                        *this.pos = 0;
                        *this.cap = n;
                        let len = std::cmp::min(buf.remaining(), n);
                        buf.put_slice(&this.buf[0..len]);
                        *this.pos += len;
                        Poll::Ready(Ok(()))
                     }
                     res => res
                }
            }
            Poll::Pending => Poll::Pending,
            Poll::Ready(Err(e)) => Poll::Ready(Err(e))
        }
    }
}

impl<R: AsyncRead> AsyncBufRead for BufReader<R> {
    fn poll_fill_buf(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<&[u8]>> {
        if chaos::should_fail(ChaosOperation::IoBufRead) {
             return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: BufRead failed (chaos)")));
        }

        let this = self.project();

        if *this.pos >= *this.cap {
            *this.pos = 0;
            *this.cap = 0;
            let mut read_buf = ReadBuf::new(this.buf);
            match this.inner.poll_read(cx, &mut read_buf) {
                Poll::Ready(Ok(())) => {
                    *this.cap = read_buf.filled().len();
                }
                Poll::Ready(Err(e)) => return Poll::Ready(Err(e)),
                Poll::Pending => return Poll::Pending,
            }
        }

        Poll::Ready(Ok(&this.buf[*this.pos..*this.cap]))
    }

    fn consume(self: Pin<&mut Self>, amt: usize) {
        let this = self.project();
        *this.pos = std::cmp::min(*this.pos + amt, *this.cap);
    }
}

#[pin_project]
pub struct BufWriter<W> {
    #[pin]
    inner: W,
    buf: Vec<u8>,
    written: usize,
}

impl<W: AsyncWrite> BufWriter<W> {
    pub fn new(inner: W) -> Self {
        Self::with_capacity(8 * 1024, inner)
    }

    pub fn with_capacity(cap: usize, inner: W) -> Self {
        Self {
            inner,
            buf: Vec::with_capacity(cap),
            written: 0,
        }
    }

    fn flush_buf(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>> {
        let mut this = self.project();
        let len = this.buf.len();
        let mut ret = Poll::Ready(Ok(()));

        while *this.written < len {
            match this.inner.as_mut().poll_write(cx, &this.buf[*this.written..]) {
                Poll::Ready(Ok(0)) => {
                    ret = Poll::Ready(Err(Error::new(ErrorKind::WriteZero, "fracture: failed to write the buffered data")));
                    break;
                }
                Poll::Ready(Ok(n)) => *this.written += n,
                Poll::Ready(Err(e)) => {
                    ret = Poll::Ready(Err(e));
                    break;
                }
                Poll::Pending => {
                    ret = Poll::Pending;
                    break;
                }
            }
        }

        if *this.written == len {
            this.buf.clear();
            *this.written = 0;
        }

        ret
    }
}

impl<W: AsyncWrite> AsyncWrite for BufWriter<W> {
    fn poll_write(mut self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &[u8]) -> Poll<Result<usize>> {
        if chaos::should_fail(ChaosOperation::IoBufWrite) {
             return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: BufWrite failed (chaos)")));
        }

        if self.buf.len() + buf.len() > self.buf.capacity() {
            match self.as_mut().flush_buf(cx) {
                Poll::Ready(Ok(())) => {},
                Poll::Ready(Err(e)) => return Poll::Ready(Err(e)),
                Poll::Pending => return Poll::Pending,
            }
        }

        if buf.len() >= self.buf.capacity() {
            self.project().inner.poll_write(cx, buf)
        } else {
            self.project().buf.extend_from_slice(buf);
            Poll::Ready(Ok(buf.len()))
        }
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>> {
        match self.as_mut().flush_buf(cx) {
            Poll::Ready(Ok(())) => self.project().inner.poll_flush(cx),
            other => other
        }
    }

    fn poll_shutdown(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>> {
        match self.as_mut().flush_buf(cx) {
            Poll::Ready(Ok(())) => self.project().inner.poll_shutdown(cx),
            other => other
        }
    }
}

#[pin_project]
pub struct Lines<R> {
    #[pin]
    reader: R,
}

impl<R: AsyncBufRead> Lines<R> {
    pub async fn next_line(&mut self) -> Result<Option<String>> {
        let mut line = String::new();
        match self.reader.read_line(&mut line).await {
            Ok(0) => Ok(None),
            Ok(_) => {
                if line.ends_with('\n') {
                    line.pop();
                    if line.ends_with('\r') {
                        line.pop();
                    }
                }
                Ok(Some(line))
            }
            Err(e) => Err(e),
        }
    }
}

pub struct ReadUntilFuture<'a, R: ?Sized> {
    reader: &'a mut R,
    delimiter: u8,
    buf: &'a mut Vec<u8>,
    read: usize
}

impl<R: AsyncBufRead + Unpin + ?Sized> Future for ReadUntilFuture<'_, R> {
    type Output = Result<usize>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        if chaos::should_fail(ChaosOperation::IoReadUntil) {
             return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: ReadUntil failed (chaos)")));
        }
        
        loop {
            let (done, used) = {
                let available = match Pin::new(&mut *self.reader).poll_fill_buf(cx) {
                    Poll::Ready(Ok(chunk)) => chunk,
                    Poll::Ready(Err(e)) => return Poll::Ready(Err(e)),
                    Poll::Pending => return Poll::Pending,
                };

                if available.is_empty() {
                    return Poll::Ready(Ok(self.read));
                }

                if let Some(i) = available.iter().position(|&b| b == self.delimiter) {
                    self.buf.extend_from_slice(&available[..=i]);
                    (true, i + 1)
                } else {
                    self.buf.extend_from_slice(available);
                    (false, available.len())
                }
            };

            Pin::new(&mut *self.reader).consume(used);
            self.read += used;
            if done {
                return Poll::Ready(Ok(self.read));
            }
        }
    }
}

pub struct ReadLineFuture<'a, R: ?Sized> {
    reader: &'a mut R,
    buf: &'a mut String,
    bytes: Vec<u8>,
    read: usize
}

impl<R: AsyncBufRead + Unpin + ?Sized> Future for ReadLineFuture<'_, R> {
    type Output = Result<usize>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        if chaos::should_fail(ChaosOperation::IoReadLine) {
             return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: ReadLine failed (chaos)")));
        }

        let reader = unsafe { Pin::new_unchecked(&mut *self.reader) };
        let mut read_until = ReadUntilFuture {
            reader: &mut reader,
            delimiter: b'\n',
            buf: &mut self.bytes,
            read: self.read
        };

        match Pin::new(&mut read_until).poll(cx) {
            Poll::Ready(Ok(n)) => {
                self.read = n;
                match String::from_utf8(std::mem::take(&mut self.bytes)) {
                    Ok(s) => {
                        self.buf.push_str(&s);
                        Poll::Ready(Ok(n))
                    }
                    Err(_) => Poll::Ready(Err(Error::new(ErrorKind::InvalidData, "stream did not contain valid UTF-8")))
                }
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
            Poll::Pending => Poll::Pending
        }
    }
}

#[pin_project]
pub struct ChaosReader<R> {
    #[pin]
    inner: R
}

impl<R> ChaosReader<R> {
    pub fn new(inner: R) -> Self {
        Self { inner }
    }
}

impl<R: AsyncRead> AsyncRead for ChaosReader<R> {
    fn poll_read(self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &mut ReadBuf<'_>) -> Poll<Result<()>> {
        if chaos::should_fail(ChaosOperation::IoRead) {
            return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: IoRead failed (chaos wrapper)")));
        }
        
        self.project().inner.poll_read(cx, buf)
    }
}

impl<R: AsyncBufRead> AsyncBufRead for ChaosReader<R> {
    fn poll_fill_buf(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<&[u8]>> {
        if chaos::should_fail(ChaosOperation::IoBufRead) {
             return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: BufRead failed (chaos wrapper)")));
        }
        self.project().inner.poll_fill_buf(cx)
    }

    fn consume(self: Pin<&mut Self>, amt: usize) {
        self.project().inner.consume(amt)
    }
}

#[pin_project]
pub struct ChaosWriter<W> {
    #[pin]
    inner: W
}

impl<W> ChaosWriter<W> {
    pub fn new(inner: W) -> Self {
        Self { inner }
    }
}

impl<W: AsyncWrite> AsyncWrite for ChaosWriter<W> {
    fn poll_write(self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &[u8]) -> Poll<Result<usize>> {
        if chaos::should_fail(ChaosOperation::IoWrite) {
            return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: IoWrite failed (chaos wrapper)")));
        }
        self.project().inner.poll_write(cx, buf)
    }

    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>> {
        self.project().inner.poll_flush(cx)
    }

    fn poll_shutdown(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>> {
        self.project().inner.poll_shutdown(cx)
    }
}

#[pin_project]
pub struct ChaosSeeker<S> {
    #[pin]
    inner: S
}

impl<S> ChaosSeeker<S> {
    pub fn new(inner: S) -> Self {
        Self { inner }
    }
}

impl<S: AsyncSeek> AsyncSeek for ChaosSeeker<S> {
    fn start_seek(self: Pin<&mut Self>, position: SeekFrom) -> Result<()> {
        if chaos::should_fail(ChaosOperation::IoSeek) {
            return Err(Error::new(ErrorKind::Other, "fracture: IoSeek failed (chaos wrapper)"));
        }
        self.project().inner.start_seek(position)
    }

    fn poll_complete(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<u64>> {
        self.project().inner.poll_complete(cx)
    }
}

#[pin_project]
pub struct Chain<T, U> {
    #[pin]
    first: T,
    #[pin]
    second: U,
    done_first: bool,
}

impl<T: AsyncRead, U: AsyncRead> AsyncRead for Chain<T, U> {
    fn poll_read(self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &mut ReadBuf<'_>) -> Poll<Result<()>> {
        if chaos::should_fail(ChaosOperation::IoChain) {
             return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: Chain failed (chaos)")));
        }

        let this = self.project();
        if !*this.done_first {
            match this.first.poll_read(cx, buf) {
                Poll::Ready(Ok(())) if buf.filled().len() == 0 => {
                    *this.done_first = true;
                }
                other => return other,
            }
        }
        if *this.done_first {
            return this.second.poll_read(cx, buf);
        }
        Poll::Ready(Ok(()))
    }
}

#[pin_project]
pub struct Take<R> {
    #[pin]
    inner: R,
    limit: u64,
}

impl<R: AsyncRead> AsyncRead for Take<R> {
    fn poll_read(self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &mut ReadBuf<'_>) -> Poll<Result<()>> {
        if chaos::should_fail(ChaosOperation::IoTake) {
             return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: Take failed (chaos)")));
        }
        
        let this = self.project();
        if *this.limit == 0 {
            return Poll::Ready(Ok(()));
        }

        let max = std::cmp::min(buf.remaining() as u64, *this.limit) as usize;
        let mut temp = vec![0u8; max];
        let mut temp_buf = ReadBuf::new(&mut temp);
        
        match this.inner.poll_read(cx, &mut temp_buf) {
            Poll::Ready(Ok(())) => {
                let n = temp_buf.filled().len();
                *this.limit -= n as u64;
                buf.put_slice(&temp[..n]);
                Poll::Ready(Ok(()))
            }
            other => other
        }
    }
}