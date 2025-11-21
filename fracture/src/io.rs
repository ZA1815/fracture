use std::collections::VecDeque;
use std::io::{self, Result, Error, ErrorKind, SeekFrom};
use std::mem::MaybeUninit;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll, Waker};
use std::future::Future;
use pin_project::pin_project;
use bytes::BufMut;

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

    pub fn uninit(buf: &'a mut [MaybeUninit<u8>]) -> Self {
        let buf = unsafe { &mut *(buf as *mut [MaybeUninit<u8>] as *mut [u8]) };
        Self { buf, filled: 0, initialized: 0 }
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

    pub fn initialized(&self) -> &[u8] {
        &self.buf[..self.initialized]
    }

    pub fn initialized_mut(&mut self) -> &mut [u8] {
        &mut self.buf[..self.initialized]
    }

    pub fn unfilled_mut(&mut self) -> &mut [u8] {
        &mut self.buf[self.filled..self.initialized]
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

    pub fn assume_init(&mut self, n: usize) {
        let new_init = std::cmp::min(self.filled + n, self.buf.len());
        self.initialized = std::cmp::max(self.initialized, new_init);
    }

    pub fn set_filled(&mut self, n: usize) {
        self.filled = std::cmp::min(n, self.capacity());
    }

    pub fn initialize_unfilled(&mut self) -> &mut [u8] {
        let len = self.buf.len();
        self.initialized = len;
        &mut self.buf[self.filled..]
    }

    pub fn initialize_unfilled_to(&mut self, n: usize) -> &mut [u8] {
        let end = std::cmp::min(self.filled + n, self.buf.len());
        self.initialized = std::cmp::max(self.initialized, end);
        &mut self.buf[self.filled..end]
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
    let arc = std::sync::Arc::new(std::sync::Mutex::new(stream));
    let r = arc.clone();
    let w = arc;

    (ReadHalf { inner: r }, WriteHalf { inner: w })
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

    let mut a_to_b_buf = vec![0u8; 8192];
    let mut b_to_a_buf = vec![0u8; 8192];
    let mut a_to_b_total = 0u64;
    let mut b_to_a_total = 0u64;

    std::future::poll_fn(|cx| {
        let mut a_to_b_read_buf = ReadBuf::new(&mut a_to_b_buf);
        match Pin::new(&mut *a).poll_read(cx, &mut a_to_b_read_buf) {
            Poll::Ready(Ok(())) => {
                let n = a_to_b_read_buf.filled().len();
                if n > 0 {
                    match Pin::new(&mut *b).poll_write(cx, a_to_b_read_buf.filled()) {
                        Poll::Ready(Ok(written)) => {
                            a_to_b_total += written as u64;
                        }
                        Poll::Ready(Err(e)) => return Poll::Ready(Err(e)),
                        Poll::Pending => {}
                    }
                } else {
                    return Poll::Ready(Ok((a_to_b_total, b_to_a_total)));
                }
            }
            Poll::Ready(Err(e)) => return Poll::Ready(Err(e)),
            Poll::Pending => {}
        }

        let mut b_to_a_read_buf = ReadBuf::new(&mut b_to_a_buf);
        match Pin::new(&mut *b).poll_read(cx, &mut b_to_a_read_buf) {
            Poll::Ready(Ok(())) => {
                let n = b_to_a_read_buf.filled().len();
                if n > 0 {
                    match Pin::new(&mut *a).poll_write(cx, b_to_a_read_buf.filled()) {
                        Poll::Ready(Ok(written)) => {
                            b_to_a_total += written as u64;
                        }
                        Poll::Ready(Err(e)) => return Poll::Ready(Err(e)),
                        Poll::Pending => {}
                    }
                }
            }
            Poll::Ready(Err(e)) => return Poll::Ready(Err(e)),
            Poll::Pending => {}
        }

        Poll::Pending
    }).await
}

pub trait AsyncReadExt: AsyncRead {
    fn read<'a>(&'a mut self, buf: &'a mut [u8]) -> ReadFuture<'a, Self>
    where Self: Unpin + Sized {
        ReadFuture {
            reader: self,
            buf
        }
    }

    fn read_buf<'a, B>(&'a mut self, buf: &'a mut B) -> ReadBufMutFuture<'a, Self, B>
    where Self: Unpin + Sized, B: BufMut {
        ReadBufMutFuture {
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

    fn read_u8(&mut self) -> ReadU8Future<'_, Self>
    where Self: Unpin + Sized {
        ReadU8Future { reader: self }
    }

    fn read_i8(&mut self) -> ReadI8Future<'_, Self>
    where Self: Unpin + Sized {
        ReadI8Future { reader: self }
    }

    fn read_u16(&mut self) -> ReadU16Future<'_, Self>
    where Self: Unpin + Sized {
        ReadU16Future { reader: self }
    }

    fn read_u16_le(&mut self) -> ReadU16LeFuture<'_, Self>
    where Self: Unpin + Sized {
        ReadU16LeFuture { reader: self }
    }

    fn read_i16(&mut self) -> ReadI16Future<'_, Self>
    where Self: Unpin + Sized {
        ReadI16Future { reader: self }
    }

    fn read_i16_le(&mut self) -> ReadI16LeFuture<'_, Self>
    where Self: Unpin + Sized {
        ReadI16LeFuture { reader: self }
    }

    fn read_u32(&mut self) -> ReadU32Future<'_, Self>
    where Self: Unpin + Sized {
        ReadU32Future { reader: self }
    }

    fn read_u32_le(&mut self) -> ReadU32LeFuture<'_, Self>
    where Self: Unpin + Sized {
        ReadU32LeFuture { reader: self }
    }

    fn read_i32(&mut self) -> ReadI32Future<'_, Self>
    where Self: Unpin + Sized {
        ReadI32Future { reader: self }
    }

    fn read_i32_le(&mut self) -> ReadI32LeFuture<'_, Self>
    where Self: Unpin + Sized {
        ReadI32LeFuture { reader: self }
    }

    fn read_u64(&mut self) -> ReadU64Future<'_, Self>
    where Self: Unpin + Sized {
        ReadU64Future { reader: self }
    }

    fn read_u64_le(&mut self) -> ReadU64LeFuture<'_, Self>
    where Self: Unpin + Sized {
        ReadU64LeFuture { reader: self }
    }

    fn read_i64(&mut self) -> ReadI64Future<'_, Self>
    where Self: Unpin + Sized {
        ReadI64Future { reader: self }
    }

    fn read_i64_le(&mut self) -> ReadI64LeFuture<'_, Self>
    where Self: Unpin + Sized {
        ReadI64LeFuture { reader: self }
    }

    fn read_u128(&mut self) -> ReadU128Future<'_, Self>
    where Self: Unpin + Sized {
        ReadU128Future { reader: self }
    }

    fn read_u128_le(&mut self) -> ReadU128LeFuture<'_, Self>
    where Self: Unpin + Sized {
        ReadU128LeFuture { reader: self }
    }

    fn read_i128(&mut self) -> ReadI128Future<'_, Self>
    where Self: Unpin + Sized {
        ReadI128Future { reader: self }
    }

    fn read_i128_le(&mut self) -> ReadI128LeFuture<'_, Self>
    where Self: Unpin + Sized {
        ReadI128LeFuture { reader: self }
    }

    fn read_f32(&mut self) -> ReadF32Future<'_, Self>
    where Self: Unpin + Sized {
        ReadF32Future { reader: self }
    }

    fn read_f32_le(&mut self) -> ReadF32LeFuture<'_, Self>
    where Self: Unpin + Sized {
        ReadF32LeFuture { reader: self }
    }

    fn read_f64(&mut self) -> ReadF64Future<'_, Self>
    where Self: Unpin + Sized {
        ReadF64Future { reader: self }
    }

    fn read_f64_le(&mut self) -> ReadF64LeFuture<'_, Self>
    where Self: Unpin + Sized {
        ReadF64LeFuture { reader: self }
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

    fn write_u8(&mut self, n: u8) -> WriteU8Future<'_, Self>
    where Self: Unpin + Sized {
        WriteU8Future { writer: self, n }
    }

    fn write_i8(&mut self, n: i8) -> WriteI8Future<'_, Self>
    where Self: Unpin + Sized {
        WriteI8Future { writer: self, n }
    }

    fn write_u16(&mut self, n: u16) -> WriteU16Future<'_, Self>
    where Self: Unpin + Sized {
        WriteU16Future { writer: self, n }
    }

    fn write_u16_le(&mut self, n: u16) -> WriteU16LeFuture<'_, Self>
    where Self: Unpin + Sized {
        WriteU16LeFuture { writer: self, n }
    }

    fn write_i16(&mut self, n: i16) -> WriteI16Future<'_, Self>
    where Self: Unpin + Sized {
        WriteI16Future { writer: self, n }
    }

    fn write_i16_le(&mut self, n: i16) -> WriteI16LeFuture<'_, Self>
    where Self: Unpin + Sized {
        WriteI16LeFuture { writer: self, n }
    }

    fn write_u32(&mut self, n: u32) -> WriteU32Future<'_, Self>
    where Self: Unpin + Sized {
        WriteU32Future { writer: self, n }
    }

    fn write_u32_le(&mut self, n: u32) -> WriteU32LeFuture<'_, Self>
    where Self: Unpin + Sized {
        WriteU32LeFuture { writer: self, n }
    }

    fn write_i32(&mut self, n: i32) -> WriteI32Future<'_, Self>
    where Self: Unpin + Sized {
        WriteI32Future { writer: self, n }
    }

    fn write_i32_le(&mut self, n: i32) -> WriteI32LeFuture<'_, Self>
    where Self: Unpin + Sized {
        WriteI32LeFuture { writer: self, n }
    }

    fn write_u64(&mut self, n: u64) -> WriteU64Future<'_, Self>
    where Self: Unpin + Sized {
        WriteU64Future { writer: self, n }
    }

    fn write_u64_le(&mut self, n: u64) -> WriteU64LeFuture<'_, Self>
    where Self: Unpin + Sized {
        WriteU64LeFuture { writer: self, n }
    }

    fn write_i64(&mut self, n: i64) -> WriteI64Future<'_, Self>
    where Self: Unpin + Sized {
        WriteI64Future { writer: self, n }
    }

    fn write_i64_le(&mut self, n: i64) -> WriteI64LeFuture<'_, Self>
    where Self: Unpin + Sized {
        WriteI64LeFuture { writer: self, n }
    }

    fn write_u128(&mut self, n: u128) -> WriteU128Future<'_, Self>
    where Self: Unpin + Sized {
        WriteU128Future { writer: self, n }
    }

    fn write_u128_le(&mut self, n: u128) -> WriteU128LeFuture<'_, Self>
    where Self: Unpin + Sized {
        WriteU128LeFuture { writer: self, n }
    }

    fn write_i128(&mut self, n: i128) -> WriteI128Future<'_, Self>
    where Self: Unpin + Sized {
        WriteI128Future { writer: self, n }
    }

    fn write_i128_le(&mut self, n: i128) -> WriteI128LeFuture<'_, Self>
    where Self: Unpin + Sized {
        WriteI128LeFuture { writer: self, n }
    }

    fn write_f32(&mut self, n: f32) -> WriteF32Future<'_, Self>
    where Self: Unpin + Sized {
        WriteF32Future { writer: self, n }
    }

    fn write_f32_le(&mut self, n: f32) -> WriteF32LeFuture<'_, Self>
    where Self: Unpin + Sized {
        WriteF32LeFuture { writer: self, n }
    }

    fn write_f64(&mut self, n: f64) -> WriteF64Future<'_, Self>
    where Self: Unpin + Sized {
        WriteF64Future { writer: self, n }
    }

    fn write_f64_le(&mut self, n: f64) -> WriteF64LeFuture<'_, Self>
    where Self: Unpin + Sized {
        WriteF64LeFuture { writer: self, n }
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

#[pin_project::pin_project]
pub struct ReadFuture<'a, R: ?Sized> {
    reader: &'a mut R,
    buf: &'a mut [u8]
}

impl<R: AsyncRead + Unpin + ?Sized> Future for ReadFuture<'_, R> {
    type Output = Result<usize>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.get_mut();
        let mut buf = ReadBuf::new(this.buf);

        match Pin::new(&mut *this.reader).poll_read(cx, &mut buf) {
            Poll::Ready(Ok(())) => Poll::Ready(Ok(buf.filled().len())),
            Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
            Poll::Pending => Poll::Pending
        }
    }
}

#[pin_project::pin_project]
pub struct ReadBufMutFuture<'a, R: ?Sized, B> {
    reader: &'a mut R,
    buf: &'a mut B
}

impl<R, B> Future for ReadBufMutFuture<'_, R, B>
where
    R: AsyncRead + Unpin + ?Sized,
    B: BufMut,
{
    type Output = Result<usize>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.get_mut();

        if !this.buf.has_remaining_mut() {
            return Poll::Ready(Ok(0));
        }

        let dst = this.buf.chunk_mut();
        let dst_len = dst.len();
        let mut temp_buf = vec![0u8; dst_len];
        let mut read_buf = ReadBuf::new(&mut temp_buf);

        match Pin::new(&mut *this.reader).poll_read(cx, &mut read_buf) {
            Poll::Ready(Ok(())) => {
                let n = read_buf.filled().len();
                this.buf.put_slice(read_buf.filled());
                Poll::Ready(Ok(n))
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
            Poll::Pending => Poll::Pending
        }
    }
}

#[pin_project::pin_project]
pub struct ReadExactFuture<'a, R: ?Sized> {
    reader: &'a mut R,
    buf: &'a mut [u8],
    pos: usize
}

impl<R: AsyncRead + Unpin + ?Sized> Future for ReadExactFuture<'_, R> {
    type Output = Result<usize>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.get_mut();

        loop {
            if this.pos >= this.buf.len() {
                return Poll::Ready(Ok(this.pos));
            }

            let mut buf = ReadBuf::new(&mut this.buf[this.pos..]);

            match Pin::new(&mut *this.reader).poll_read(cx, &mut buf) {
                Poll::Ready(Ok(())) => {
                    let n = buf.filled().len();
                    if n == 0 {
                        return Poll::Ready(Err(Error::new(ErrorKind::UnexpectedEof, "fracture: Unexpected EOF")));
                    }

                    this.pos += n;
                }
                Poll::Ready(Err(e)) => return Poll::Ready(Err(e)),
                Poll::Pending => return Poll::Pending
            }
        }
    }
}

pub struct ReadToEndFuture<'a, R: ?Sized> {
    reader: &'a mut R,
    buf: &'a mut Vec<u8>
}

impl<R: AsyncRead + Unpin + ?Sized> Future for ReadToEndFuture<'_, R> {
    type Output = Result<usize>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.get_mut();
        let start_len = this.buf.len();

        loop {
            if this.buf.capacity() == this.buf.len() {
                this.buf.reserve(32);
            }

            let len = this.buf.len();
            let cap = this.buf.capacity();
            this.buf.resize(cap, 0);
            let mut buf = ReadBuf::new(&mut this.buf[len..]);

            match Pin::new(&mut *this.reader).poll_read(cx, &mut buf) {
                Poll::Ready(Ok(())) => {
                    let n = buf.filled().len();
                    this.buf.truncate(len + n);
                    if n == 0 {
                        return Poll::Ready(Ok(this.buf.len() - start_len));
                    }
                }
                Poll::Ready(Err(e)) => {
                    this.buf.truncate(len);
                    return Poll::Ready(Err(e));
                }
                Poll::Pending => {
                    this.buf.truncate(len);
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

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.get_mut();
        Pin::new(&mut *this.writer).poll_write(cx, this.buf)
    }
}

pub struct WriteAllFuture<'a, W: ?Sized> {
    writer: &'a mut W,
    buf: &'a [u8],
    pos: usize
}

impl<W: AsyncWrite + Unpin + ?Sized> Future for WriteAllFuture<'_, W> {
    type Output = Result<()>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.get_mut();
        while this.pos < this.buf.len() {
            match Pin::new(&mut *this.writer).poll_write(cx, &this.buf[this.pos..]) {
                Poll::Ready(Ok(0)) => return Poll::Ready(Err(Error::new(ErrorKind::WriteZero, "fracture: Write zero"))),
                Poll::Ready(Ok(n)) => this.pos += n,
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

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.get_mut();
        match Pin::new(&mut *this.seeker).start_seek(this.pos) {
            Ok(_) => {},
            Err(e) => return Poll::Ready(Err(e)),
        }
        Pin::new(&mut *this.seeker).poll_complete(cx)
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

impl<R: AsyncBufRead + Unpin> Lines<R> {
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
            let this = self.as_mut().get_mut();
            let delimiter = this.delimiter;
            let reader = &mut this.reader;
            let buf = &mut this.buf;
            let read = &mut this.read;
            
            let (done, used) = {
                let available = match Pin::new(&mut **reader).poll_fill_buf(cx) {
                    Poll::Ready(Ok(chunk)) => chunk,
                    Poll::Ready(Err(e)) => return Poll::Ready(Err(e)),
                    Poll::Pending => return Poll::Pending,
                };

                if available.is_empty() {
                    return Poll::Ready(Ok(*read));
                }

                // Optimization: Use memchr later
                if let Some(i) = available.iter().position(|&b| b == delimiter) {
                    let chunk = &available[..=i];
                    buf.extend_from_slice(chunk);
                    (true, i + 1)
                } else {
                    let chunk = available;
                    buf.extend_from_slice(chunk);
                    (false, chunk.len())
                }
            };

            Pin::new(&mut **reader).consume(used);
            *read += used;
            if done {
                return Poll::Ready(Ok(*read));
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

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        if chaos::should_fail(ChaosOperation::IoReadLine) {
             return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: ReadLine failed (chaos)")));
        }

        let this = self.get_mut();
        let mut read_until = ReadUntilFuture {
            reader: &mut *this.reader,
            delimiter: b'\n',
            buf: &mut this.bytes,
            read: this.read
        };

        match Pin::new(&mut read_until).poll(cx) {
            Poll::Ready(Ok(n)) => {
                this.read = n;
                match String::from_utf8(std::mem::take(&mut this.bytes)) {
                    Ok(s) => {
                        this.buf.push_str(&s);
                        Poll::Ready(Ok(n))
                    }
                    Err(_) => Poll::Ready(Err(Error::new(ErrorKind::InvalidData, "fracture: Invalid UTF-8")))
                }
            },
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

macro_rules! impl_read_primitive {
    ($name:ident, $ty:ty, $size:expr, $from_bytes:expr) => {
        pub struct $name<'a, R: ?Sized> {
            reader: &'a mut R,
        }

        impl<R: AsyncRead + Unpin + ?Sized> Future for $name<'_, R> {
            type Output = Result<$ty>;

            fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                let mut buf = [0u8; $size];
                let mut read_buf = ReadBuf::new(&mut buf);

                match Pin::new(&mut *self.reader).poll_read(cx, &mut read_buf) {
                    Poll::Ready(Ok(())) => {
                        if read_buf.filled().len() < $size {
                            return Poll::Ready(Err(Error::new(
                                ErrorKind::UnexpectedEof,
                                "fracture: Unexpected EOF",
                            )));
                        }
                        Poll::Ready(Ok($from_bytes(&buf)))
                    }
                    Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                    Poll::Pending => Poll::Pending,
                }
            }
        }
    };
}

macro_rules! impl_write_primitive {
    ($name:ident, $ty:ty, $to_bytes:expr) => {
        pub struct $name<'a, W: ?Sized> {
            writer: &'a mut W,
            n: $ty,
        }

        impl<W: AsyncWrite + Unpin + ?Sized> Future for $name<'_, W> {
            type Output = Result<()>;

            fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                let buf = $to_bytes(self.n);
                let mut pos = 0;

                while pos < buf.len() {
                    match Pin::new(&mut *self.writer).poll_write(cx, &buf[pos..]) {
                        Poll::Ready(Ok(0)) => {
                            return Poll::Ready(Err(Error::new(
                                ErrorKind::WriteZero,
                                "fracture: Write zero",
                            )))
                        }
                        Poll::Ready(Ok(n)) => pos += n,
                        Poll::Ready(Err(e)) => return Poll::Ready(Err(e)),
                        Poll::Pending => return Poll::Pending,
                    }
                }

                Poll::Ready(Ok(()))
            }
        }
    };
}

impl_read_primitive!(ReadU8Future, u8, 1, |buf: &[u8]| buf[0]);
impl_read_primitive!(ReadI8Future, i8, 1, |buf: &[u8]| buf[0] as i8);

impl_read_primitive!(ReadU16Future, u16, 2, |buf: &[u8]| {
    u16::from_be_bytes([buf[0], buf[1]])
});
impl_read_primitive!(ReadU16LeFuture, u16, 2, |buf: &[u8]| {
    u16::from_le_bytes([buf[0], buf[1]])
});

impl_read_primitive!(ReadI16Future, i16, 2, |buf: &[u8]| {
    i16::from_be_bytes([buf[0], buf[1]])
});
impl_read_primitive!(ReadI16LeFuture, i16, 2, |buf: &[u8]| {
    i16::from_le_bytes([buf[0], buf[1]])
});

impl_read_primitive!(ReadU32Future, u32, 4, |buf: &[u8]| {
    u32::from_be_bytes([buf[0], buf[1], buf[2], buf[3]])
});
impl_read_primitive!(ReadU32LeFuture, u32, 4, |buf: &[u8]| {
    u32::from_le_bytes([buf[0], buf[1], buf[2], buf[3]])
});

impl_read_primitive!(ReadI32Future, i32, 4, |buf: &[u8]| {
    i32::from_be_bytes([buf[0], buf[1], buf[2], buf[3]])
});
impl_read_primitive!(ReadI32LeFuture, i32, 4, |buf: &[u8]| {
    i32::from_le_bytes([buf[0], buf[1], buf[2], buf[3]])
});

impl_read_primitive!(ReadU64Future, u64, 8, |buf: &[u8]| {
    u64::from_be_bytes([buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7]])
});
impl_read_primitive!(ReadU64LeFuture, u64, 8, |buf: &[u8]| {
    u64::from_le_bytes([buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7]])
});

impl_read_primitive!(ReadI64Future, i64, 8, |buf: &[u8]| {
    i64::from_be_bytes([buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7]])
});
impl_read_primitive!(ReadI64LeFuture, i64, 8, |buf: &[u8]| {
    i64::from_le_bytes([buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7]])
});

impl_read_primitive!(ReadU128Future, u128, 16, |buf: &[u8]| {
    u128::from_be_bytes([
        buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7],
        buf[8], buf[9], buf[10], buf[11], buf[12], buf[13], buf[14], buf[15],
    ])
});
impl_read_primitive!(ReadU128LeFuture, u128, 16, |buf: &[u8]| {
    u128::from_le_bytes([
        buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7],
        buf[8], buf[9], buf[10], buf[11], buf[12], buf[13], buf[14], buf[15],
    ])
});

impl_read_primitive!(ReadI128Future, i128, 16, |buf: &[u8]| {
    i128::from_be_bytes([
        buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7],
        buf[8], buf[9], buf[10], buf[11], buf[12], buf[13], buf[14], buf[15],
    ])
});
impl_read_primitive!(ReadI128LeFuture, i128, 16, |buf: &[u8]| {
    i128::from_le_bytes([
        buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7],
        buf[8], buf[9], buf[10], buf[11], buf[12], buf[13], buf[14], buf[15],
    ])
});

impl_read_primitive!(ReadF32Future, f32, 4, |buf: &[u8]| {
    f32::from_be_bytes([buf[0], buf[1], buf[2], buf[3]])
});
impl_read_primitive!(ReadF32LeFuture, f32, 4, |buf: &[u8]| {
    f32::from_le_bytes([buf[0], buf[1], buf[2], buf[3]])
});

impl_read_primitive!(ReadF64Future, f64, 8, |buf: &[u8]| {
    f64::from_be_bytes([buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7]])
});
impl_read_primitive!(ReadF64LeFuture, f64, 8, |buf: &[u8]| {
    f64::from_le_bytes([buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7]])
});

impl_write_primitive!(WriteU8Future, u8, |n: u8| [n]);
impl_write_primitive!(WriteI8Future, i8, |n: i8| [n as u8]);

impl_write_primitive!(WriteU16Future, u16, |n: u16| n.to_be_bytes());
impl_write_primitive!(WriteU16LeFuture, u16, |n: u16| n.to_le_bytes());

impl_write_primitive!(WriteI16Future, i16, |n: i16| n.to_be_bytes());
impl_write_primitive!(WriteI16LeFuture, i16, |n: i16| n.to_le_bytes());

impl_write_primitive!(WriteU32Future, u32, |n: u32| n.to_be_bytes());
impl_write_primitive!(WriteU32LeFuture, u32, |n: u32| n.to_le_bytes());

impl_write_primitive!(WriteI32Future, i32, |n: i32| n.to_be_bytes());
impl_write_primitive!(WriteI32LeFuture, i32, |n: i32| n.to_le_bytes());

impl_write_primitive!(WriteU64Future, u64, |n: u64| n.to_be_bytes());
impl_write_primitive!(WriteU64LeFuture, u64, |n: u64| n.to_le_bytes());

impl_write_primitive!(WriteI64Future, i64, |n: i64| n.to_be_bytes());
impl_write_primitive!(WriteI64LeFuture, i64, |n: i64| n.to_le_bytes());

impl_write_primitive!(WriteU128Future, u128, |n: u128| n.to_be_bytes());
impl_write_primitive!(WriteU128LeFuture, u128, |n: u128| n.to_le_bytes());

impl_write_primitive!(WriteI128Future, i128, |n: i128| n.to_be_bytes());
impl_write_primitive!(WriteI128LeFuture, i128, |n: i128| n.to_le_bytes());

impl_write_primitive!(WriteF32Future, f32, |n: f32| n.to_be_bytes());
impl_write_primitive!(WriteF32LeFuture, f32, |n: f32| n.to_le_bytes());

impl_write_primitive!(WriteF64Future, f64, |n: f64| n.to_be_bytes());
impl_write_primitive!(WriteF64LeFuture, f64, |n: f64| n.to_le_bytes());

pub struct Stdout {
    _private: (),
}

pub struct Stderr {
    _private: (),
}

pub struct Stdin {
    _private: (),
}

pub fn stdout() -> Stdout {
    Stdout { _private: () }
}

pub fn stderr() -> Stderr {
    Stderr { _private: () }
}

pub fn stdin() -> Stdin {
    Stdin { _private: () }
}

impl AsyncWrite for Stdout {
    fn poll_write(self: Pin<&mut Self>, _cx: &mut Context<'_>, buf: &[u8]) -> Poll<Result<usize>> {
        Poll::Ready(Ok(buf.len()))
    }

    fn poll_flush(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<()>> {
        Poll::Ready(Ok(()))
    }

    fn poll_shutdown(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<()>> {
        Poll::Ready(Ok(()))
    }
}

impl AsyncWrite for Stderr {
    fn poll_write(self: Pin<&mut Self>, _cx: &mut Context<'_>, buf: &[u8]) -> Poll<Result<usize>> {
        Poll::Ready(Ok(buf.len()))
    }

    fn poll_flush(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<()>> {
        Poll::Ready(Ok(()))
    }

    fn poll_shutdown(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<()>> {
        Poll::Ready(Ok(()))
    }
}

impl AsyncRead for Stdin {
    fn poll_read(self: Pin<&mut Self>, _cx: &mut Context<'_>, _buf: &mut ReadBuf<'_>) -> Poll<Result<()>> {
        Poll::Ready(Ok(()))
    }
}

pub struct Sink {
    _private: (),
}

pub fn sink() -> Sink {
    Sink { _private: () }
}

impl AsyncWrite for Sink {
    fn poll_write(self: Pin<&mut Self>, _cx: &mut Context<'_>, buf: &[u8]) -> Poll<Result<usize>> {
        Poll::Ready(Ok(buf.len()))
    }

    fn poll_flush(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<()>> {
        Poll::Ready(Ok(()))
    }

    fn poll_shutdown(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<()>> {
        Poll::Ready(Ok(()))
    }
}

pub struct Empty {
    _private: (),
}

pub fn empty() -> Empty {
    Empty { _private: () }
}

impl AsyncRead for Empty {
    fn poll_read(self: Pin<&mut Self>, _cx: &mut Context<'_>, _buf: &mut ReadBuf<'_>) -> Poll<Result<()>> {
        Poll::Ready(Ok(()))
    }
}

pub struct Repeat {
    byte: u8,
}

pub fn repeat(byte: u8) -> Repeat {
    Repeat { byte }
}

impl AsyncRead for Repeat {
    fn poll_read(self: Pin<&mut Self>, _cx: &mut Context<'_>, buf: &mut ReadBuf<'_>) -> Poll<Result<()>> {
        let remaining = buf.remaining();
        let to_fill = vec![self.byte; remaining];
        buf.put_slice(&to_fill);
        Poll::Ready(Ok(()))
    }
}

pub struct DuplexStream {
    read_half: Arc<std::sync::Mutex<VecDeque<u8>>>,
    write_half: Arc<std::sync::Mutex<VecDeque<u8>>>,
    read_waker: Arc<std::sync::Mutex<Option<Waker>>>,
    write_waker: Arc<std::sync::Mutex<Option<Waker>>>,
    max_buf_size: usize,
}

pub fn duplex(max_buf_size: usize) -> (DuplexStream, DuplexStream) {
    let buf1 = Arc::new(std::sync::Mutex::new(VecDeque::new()));
    let buf2 = Arc::new(std::sync::Mutex::new(VecDeque::new()));
    let waker1 = Arc::new(std::sync::Mutex::new(None));
    let waker2 = Arc::new(std::sync::Mutex::new(None));

    let stream1 = DuplexStream {
        read_half: buf1.clone(),
        write_half: buf2.clone(),
        read_waker: waker1.clone(),
        write_waker: waker2.clone(),
        max_buf_size,
    };

    let stream2 = DuplexStream {
        read_half: buf2,
        write_half: buf1,
        read_waker: waker2,
        write_waker: waker1,
        max_buf_size,
    };

    (stream1, stream2)
}

impl AsyncRead for DuplexStream {
    fn poll_read(self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &mut ReadBuf<'_>) -> Poll<Result<()>> {
        let mut read_buf = self.read_half.lock().unwrap();

        if read_buf.is_empty() {
            *self.read_waker.lock().unwrap() = Some(cx.waker().clone());
            return Poll::Pending;
        }

        let to_read = std::cmp::min(buf.remaining(), read_buf.len());
        for _ in 0..to_read {
            if let Some(byte) = read_buf.pop_front() {
                buf.put_slice(&[byte]);
            }
        }

        if let Some(waker) = self.write_waker.lock().unwrap().take() {
            waker.wake();
        }

        Poll::Ready(Ok(()))
    }
}

impl AsyncWrite for DuplexStream {
    fn poll_write(self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &[u8]) -> Poll<Result<usize>> {
        let mut write_buf = self.write_half.lock().unwrap();

        if write_buf.len() >= self.max_buf_size {
            *self.write_waker.lock().unwrap() = Some(cx.waker().clone());
            return Poll::Pending;
        }

        let to_write = std::cmp::min(buf.len(), self.max_buf_size - write_buf.len());
        write_buf.extend(&buf[..to_write]);

        if let Some(waker) = self.read_waker.lock().unwrap().take() {
            waker.wake();
        }

        Poll::Ready(Ok(to_write))
    }

    fn poll_flush(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<()>> {
        Poll::Ready(Ok(()))
    }

    fn poll_shutdown(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<()>> {
        Poll::Ready(Ok(()))
    }
}