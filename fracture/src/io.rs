use std::io::{self, Error, ErrorKind, Result, SeekFrom};
use std::pin::Pin;
use std::task::{Context, Poll};
pub use tokio::io::{AsyncBufRead, AsyncBufReadExt, AsyncRead, AsyncReadExt, AsyncSeek, AsyncWrite, AsyncWriteExt, ReadBuf};
use pin_project::pin_project;

use crate::chaos::{self, ChaosOperation};

pub struct ChaosReader<R> {
    inner: R,
    chaos_state: ReaderChaosState
}

struct ReaderChaosState {
    bytes_until_error: Option<usize>,
    corrupt_next_read: bool,
    partial_read_size: Option<usize>,
    eof_early: bool,
    read_count: usize
}

impl <R: AsyncRead> ChaosReader<R> {
    pub fn new(inner: R) -> Self {
        Self {
            inner,
            chaos_state: ReaderChaosState {
                bytes_until_error: if chaos::should_fail(ChaosOperation::IoRead) {
                    Some(rand::random::<usize>() % 1024)
                }
                else {
                    None
                },
                corrupt_next_read: chaos::should_fail(ChaosOperation::IoCorruption),
                partial_read_size: if chaos::should_fail(ChaosOperation::IoPartialRead) {
                    Some(rand::random::<usize>() % 64 + 1)
                }
                else {
                    None
                },
                eof_early: chaos::should_fail(ChaosOperation::IoEof),
                read_count: 0
            }
        }
    }
}

impl<R: AsyncRead + Unpin> AsyncRead for ChaosReader<R> {
    fn poll_read(
            mut self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &mut ReadBuf<'_>,
        ) -> Poll<io::Result<()>> {
        if chaos::should_fail(ChaosOperation::IoRead) {
            return Poll::Ready(Err(Error::new(ErrorKind::BrokenPipe, "fracture: Read failed (chaos)")));
        }

        if self.chaos_state.eof_early && self.chaos_state.read_count > 0 {
            return Poll::Ready(Ok(()))
        }

        if let Some(max_size) = self.chaos_state.partial_read_size {
            if buf.remaining() > max_size {
                let mut limited_buf = buf.take(max_size);
                let result = Pin::new(&mut self.inner).poll_read(cx, &mut limited_buf);
                let filled_len = limited_buf.filled().len();
                buf.advance(filled_len);
                return result;
            }
        }

        self.chaos_state.read_count += 1;

        if let Some(ref mut bytes_left) = self.chaos_state.bytes_until_error {
            if *bytes_left == 0 {
                return Poll::Ready(Err(Error::new(ErrorKind::UnexpectedEof, "fracture: Read error after N bytes")));
            }
            *bytes_left = bytes_left.saturating_sub(buf.remaining());
        }

        if self.chaos_state.corrupt_next_read {
            let filled_before = buf.filled().len();
            let result = Pin::new(&mut self.inner).poll_read(cx, buf);

            if let Poll::Ready(Ok(())) = result {
                let filled_after = buf.filled().len();
                if filled_after > filled_before {
                    let filled_mut = buf.filled_mut();
                    for i in filled_before..filled_after.min(filled_before + 4) {
                        filled_mut[i] ^= 0xAA;
                    }
                }
            }

            self.chaos_state.corrupt_next_read = false;
            return result;
        }

        Pin::new(&mut self.inner).poll_read(cx, buf)
    }
}

pub struct ChaosWriter<W> {
    inner: W,
    chaos_state: WriterChaosState
}

struct WriterChaosState {
    bytes_until_error: Option<usize>,
    partial_write_size: Option<usize>,
    flush_fails: bool,
    write_count: usize
}

impl<W: AsyncWrite> ChaosWriter<W> {
    pub fn new(inner: W) -> Self {
        Self {
            inner,
            chaos_state: WriterChaosState {
                bytes_until_error: if chaos::should_fail(ChaosOperation::IoWrite) {
                    Some(rand::random::<usize>() % 1024)
                }
                else {
                    None
                },
                partial_write_size: if chaos::should_fail(ChaosOperation::IoPartialWrite) {
                    Some(rand::random::<usize>() % 64 + 1)
                }
                else {
                    None
                },
                flush_fails: chaos::should_fail(ChaosOperation::IoFlush),
                write_count: 0
            }
        }
    }
}

impl<W: AsyncWrite + Unpin> AsyncWrite for ChaosWriter<W> {
    fn poll_write(
            mut self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &[u8],
        ) -> Poll<std::result::Result<usize, io::Error>> {
        if chaos::should_fail(ChaosOperation::IoWrite) {
            return Poll::Ready(Err(Error::new(ErrorKind::BrokenPipe, "fracture: Write failed (chaos)")));
        }

        if let Some(max_size) = self.chaos_state.partial_write_size {
            if buf.len() > max_size {
                return Pin::new(&mut self.inner).poll_write(cx, &buf[..max_size]);
            }
        }

        self.chaos_state.write_count += 1;

        if let Some(ref mut bytes_left) = self.chaos_state.bytes_until_error {
            if *bytes_left == 0 {
                return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: Write error after N bytes (chaos)")));
            }
            *bytes_left = bytes_left.saturating_sub(buf.len());
        }

        Pin::new(&mut self.inner).poll_write(cx, buf)
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::result::Result<(), io::Error>> {
        if self.chaos_state.flush_fails {
            return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: Flush failed (chaos)")));
        }

        Pin::new(&mut self.inner).poll_flush(cx)
    }

    fn poll_shutdown(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::result::Result<(), io::Error>> {
        Pin::new(&mut self.inner).poll_shutdown(cx)
    }
}

#[pin_project]
pub struct BufReader<R> {
    #[pin]
    inner: tokio::io::BufReader<R>,
    chaos_state: BufReaderChaosState
}

struct BufReaderChaosState {
    skip_lines: bool,
    duplicate_lines: bool
}

impl<R: AsyncRead> BufReader<R> {
    pub fn new(inner: R) -> Self {
        Self {
            inner: tokio::io::BufReader::new(inner),
            chaos_state: BufReaderChaosState {
                skip_lines: chaos::should_fail(ChaosOperation::IoBufRead),
                duplicate_lines: false
            }
        }
    }

    pub fn with_capacity(capacity: usize, inner: R) -> Self {
        Self {
            inner: tokio::io::BufReader::with_capacity(capacity, inner),
            chaos_state: BufReaderChaosState {
                skip_lines: chaos::should_fail(ChaosOperation::IoBufRead),
                duplicate_lines: false
            }
        }
    }
}

impl<R: AsyncRead + Unpin> AsyncRead for BufReader<R> {
    fn poll_read(
            mut self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &mut ReadBuf<'_>,
        ) -> Poll<io::Result<()>> {
        Pin::new(&mut self.inner).poll_read(cx, buf)
    }
}

impl<R: AsyncRead + Unpin> AsyncBufRead for BufReader<R> {
    fn poll_fill_buf(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<io::Result<&[u8]>> {
        let this = self.project();
        this.inner.poll_fill_buf(cx)
    }

    fn consume(mut self: Pin<&mut Self>, amt: usize) {
        Pin::new(&mut self.inner).consume(amt);
    }
}

pub struct BufWriter<W> {
    inner: tokio::io::BufWriter<W>,
    chaos_state: BufWriterChaosState
}

struct BufWriterChaosState {
    auto_flush_fails: bool,
    buffer_overflow: bool
}

impl<W: AsyncWrite> BufWriter<W> {
    pub fn new(inner: W) -> Self {
        Self {
            inner: tokio::io::BufWriter::new(inner),
            chaos_state: BufWriterChaosState {
                auto_flush_fails: chaos::should_fail(ChaosOperation::IoFlush),
                buffer_overflow: chaos::should_fail(ChaosOperation::IoBufWrite)
            }
        }
    }

    pub fn with_capacity(capacity: usize, inner: W) -> Self {
        Self {
            inner: tokio::io::BufWriter::with_capacity(capacity, inner),
            chaos_state: BufWriterChaosState {
                auto_flush_fails: chaos::should_fail(ChaosOperation::IoFlush),
                buffer_overflow: chaos::should_fail(ChaosOperation::IoBufWrite)
            }
        }
    }
}

impl<W: AsyncWrite + Unpin> AsyncWrite for BufWriter<W> {
    fn poll_write(
            mut self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &[u8],
        ) -> Poll<std::result::Result<usize, io::Error>> {
        if self.chaos_state.buffer_overflow && buf.len() > 1024 {
            return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: Buffer overflow (chaos)")));
        }

        Pin::new(&mut self.inner).poll_write(cx, buf)
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::result::Result<(), io::Error>> {
        if self.chaos_state.auto_flush_fails {
            return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: Auto-flush failed (chaos)")));
        }

        Pin::new(&mut self.inner).poll_flush(cx)
    }

    fn poll_shutdown(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::result::Result<(), io::Error>> {
        Pin::new(&mut self.inner).poll_shutdown(cx)
    }
}

pub async fn copy<'a, R, W>(reader: &'a mut R, writer: &'a mut W) -> Result<u64>
where R: AsyncRead + Unpin + ?Sized, W: AsyncWrite + Unpin + ?Sized {
    if chaos::should_fail(ChaosOperation::IoCopy) {
        return Err(Error::new(ErrorKind::Other, "fracture: Copy failed (chaos)"));
    }

    if chaos::should_fail(ChaosOperation::IoCorruption) {
        // Placeholder
    }

    tokio::io::copy(reader, writer).await
}

pub async fn copy_bidirectional<A, B>(a: &mut A, b: &mut B) -> Result<(u64, u64)>
where A: AsyncRead + AsyncWrite + Unpin + ?Sized, B: AsyncRead + AsyncWrite + Unpin + ?Sized {
    if chaos::should_fail(ChaosOperation::IoCopyBidirectional) {
        return Err(Error::new(ErrorKind::Other, "fracture: Bidirectional copy failed (chaos)"));
    }

    tokio::io::copy_bidirectional(a, b).await
}

pub async fn copy_buf<'a, R, W>(reader: &'a mut R, writer: &'a mut W) -> Result<u64>
where R: AsyncBufRead + Unpin + ?Sized, W: AsyncWrite + Unpin + ?Sized {
    tokio::io::copy_buf(reader, writer).await
}

pub fn split<T>(stream: T) -> (ReadHalf<T>, WriteHalf<T>)
where T: AsyncRead + AsyncWrite {
    let (read_half, write_half) = tokio::io::split(stream);

    (
        ReadHalf { inner: read_half },
        WriteHalf { inner: write_half }
    )
}

pub struct ReadHalf<T> {
    inner: tokio::io::ReadHalf<T>
}

pub struct WriteHalf<T> {
    inner: tokio::io::WriteHalf<T>
}

impl<T> ReadHalf<T> {
    pub fn unsplit(self, write_half: WriteHalf<T>) -> T
    where T: AsyncRead + AsyncWrite + Unpin {
        self.inner.unsplit(write_half.inner)
    }
}

impl<T> AsyncRead for ReadHalf<T>
where T: AsyncRead + AsyncWrite + Unpin {
    fn poll_read(
            mut self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &mut ReadBuf<'_>,
        ) -> Poll<io::Result<()>> {
        if chaos::should_fail(ChaosOperation::IoSplitRead) {
            return Poll::Ready(Err(Error::new(ErrorKind::BrokenPipe, "fracture: Split read failed (chaos)")));
        }

        Pin::new(&mut self.inner).poll_read(cx, buf)
    }
}

impl<T> AsyncWrite for WriteHalf<T>
where T: AsyncRead + AsyncWrite + Unpin {
    fn poll_write(
            mut self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &[u8],
        ) -> Poll<std::result::Result<usize, io::Error>> {
        if chaos::should_fail(ChaosOperation::IoSplitWrite) {
            return Poll::Ready(Err(Error::new(ErrorKind::BrokenPipe, "fracture: Split write failed (chaos)")));
        }

        Pin::new(&mut self.inner).poll_write(cx, buf)
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::result::Result<(), io::Error>> {
        Pin::new(&mut self.inner).poll_flush(cx)
    }

    fn poll_shutdown(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::result::Result<(), io::Error>> {
        Pin::new(&mut self.inner).poll_shutdown(cx)
    }
}

pub struct ChaosReadBuf<'a> {
    inner: &'a mut ReadBuf<'a>,
    chaos_state: ReadBufChaosState
}

struct ReadBufChaosState {
    corrupt_next: bool,
    limit_capacity: Option<usize>
}

impl<'a> ChaosReadBuf<'a> {
    pub fn new(inner: &'a mut ReadBuf<'a>) -> Self {
        Self {
            chaos_state: ReadBufChaosState {
                corrupt_next: chaos::should_fail(ChaosOperation::IoCorruption),
                limit_capacity: if chaos::should_fail(ChaosOperation::IoPartialRead) {
                    Some(inner.capacity() / 2)
                }
                else {
                    None
                }
            },
            inner
        }
    }

    pub fn capacity(&self) -> usize {
        self.chaos_state.limit_capacity.unwrap_or(self.inner.capacity())
    }

    pub fn filled(&self) -> &[u8] {
        self.inner.filled()
    }

    pub fn filled_mut(&mut self) -> &mut [u8] {
        let filled = self.inner.filled_mut();
        if self.chaos_state.corrupt_next {
            for byte in filled.iter_mut().take(4) {
                *byte ^= 0xFF;
            }
            self.chaos_state.corrupt_next = false;
        }

        filled
    }

    pub fn initialize_unfilled(&mut self) -> &mut [u8] {
        self.inner.initialize_unfilled()
    }

    pub fn initialize_unfilled_to(&mut self, n: usize) -> &mut [u8] {
        let n = if let Some(limit) = self.chaos_state.limit_capacity {
            n.min(limit)
        }
        else {
            n
        };

        self.inner.initialize_unfilled_to(n)
    }

    pub fn remaining(&self) -> usize {
        if let Some(limit) = self.chaos_state.limit_capacity {
            self.inner.remaining().min(limit - self.inner.filled().len())
        }
        else {
            self.inner.remaining()
        }
    }

    pub fn advance(&mut self, n: usize) {
        self.inner.advance(n);
    }

    pub fn set_filled(&mut self, n: usize) {
        self.inner.set_filled(n);
    }

    pub fn put_slice(&mut self, buf: &[u8]) {
        self.inner.put_slice(buf);
    }
}

pub struct DuplexStream {
    inner: tokio::io::DuplexStream,
    chaos_state: DuplexChaosState
}

#[derive(Clone)]
struct DuplexChaosState {
    drop_writes: bool,
    corrupt_reads: bool
}

impl DuplexStream {
    pub fn new(max_buf_size: usize) -> (Self, Self) {
        let (a, b) = tokio::io::duplex(max_buf_size);

        let chaos_state_a = DuplexChaosState {
            drop_writes: chaos::should_fail(ChaosOperation::IoWrite),
            corrupt_reads: chaos::should_fail(ChaosOperation::IoCorruption)
        };

        let chaos_state_b = chaos_state_a.clone();

        (
            Self {
                inner: a,
                chaos_state: chaos_state_a
            },
            Self {
                inner: b,
                chaos_state: chaos_state_b
            }
        )
    }
}

impl AsyncRead for DuplexStream {
    fn poll_read(
            mut self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &mut ReadBuf<'_>,
        ) -> Poll<io::Result<()>> {
        if chaos::should_fail(ChaosOperation::IoRead) {
            return Poll::Ready(Err(Error::new(ErrorKind::BrokenPipe, "fracture: Duplex read failed (chaos)")));
        }

        if self.chaos_state.corrupt_reads {
            let filled_before = buf.filled().len();
            let result = Pin::new(&mut self.inner).poll_read(cx, buf);

            if let Poll::Ready(Ok(())) = result {
                let filled_after = buf.filled().len();
                if filled_after > filled_before {
                    let filled_mut = buf.filled_mut();
                    for i in filled_before..filled_after.min(filled_before + 4) {
                        filled_mut[i] ^= 0xFF;
                    }
                }
            }
            return result;
        }

        Pin::new(&mut self.inner).poll_read(cx, buf)
    }
}

impl AsyncWrite for DuplexStream {
    fn poll_write(
            mut self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &[u8],
        ) -> Poll<std::result::Result<usize, io::Error>> {
        if self.chaos_state.drop_writes {
            return Poll::Ready(Ok(buf.len()));
        }

        if chaos::should_fail(ChaosOperation::IoWrite) {
            return Poll::Ready(Err(Error::new(ErrorKind::BrokenPipe, "fracture: Duplex write failed (chaos)")));
        }

        Pin::new(&mut self.inner).poll_write(cx, buf)
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::result::Result<(), io::Error>> {
        Pin::new(&mut self.inner).poll_flush(cx)
    }

    fn poll_shutdown(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::result::Result<(), io::Error>> {
        Pin::new(&mut self.inner).poll_shutdown(cx)
    }
}

pub struct Lines<R>
where R: AsyncRead + Unpin {
    inner: tokio::io::Lines<tokio::io::BufReader<R>>,
    skip_rate: f32
}

impl<R: AsyncRead> Lines<R>
where R: AsyncRead + Unpin {
    pub fn new(reader: R) -> Self {
        Self {
            inner: tokio::io::BufReader::new(reader).lines(),
            skip_rate: if chaos::should_fail(ChaosOperation::IoReadLine) { 0.1 } else { 0.0 }
        }
    }

    pub async fn next_line(&mut self) -> Result<Option<String>> {
        if self.skip_rate > 0.0 &&  rand::random::<f32>() < self.skip_rate {
            let _ = self.inner.next_line().await?;
        }

        self.inner.next_line().await
    }
}

pub struct Take<R> {
    inner: tokio::io::Take<R>,
    chaos_limit: Option<u64>
}

impl<R: AsyncRead> Take<R> {
    pub fn new(inner: R, limit: u64) -> Self {
        let chaos_limit = if chaos::should_fail(ChaosOperation::IoTake) {
            Some(limit / 2)
        }
        else {
            None
        };

        Self {
            inner: tokio::io::AsyncReadExt::take(inner, limit),
            chaos_limit
        }
    }

    pub fn limit(&self) -> u64 {
        if let Some(limit) = self.chaos_limit {
            return limit;
        }

        self.inner.limit()
    }

    pub fn set_limit(&mut self, limit: u64) {
        self.inner.set_limit(limit);
    }
}

impl<R: AsyncRead + Unpin> AsyncRead for Take<R> {
    fn poll_read(
            mut self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &mut ReadBuf<'_>,
        ) -> Poll<io::Result<()>> {
        Pin::new(&mut self.inner).poll_read(cx, buf)
    }
}

pub struct Chain<T, U> {
    inner: tokio::io::Chain<T, U>
}

impl<T, U> Chain<T, U>
where T: AsyncRead, U: AsyncRead {
    pub fn new(first: T, second: U) -> Self {
        Self {
            inner: tokio::io::AsyncReadExt::chain(first, second)
        }
    }
}

impl<T: AsyncRead + Unpin, U: AsyncRead + Unpin> AsyncRead for Chain<T, U> {
    fn poll_read(
            mut self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &mut ReadBuf<'_>,
        ) -> Poll<io::Result<()>> {
        if chaos::should_fail(ChaosOperation::IoChain) {
            return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: Chain read failed (chaos)")));
        }

        Pin::new(&mut self.inner).poll_read(cx, buf)
    }
}

pub struct ChaosSeeker<S> {
    inner: S,
    chaos_state: SeekerChaosState
}

struct SeekerChaosState {
    seek_offset_error: i64,
    seek_fails: bool
}

impl<S: AsyncSeek> ChaosSeeker<S> {
    pub fn new(inner: S) -> Self {
        Self {
            inner,
            chaos_state: SeekerChaosState {
                seek_offset_error: if chaos::should_fail(ChaosOperation::IoSeek) {
                    rand::random::<i64>() % 100 - 50
                }
                else {
                    0
                },
                seek_fails: chaos::should_fail(ChaosOperation::IoSeek)
            }
        }
    }
}

impl<S: AsyncSeek + Unpin> AsyncSeek for ChaosSeeker<S> {
    fn start_seek(mut self: Pin<&mut Self>, position: SeekFrom) -> io::Result<()> {
        if self.chaos_state.seek_fails {
            return Err(Error::new(ErrorKind::Other, "fracture: Seek failed (chaos)"));
        }

        let modified_position = match position {
            SeekFrom::Start(pos) => {
                let new_pos = (pos as i64 + self.chaos_state.seek_offset_error).max(0) as u64;
                SeekFrom::Start(new_pos)
            }
            SeekFrom::Current(offset) => {
                SeekFrom::Current(offset + self.chaos_state.seek_offset_error)
            }
            SeekFrom::End(offset) => {
                SeekFrom::End(offset + self.chaos_state.seek_offset_error)
            }
        };
        Pin::new(&mut self.inner).start_seek(modified_position)
    }

    fn poll_complete(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<io::Result<u64>> {
        Pin::new(&mut self.inner).poll_complete(cx)
    }
}

pub fn empty() -> Empty {
    Empty
}

pub struct Empty;

impl AsyncRead for Empty {
    fn poll_read(
            self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &mut ReadBuf<'_>,
        ) -> Poll<io::Result<()>> {
        Poll::Ready(Ok(()))
    }
}

pub fn repeat(byte: u8) -> Repeat {
    Repeat { byte }
}

pub struct Repeat {
    byte: u8
}

impl AsyncRead for Repeat {
    fn poll_read(
            self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &mut ReadBuf<'_>,
        ) -> Poll<io::Result<()>> {
        if chaos::should_fail(ChaosOperation::IoRead) {
            return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: Repeat read failed (chaos)")));
        }

        for byte in buf.initialize_unfilled() {
            *byte = self.byte;
        }
        let len = buf.remaining();
        buf.advance(len);
        Poll::Ready(Ok(()))
    }
}

pub fn sink() -> Sink {
    Sink
}

pub struct Sink;

impl AsyncWrite for Sink {
    fn poll_write(
            self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            buf: &[u8],
        ) -> Poll<std::result::Result<usize, io::Error>> {
        if chaos::should_fail(ChaosOperation::IoWrite) {
            return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: Sink write failed (chaos)")));
        }

        Poll::Ready(Ok(buf.len()))
    }

    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::result::Result<(), io::Error>> {
        Poll::Ready(Ok(()))
    }

    fn poll_shutdown(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::result::Result<(), io::Error>> {
        Poll::Ready(Ok(()))
    }
}