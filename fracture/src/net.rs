use core::fmt;
use std::collections::{HashMap, VecDeque};
use std::future::Future;
use std::io::{Error, ErrorKind, Result};
use std::net::{SocketAddr, ToSocketAddrs};
use std::pin::Pin;
#[cfg(unix)]
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::task::{Context, Poll, Waker};
use std::time::Duration;

use bytes::{Buf, BufMut, Bytes, BytesMut};
use futures::TryFutureExt;
use rand::Rng;

use crate::chaos::{self, ChaosOperation};
use crate::io::{AsyncRead, AsyncWrite, ReadBuf};
use crate::runtime::Handle;
use crate::time::sleep;

const DEFAULT_TCP_WINDOW_SIZE: usize = 65535;
const MAX_UDP_PACKET_SIZE: usize = 65507;

pub(crate) struct NetworkState {
    listeners: HashMap<SocketAddr, Sender<TcpConnection>>,
    udp_sockets: HashMap<SocketAddr, Sender<(Bytes, SocketAddr)>>,
    #[cfg(unix)]
    unix_listeners: HashMap<PathBuf, Sender<unix::UnixConnection>>,
    #[cfg(unix)]
    unix_dgram: HashMap<PathBuf, Sender<(Bytes, PathBuf)>>
}

impl NetworkState {
    pub fn new() -> Self {
        Self {
            listeners: HashMap::new(),
            udp_sockets: HashMap::new(),
            #[cfg(unix)]
            unix_listeners: HashMap::new(),
            #[cfg(unix)]
            unix_dgram: HashMap::new()
        }
    }

    pub fn assign_ephemeral_port(&self, core_rng: &mut rand_chacha::ChaCha8Rng) -> u16 {
        loop {
             let port = core_rng.gen_range(49152..=65535);
             // Improve logic here, make more realistic
             let addr = SocketAddr::from(([127, 0, 0, 1], port));
             if !self.listeners.contains_key(&addr) && !self.udp_sockets.contains_key(&addr) {
                 return port;
             }
        }
    }
}

pub struct TcpConnection {
    pub remote_addr: SocketAddr,
    pub tx: Sender<Bytes>,
    pub rx: AsyncReceiver<Bytes>
}

pub struct TcpSocket {
    ttl: Option<u32>,
    nodelay: bool,
    linger: Option<Option<Duration>>,
    recv_buffer_size: Option<u32>,
    send_buffer_size: Option<u32>,
}

impl TcpSocket {
    pub fn new_v4() -> Result<TcpSocket> {
        Ok(TcpSocket {
            ttl: None,
            nodelay: false,
            linger: None,
            recv_buffer_size: None,
            send_buffer_size: None,
        })
    }

    pub fn new_v6() -> Result<TcpSocket> {
        Ok(TcpSocket {
            ttl: None,
            nodelay: false,
            linger: None,
            recv_buffer_size: None,
            send_buffer_size: None,
        })
    }

    pub fn set_reuseaddr(&mut self, _reuseaddr: bool) -> Result<()> {
        Ok(())
    }

    pub fn set_reuseport(&mut self, _reuseport: bool) -> Result<()> {
        Ok(())
    }

    pub fn set_nodelay(&mut self, nodelay: bool) -> Result<()> {
        self.nodelay = nodelay;
        Ok(())
    }

    pub fn nodelay(&self) -> Result<bool> {
        Ok(self.nodelay)
    }

    pub fn set_linger(&mut self, dur: Option<Duration>) -> Result<()> {
        self.linger = Some(dur);
        Ok(())
    }

    pub fn linger(&self) -> Result<Option<Duration>> {
        Ok(self.linger.flatten())
    }

    pub fn set_ttl(&mut self, ttl: u32) -> Result<()> {
        self.ttl = Some(ttl);
        Ok(())
    }

    pub fn ttl(&self) -> Result<u32> {
        Ok(self.ttl.unwrap_or(64))
    }

    pub fn bind(&self, addr: SocketAddr) -> Result<TcpListener> {
        TcpListener::bind(addr)
    }

    pub async fn connect(self, addr: SocketAddr) -> Result<TcpStream> {
        TcpStream::connect(addr).await
    }
}

pub struct TcpListener {
    local_addr: SocketAddr,
    accept_rx: AsyncReceiver<TcpConnection>
}

impl TcpListener {
    pub async fn bind<A: ToSocketAddrs>(addr: A) -> Result<Self> {
        let addr = addr.to_socket_addrs()?.next().ok_or_else(|| Error::new(ErrorKind::InvalidInput, "fracture: Invalid address"))?;

        let handle = Handle::current();

        let core_rc = handle.core.upgrade().ok_or_else(|| Error::new(ErrorKind::Other, "fracture: Runtime dropped"))?;
        let mut core = core_rc.borrow_mut();

        if core.network.listeners.contains_key(&addr) {
            return Err(Error::new(ErrorKind::AddrInUse, format!("fracture: Address {} already in use", addr)));
        }

        let (tx, rx) = channel(128);

        core.network.listeners.insert(addr, tx);

        Ok(Self { local_addr: addr, accept_rx: rx })
    }

    pub async fn accept(&self) -> Result<(TcpStream, SocketAddr)> {
        let conn = self.accept_rx.recv().await.ok_or_else(|| Error::new(ErrorKind::BrokenPipe, "fracture: Listener closed"))?;

        let stream = TcpStream::new(conn.tx, conn.rx, self.local_addr, conn.remote_addr);

        Ok((stream, conn.remote_addr))
    }

    pub fn local_addr(&self) -> Result<SocketAddr> {
        Ok(self.local_addr)
    }

    pub fn ttl(&self) -> Result<u32> {
        Ok(64)
    }
    pub fn set_ttl(&self, _ttl: u32) -> Result<()> {
        Ok(())
    }
}

pub struct TcpStream {
    tx: Sender<Bytes>,
    rx: AsyncReceiver<Bytes>,
    read_buffer: BytesMut,
    local_addr: SocketAddr,
    peer_addr: SocketAddr,
    nodelay: bool,
    linger: Option<Duration>,
    ttl: u32
}

impl TcpStream {
    fn new(tx: Sender<Bytes>, rx: AsyncReceiver<Bytes>, local_addr: SocketAddr, peer_addr: SocketAddr) -> Self {
        Self {
            tx,
            rx,
            read_buffer: BytesMut::new(),
            local_addr,
            peer_addr,
            nodelay: false,
            linger: None,
            ttl: 64
        }
    }

    pub async fn connect<A: ToSocketAddrs>(addr: A) -> Result<Self> {
        if chaos::should_fail(ChaosOperation::TcpConnect) {
            return Err(Error::new(
                ErrorKind::ConnectionRefused,
                "fracture: Connection failed (chaos)",
            ));
        }

        let addr = addr.to_socket_addrs()?.next().ok_or_else(|| Error::new(ErrorKind::InvalidInput, "fracture: Invalid address"))?;

        let handle = Handle::current();
        let core_rc = handle.core.upgrade().ok_or_else(|| Error::new(ErrorKind::Other, "fracture: Runtime dropped"))?;

        let (local_port, listener_tx) = {
            let mut core = core_rc.borrow_mut();
            let port = core.network.assign_ephemeral_port(&mut core.rng);
            let listener = core.network.listeners.get(&addr).cloned();
            (port, listener)
        };

        let local_addr = SocketAddr::from(([127, 0, 0, 1], local_port));

        let local_addr_str = local_addr.to_string();
        let peer_addr_str = addr.to_string();

        if chaos::is_partitioned(&local_addr_str, &peer_addr_str) {
            if chaos::should_fail(ChaosOperation::NetworkPartition) {
                sleep(Duration::from_secs(60)).await;
                return Err(Error::new(
                    ErrorKind::TimedOut,
                    "fracture: Connection timed out (partition)",
                ));
            }
        }

        let listener_tx = listener_tx.ok_or_else(|| Error::new(ErrorKind::ConnectionRefused, "fracture: Connection refused"))?;

        let delay = chaos::get_delay(&peer_addr_str).unwrap_or(Duration::ZERO);
        if !delay.is_zero() {
            sleep(delay).await;
        }
        
        let (tx1, rx1) = channel(DEFAULT_TCP_WINDOW_SIZE);
        let (tx2, rx2) = channel(DEFAULT_TCP_WINDOW_SIZE);

        let server_conn = TcpConnection {
            remote_addr: local_addr,
            tx: tx2,
            rx: rx1
        };

        listener_tx.send(server_conn).map_err(|_| Error::new(ErrorKind::ConnectionRefused, "fracture: Remote backlog full"))?;

        Ok(Self::new(tx1, rx2, local_addr, addr))
    }

    pub async fn peek(&mut self, buf: &mut [u8]) -> Result<usize> {
        if chaos::should_fail(ChaosOperation::TcpPeek) {
            return Err(Error::new(ErrorKind::Other, "fracture: Peek failed (chaos)"));
        }

        if !self.read_buffer.is_empty() {
            let len = std::cmp::min(buf.len(), self.read_buffer.len());
            buf[..len].copy_from_slice(&self.read_buffer[..len]);

            return Ok(len);
        }

        match self.rx.peek().await {
            Some(data) => {
                let len = std::cmp::min(buf.len(), data.len());
                buf[..len].copy_from_slice(&data[..len]);
                
                Ok(len)
            }
            None => Ok(0)
        }
    }

    pub async fn ready(&self, interest: Interest) -> Result<Ready> {
        if interest.is_readable() {
            self.readable().await?;
        }
        if interest.is_writable() {
            self.writable().await?;
        }
        Ok(Ready::from_interest(interest))
    }

    pub async fn readable(&self) -> Result<()> {
        if chaos::should_fail(ChaosOperation::TcpReadReady) {
            std::future::pending().await
        }
        
        if !self.read_buffer.is_empty() {
            return Ok(());
        }

        struct ReadableFuture<'a> { rx: &'a AsyncReceiver<Bytes> }
        impl Future for ReadableFuture<'_> {
            type Output = Result<()>;
            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                match self.rx.poll_peek(cx) {
                    Poll::Ready(_) => Poll::Ready(Ok(())),
                    Poll::Pending => Poll::Pending,
                }
            }
        }
        ReadableFuture { rx: &self.rx }.await
    }

    pub async fn writable(&self) -> Result<()> {
        if chaos::should_fail(ChaosOperation::TcpWriteReady) {
            std::future::pending().await
        }

        struct WritableFuture<'a> { tx: &'a Sender<Bytes> }
        impl Future for WritableFuture<'_> {
            type Output = Result<()>;
            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                self.tx.poll_reserve(cx, 1).map(|_| Ok(()))
            }
        }
        WritableFuture { tx: &self.tx }.await
    }

    pub fn try_read(&mut self, buf: &mut [u8]) -> Result<usize> {
        if chaos::should_fail(ChaosOperation::TcpTryRead) {
             return Err(Error::new(ErrorKind::WouldBlock, "fracture: TryRead failed (chaos)"));
        }

        if !self.read_buffer.is_empty() {
            let len = std::cmp::min(buf.len(), self.read_buffer.len());
            buf[..len].copy_from_slice(&self.read_buffer[..len]);
            self.read_buffer.advance(len);
            return Ok(len);
        }
        
        match self.rx.try_recv() {
            Ok(Some(data)) => {
                let len = std::cmp::min(buf.len(), data.len());
                buf[..len].copy_from_slice(&data[..len]);
                if len < data.len() {
                    self.read_buffer.extend_from_slice(&data[len..]);
                }
                Ok(len)
            }
            Ok(None) => Ok(0),
            Err(_) => Err(Error::new(ErrorKind::WouldBlock, "fracture: Would block"))
        }
    }

    pub fn try_write(&self, buf: &[u8]) -> Result<usize> {
        if chaos::should_fail(ChaosOperation::TcpTryWrite) {
             return Err(Error::new(ErrorKind::WouldBlock, "fracture: TryWrite failed (chaos)"));
        }

        let bytes = Bytes::copy_from_slice(buf);
        match self.tx.try_send(bytes) {
            Ok(()) => Ok(buf.len()),
            Err(TrySendError::Full(_)) => Err(Error::new(ErrorKind::WouldBlock, "fracture: Would block")),
            Err(TrySendError::Closed(_)) => Err(Error::new(ErrorKind::BrokenPipe, "fracture: Broken pipe"))
        }
    }

    pub fn poll_peek(&mut self, cx: &mut Context<'_>, buf: &mut ReadBuf<'_>) -> Poll<Result<usize>> {
        // Need direct access to rx queue, fix later
        match self.rx.poll_peek(cx) {
            Poll::Ready(Some(data)) => {
                let len = std::cmp::min(buf.remaining(), data.len());
                buf.put_slice(&data[..len]);
                Poll::Ready(Ok(len))
            }
            Poll::Ready(None) => Poll::Ready(Ok(0)),
            Poll::Pending => Poll::Pending
        }
    }

    pub fn into_split(self) -> (OwnedReadHalf, OwnedWriteHalf) {
        let stream = Arc::new(Mutex::new(self));
        
        (OwnedReadHalf { stream: stream.clone() }, OwnedWriteHalf { stream })
    }

    pub fn poll_read_ready(&self, cx: &mut Context<'_>) -> Poll<Result<()>> {
        match self.rx.poll_peek(cx) {
            Poll::Ready(_) => Poll::Ready(Ok(())),
            Poll::Pending => Poll::Pending
        }
    }

    pub fn poll_write_ready(&self, cx: &mut Context<'_>) -> Poll<Result<()>> {
        self.tx.poll_reserve(cx, 1).map(|_| Ok(()))
    }

    pub fn local_addr(&self) -> Result<SocketAddr> {
        Ok(self.local_addr)
    }

    pub fn peer_addr(&self) -> Result<SocketAddr> {
        Ok(self.peer_addr)
    }

    pub fn set_nodelay(&mut self, nodelay: bool) -> Result<()> {
        self.nodelay = nodelay;

        Ok(())
    }

    pub fn nodelay(&self) -> Result<bool> {
        Ok(self.nodelay)
    }
    
    pub fn set_linger(&mut self, dur: Option<Duration>) -> Result<()> {
        self.linger = dur;

        Ok(())
    }

    pub fn linger(&self) -> Result<Option<Duration>> {
        Ok(self.linger)
    }

    pub fn set_ttl(&mut self, ttl: u32) -> Result<()> {
        self.ttl = ttl;
        Ok(())
    }

    pub fn ttl(&self) -> Result<u32> {
        Ok(self.ttl)
    }
}

impl AsyncRead for TcpStream {
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>
    ) -> Poll<Result<()>> {
        if chaos::should_fail(ChaosOperation::TcpRead) {
            return Poll::Ready(Err(Error::new(
                ErrorKind::BrokenPipe,
                "fracture: Read failed (chaos)",
            )));
        }

        let local = self.local_addr.to_string();
        let remote = self.peer_addr.to_string();
        if chaos::is_partitioned(&remote, &local) {
            return Poll::Pending;
        }

        if !self.read_buffer.is_empty() {
            let to_copy = std::cmp::min(buf.remaining(), self.read_buffer.len());
            buf.put_slice(&self.read_buffer[..to_copy]);
            self.read_buffer.advance(to_copy);
            return Poll::Ready(Ok(()))
        }

        match self.rx.poll_recv(cx) {
            Poll::Ready(Some(data)) => {
                let to_copy = std::cmp::min(buf.remaining(), data.len());
                buf.put_slice(&data[..to_copy]);

                if data.len() > to_copy {
                    self.read_buffer.extend_from_slice(&data[..to_copy]);
                }

                Poll::Ready(Ok(()))
            }
            Poll::Ready(None) => {
                Poll::Ready(Ok(()))
            }
            Poll::Pending => Poll::Pending
        }
    }
}

impl AsyncWrite for TcpStream {
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8]
    ) -> Poll<Result<usize>> {
        if chaos::should_fail(ChaosOperation::TcpWrite) {
            return Poll::Ready(Err(Error::new(
                ErrorKind::BrokenPipe,
                "fracture: Write failed (chaos)",
            )));
        }

        let local = self.local_addr.to_string();
        let remote = self.peer_addr.to_string();

        if chaos::should_drop_packet(&remote) {
            return Poll::Ready(Ok(buf.len()));
        }

        if chaos::is_partitioned(&local, &remote) {
            // Simulate a timeout/error later
            return Poll::Ready(Ok(buf.len()));
        }

        if let Poll::Pending = self.tx.poll_reserve(cx, buf.len()) {
            return Poll::Pending;
        }

        let delay = chaos::get_delay(&remote).unwrap_or(Duration::ZERO);

        let bytes = Bytes::copy_from_slice(buf);

        match self.tx.try_send_delayed(bytes, delay) {
            Ok(_) => Poll::Ready(Ok(buf.len())),
            Err(TrySendError::Closed(_)) => Poll::Ready(Err(Error::new(
                ErrorKind::BrokenPipe,
                "fracture: Connection reset",
            ))),
            Err(TrySendError::Full(_)) => {
                cx.waker().wake_by_ref();
                Poll::Pending
            }
        }
    }

    fn poll_flush(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<()>> {
        Poll::Ready(Ok(()))
    }

    fn poll_shutdown(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<()>> {
        self.tx.close();
        Poll::Ready(Ok(()))
    }
}

pub struct OwnedReadHalf {
    stream: Arc<Mutex<TcpStream>>
}
pub struct OwnedWriteHalf {
    stream: Arc<Mutex<TcpStream>>
}

impl AsyncRead for OwnedReadHalf {
    fn poll_read(self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &mut ReadBuf<'_>) -> Poll<Result<()>> {
        let mut stream = self.stream.lock().unwrap();

        Pin::new(&mut *stream).poll_read(cx, buf)
    }
}

impl AsyncWrite for OwnedWriteHalf {
    fn poll_write(self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &[u8]) -> Poll<Result<usize>> {
        let mut stream = self.stream.lock().unwrap();

        Pin::new(&mut *stream).poll_write(cx, buf)
    }
    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>> {
        let mut stream = self.stream.lock().unwrap();

        Pin::new(&mut *stream).poll_flush(cx)
    }
    fn poll_shutdown(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>> {
        let mut stream = self.stream.lock().unwrap();

        Pin::new(&mut *stream).poll_shutdown(cx)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Interest(u8);

impl Interest {
    pub const READABLE: Interest = Interest(0b01);

    pub const WRITABLE: Interest = Interest(0b10);

    pub const READ_WRITE: Interest = Interest(0b11);

    pub fn is_readable(self) -> bool {
        (self.0 & 0b01) != 0
    }
    pub fn is_writable(self) -> bool {
        (self.0 & 0b10) != 0
    }
    pub fn add(self, other: Interest) -> Interest {
        Interest(self.0 | other.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ready(u8);
impl Ready {
    pub const EMPTY: Ready = Ready(0);

    pub const READABLE: Ready = Ready(0b01);

    pub const WRITABLE: Ready = Ready(0b10);

    pub const READ_WRITE: Ready = Ready(0b11);

    pub fn is_readable(self) -> bool {
        (self.0 & 0b01) != 0
    }
    pub fn is_writable(self) -> bool {
        (self.0 & 0b10) != 0
    }
    fn from_interest(interest: Interest) -> Ready {
        Ready(interest.0)
    }
}

#[derive(Debug)]
pub struct SendError<T>(pub T);

impl<T> fmt::Display for SendError<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fracture: Channel closed")
    }
}

#[derive(Debug)]
pub enum TrySendError<T> {
    Full(T),
    Closed(T)
}

impl<T> fmt::Display for TrySendError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TrySendError::Full(_) => write!(f, "fracture: Channel full"),
            TrySendError::Closed(_) => write!(f, "fracture: Channel closed")
        }
    }
}

struct Envelope<T> {
    data: T,
    arrival_time: Duration
}

struct ChannelState<T> {
    queue: VecDeque<Envelope<T>>,
    capacity: usize,
    current_size: usize,
    closed: bool,
    recv_waiters: VecDeque<Waker>,
    send_waiters: VecDeque<Waker>
}

pub struct Sender<T> {
    state: Arc<Mutex<ChannelState<T>>>
}

pub struct AsyncReceiver<T> {
    state: Arc<Mutex<ChannelState<T>>>
}

impl<T: Buf + Send> Sender<T> {
    pub fn poll_reserve(
        &self,
        cx: &mut Context<'_>,
        size: usize
    ) -> Poll<()> {
        let mut s = self.state.lock().unwrap();
        if s.closed {
            return Poll::Ready(());
        }

        if s.current_size + size > s.capacity && s.current_size > 0 {
            s.send_waiters.push_back(cx.waker().clone());
            Poll::Pending
        }
        else {
            Poll::Ready(())
        }
    }

    pub async fn send(&self, item: T) -> std::result::Result<(), ()> {
        struct SendFuture<'a, T: Buf + Send> { sender: &'a Sender<T>, item: Option<T> }

        impl<T: Buf + Send> Future for SendFuture<'_, T> {
            type Output = std::result::Result<(), ()>;

            fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                let len = self.item.as_ref().unwrap().remaining();
                
                match self.sender.poll_reserve(cx, len) {
                    Poll::Ready(()) => {
                        let item = self.item.take().unwrap();
                        self.sender.try_send(item)?;
                        Poll::Ready(Ok(()))
                    }
                    Poll::Pending => Poll::Pending
                }
            }
        }

        SendFuture { sender: self, item: Some(item) }.await
    }

    pub fn try_send(&self, item: T) -> std::result::Result<(), ()> {
        let mut s = self.state.lock().unwrap();
        if s.closed {
            return Err(());
        }

        let len = item.remaining();
        s.queue.push_back(item);
        s.current_size += len;

        if let Some(w) = s.recv_waiters.pop_front() {
            w.wake();
        }

        Ok(())
    }

    pub fn try_send_delayed(
        &self,
        item: T,
        delay: Duration,
    ) -> std::result::Result<(), TrySendError<T>> {
        let mut s = self.state.lock().unwrap();
        if s.closed {
            return Err(TrySendError::Closed(item));
        }

        let len = item.remaining();
        if s.current_size + len > s.capacity && s.current_size > 0 {
            return Err(TrySendError::Full(item));
        }

        let handle = Handle::current();
        let core_rc = handle.core.upgrade().expect("fracture: Runtime dropped");
        let now = core_rc.borrow().current_time;
        let arrival_time = now + delay;

        let envelope = Envelope {
            data: item,
            arrival_time,
        };

        // Later, if queue is empty or last item arrives before this one, push back
        let insert_idx =
            if s.queue.is_empty() || s.queue.back().unwrap().arrival_time <= arrival_time {
                s.queue.len()
            } else {
                s.queue.partition_point(|e| e.arrival_time <= arrival_time)
            };

        s.queue.insert(insert_idx, envelope);
        s.current_size += len;

        if let Some(w) = s.recv_waiters.pop_front() {
            w.wake();
        }

        Ok(())
    }

    pub fn close(&self) {
        let mut s = self.state.lock().unwrap();
        s.closed = true;

        for w in s.recv_waiters.drain(..) { w.wake(); }
        for w in s.send_waiters.drain(..) { w.wake(); }
    }
}

impl<T> Clone for Sender<T> {
    fn clone(&self) -> Self {
        Self { state: self.state.clone() }
    }
}

impl<T: Buf + Send> AsyncReceiver<T> {
    pub async fn recv(&self) -> Option<T> {
        struct RecvFuture<'a, T: Buf + Send> { rx: &'a AsyncReceiver<T> }
        
        impl<T: Buf + Send> Future for RecvFuture<'_, T> {
            type Output = Option<T>;

            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                self.rx.poll_recv(cx)
            }
        }

        RecvFuture { rx: self }.await
    }

    pub fn poll_recv(&self, cx: &mut Context<'_>) -> Poll<Option<T>> {
        let mut s = self.state.lock().unwrap();

        let handle = Handle::current();
        let core_rc = handle.core.upgrade().expect("fracture: Runtime dropped");
        let now = core_rc.borrow().current_time;

        if let Some(envelope) = s.queue.front() {
            if envelope.arrival_time <= now {
                let envelope = s.queue.pop_front().unwrap();
                s.current_size -= envelope.data.remaining();

                if let Some(w) = s.send_waiters.pop_front() {
                    w.wake();
                }

                return Poll::Ready(Some(envelope.data));
            }
            else {
                let waker = cx.waker().clone();
                drop(s);

                let mut core = core_rc.borrow_mut();
                let entry = crate::runtime::core::TimerEntry {
                    deadline: envelope.arrival_time,
                    waker: waker,
                    id: core.rng.r#gen() as usize
                };
                core.timers.push(entry);

                return Poll::Pending;
            }
        }

        if s.closed {
            Poll::Ready(None)
        } else {
            s.recv_waiters.push_back(cx.waker().clone());
            Poll::Pending
        }
    }

    pub async fn peek(&self) -> Option<T> {
        let mut s = self.state.lock().unwrap();
        let handle = Handle::current();
        let core_rc = handle.core.upgrade().expect("fracture: Runtime dropped");
        let now = core_rc.borrow().current_time;

        if let Some(envelope) = s.queue.front() {
            if envelope.arrival_time <= now {
                return Some(envelope.data.clone());
            }
        }

        None
    }

    pub async fn poll_peek(&self, cx: &mut Context<'_>) -> Poll<Option<T>> {
        let mut s = self.state.lock().unwrap();
        let handle = Handle::current();
        let core_rc = handle.core.upgrade().expect("fracture: Runtime dropped");
        let now = core_rc.borrow().current_time;

        if let Some(envelope) = s.queue.front() {
            if envelope.arrival_time <= now {
                return Poll::Ready(Some(envelope.data.clone()));
            }

            let waker = cx.waker().clone();
            drop(s);
            let mut core = core_rc.borrow_mut();
            core.timers.push(crate::runtime::core::TimerEntry { deadline: envelope.arrival_time, waker, id: core.rng.r#gen() as usize });

            return Poll::Pending;
        }

        if s.closed {
            Poll::Ready(None)
        }
        else {
            s.recv_waiters.push_back(cx.waker().clone());

            Poll::Pending
        }
    }
}

pub fn channel<T>(capacity: usize) -> (Sender<T>, AsyncReceiver<T>) {
    let state = Arc::new(Mutex::new(ChannelState {
        queue: VecDeque::new(),
        capacity,
        current_size: 0,
        closed: false,
        recv_waiters: VecDeque::new(),
        send_waiters: VecDeque::new()
    }));

    (Sender { state: state.clone() }, AsyncReceiver { state })
}

pub struct UdpSocket {
    local_addr: SocketAddr,
    mailbox: AsyncReceiver<(Bytes, SocketAddr)>
}

impl UdpSocket {
    pub async fn bind<A: ToSocketAddrs>(addr: A) -> Result<Self> {
         if chaos::should_fail(ChaosOperation::UdpBind) {
            return Err(Error::new(ErrorKind::Other, "fracture: UdpBind failed (chaos)"));
        }

        let addr = addr.to_socket_addrs()?.next().ok_or_else(|| Error::new(ErrorKind::InvalidInput, "fracture: Invalid address"))?;
        
        let handle = Handle::current();
        let core_rc = handle.core.upgrade().ok_or_else(|| Error::new(ErrorKind::Other, "fracture: Runtime dropped"))?;
        let mut core = core_rc.borrow_mut();

        if core.network.udp_sockets.contains_key(&addr) {
            return Err(Error::new(ErrorKind::AddrInUse, "fracture: Address already in use"));
        }

        let (tx, rx) = channel(1024);
        core.network.udp_sockets.insert(addr, tx);

        Ok(Self { local_addr: addr, mailbox: rx })
    }

    pub fn local_addr(&self) -> Result<SocketAddr> {
        Ok(self.local_addr)
    }

    pub async fn send_to<A: ToSocketAddrs>(&self, buf: &[u8], target: A) -> Result<usize> {
        if chaos::should_fail(ChaosOperation::UdpSendTo) {
            return Err(Error::new(ErrorKind::Other, "fracture: SendTo failed (chaos)"));
        }

        let target = target.to_socket_addrs()?.next().ok_or_else(|| Error::new(ErrorKind::InvalidInput, "fracture: Invalid address"))?;

        if buf.len() > MAX_UDP_PACKET_SIZE {
            return Err(Error::new(ErrorKind::InvalidInput, "fracture: Packet too large"));
        }

        let local_str = self.local_addr.to_string();
        let target_str = target.to_string();

        if chaos::should_drop_packet(&local_str) || chaos::is_partitioned(&local_str, &target_str) {
            return Ok(buf.len()); 
        }

        let handle = Handle::current();
        let core_rc = handle.core.upgrade().unwrap();
        
        let target_mailbox = {
            let core = core_rc.borrow();
            core.network.udp_sockets.get(&target).cloned()
        };

        if let Some(mailbox) = target_mailbox {
            let data = Bytes::copy_from_slice(buf);
            let delay = chaos::get_delay(&target_str).unwrap_or(Duration::ZERO);
            
            mailbox.try_send_delayed((data, self.local_addr), delay);
        }

        Ok(buf.len())
    }

    pub async fn recv_from(&self, buf: &mut [u8]) -> Result<(usize, SocketAddr)> {
        if chaos::should_fail(ChaosOperation::UdpRecvFrom) {
            return Err(Error::new(ErrorKind::Other, "fracture: RecvFrom hang (chaos)"));
        }

        loop {
            match self.mailbox.recv().await {
                Some((data, src)) => {
                    let n = std::cmp::min(buf.len(), data.len());
                    buf[..n].copy_from_slice(&data[..n]);
                    return Ok((n, src));
                }
                None => {
                    return Err(Error::new(ErrorKind::BrokenPipe, "fracture: Socket closed"));
                }
            }
        }
    }

    pub fn try_send_to<A: ToSocketAddrs>(&self, buf: &[u8], target: A) -> Result<usize> {
        if chaos::should_fail(ChaosOperation::UdpTrySendTo) {
            return Err(Error::new(ErrorKind::WouldBlock, "fracture: TrySendTo failed"));
        }

        let target = target.to_socket_addrs()?.next().ok_or_else(|| Error::new(ErrorKind::InvalidInput, "fracture: Invalid address"))?;
        let local_str = self.local_addr.to_string();
        let target_str = target.to_string();

        if chaos::is_partitioned(&local_str, &target_str) {
            return Ok(buf.len());
        }

        let handle = Handle::current();
        let core_rc = handle.core.upgrade().unwrap();
        let target_mailbox = { let core = core_rc.borrow(); core.network.udp_sockets.get(&target).cloned() };
        
        if let Some(mailbox) = target_mailbox {
            let data = Bytes::copy_from_slice(buf);
            mailbox.try_send((data, self.local_addr)).map_err(|_| Error::new(ErrorKind::WouldBlock, "fracture: Buffer full"))?;
        }
        Ok(buf.len())
    }

    pub fn try_recv_from(&self, buf: &mut [u8]) -> Result<(usize, SocketAddr)> {
        if chaos::should_fail(ChaosOperation::UdpTryRecvFrom) { return Err(Error::new(ErrorKind::WouldBlock, "fracture: TryRecvFrom failed")); }
        match self.mailbox.try_recv() {
            Ok(Some((data, src))) => {
                let n = std::cmp::min(buf.len(), data.len());
                buf[..n].copy_from_slice(&data[..n]);
                Ok((n, src))
            }
            Ok(None) => Err(Error::new(ErrorKind::WouldBlock, "fracture: Would block")),
            Err(_) => Err(Error::new(ErrorKind::BrokenPipe, "fracture: Socket closed"))
        }
    }
}

#[cfg(unix)]
pub mod unix {
    use super::*;
    use std::path::Path;

    #[derive(Clone, Debug)]
    pub struct SocketAddr(PathBuf);

    impl SocketAddr {
        pub fn as_pathname(&self) -> Option<&Path> {
            Some(&self.0)
        }
        pub fn is_unnamed(&self) -> bool { false }
    }

    pub(crate) struct UnixConnection {
        pub peer_path: PathBuf,
        pub tx: Sender<Bytes>,
        pub rx: AsyncReceiver<Bytes>
    }

    pub struct UnixListener {
        path: PathBuf,
        accept_rx: AsyncReceiver<UnixConnection>
    }

    impl UnixListener {
        pub fn bind<P: AsRef<Path>>(path: P) -> Result<UnixListener> {
            if chaos::should_fail(ChaosOperation::UnixBind) {
                return Err(Error::new(ErrorKind::Other, "fracture: UnixBind failed (chaos)"));
            }

            let path = path.as_ref().to_path_buf();
            let handle = Handle::current();
            let core_rc = handle.core.upgrade().unwrap();
            let mut core = core_rc.borrow_mut();

            if core.network.unix_listeners.contains_key(&path) {
                return Err(Error::new(ErrorKind::AddrInUse, "fracture: Address already in use"));
            }

            let (tx, rx) = channel(128);
            core.network.unix_listeners.insert(path.clone(), tx);

            Ok(UnixListener { path, accept_rx: rx })
        }

        pub async fn accept(&self) -> Result<(UnixStream, SocketAddr)> {
            if chaos::should_fail(ChaosOperation::UnixAccept) {
                return Err(Error::new(ErrorKind::Other, "fracture: UnixAccept failed (chaos)"));
            }

            let conn = self.accept_rx.recv().await.ok_or(Error::new(ErrorKind::BrokenPipe, "fracture: Listener closed"))?;
            let stream = UnixStream::new(conn.tx, conn.rx, self.path.clone(), conn.peer_path.clone());
            
            Ok((stream, SocketAddr(conn.peer_path)))
        }
        
        pub fn local_addr(&self) -> Result<SocketAddr> {
            Ok(SocketAddr(self.path.clone()))
        }
    }

    pub struct UnixStream {
        tx: Sender<Bytes>,
        rx: AsyncReceiver<Bytes>,
        read_buffer: BytesMut,
        local_path: PathBuf,
        peer_path: PathBuf
    }

    impl UnixStream {
        pub(crate) fn new(tx: Sender<Bytes>, rx: AsyncReceiver<Bytes>, local: PathBuf, peer: PathBuf) -> Self {
            Self { tx, rx, read_buffer: BytesMut::new(), local_path: local, peer_path: peer }
        }

        pub async fn connect<P: AsRef<Path>>(path: P) -> Result<UnixStream> {
            if chaos::should_fail(ChaosOperation::UnixConnect) {
                 return Err(Error::new(ErrorKind::ConnectionRefused, "fracture: UnixConnect failed (chaos)"));
            }

            let peer_path = path.as_ref().to_path_buf();
            let handle = Handle::current();
            let core_rc = handle.core.upgrade().unwrap();
            
            let (tx1, rx1) = channel(DEFAULT_TCP_WINDOW_SIZE);
            let (tx2, rx2) = channel(DEFAULT_TCP_WINDOW_SIZE);

            let mut core = core_rc.borrow_mut();
            let ephemeral_id = core.rng.r#gen();
            let local_path = PathBuf::from(format!("/tmp/fracture_ephemeral_{}", ephemeral_id));

            let listener = core.network.unix_listeners.get(&peer_path).cloned();
            drop(core);

            if let Some(listener_tx) = listener {
                let conn = UnixConnection {
                    peer_path: local_path.clone(),
                    tx: tx2,
                    rx: rx1
                };
                listener_tx.send(conn).map_err(|_| Error::new(ErrorKind::ConnectionRefused, "fracture: Connection refused"))?;
            } else {
                return Err(Error::new(ErrorKind::NotFound, "fracture: Socket not found"));
            }

            Ok(UnixStream::new(tx1, rx2, local_path, peer_path))
        }

        pub fn local_addr(&self) -> Result<SocketAddr> {
            Ok(SocketAddr(self.local_path.clone()))
        }

        pub fn peer_addr(&self) -> Result<SocketAddr> {
            Ok(SocketAddr(self.peer_path.clone()))
        }

        pub fn split(self) -> (crate::io::ReadHalf<UnixStream>, crate::io::WriteHalf<UnixStream>) {
            crate::io::split(self)
        }
    }

    impl AsyncRead for UnixStream {
        fn poll_read(mut self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &mut ReadBuf<'_>) -> Poll<Result<()>> {
            if chaos::should_fail(ChaosOperation::UnixRecv) {
                return Poll::Ready(Err(Error::new(ErrorKind::Other, "fracture: UnixRecv failed (chaos)")));
            }

            if !self.read_buffer.is_empty() {
                let len = std::cmp::min(buf.remaining(), self.read_buffer.len());
                buf.put_slice(&self.read_buffer[..len]);
                self.read_buffer.advance(len);
                return Poll::Ready(Ok(()));
            }

            match self.rx.poll_recv(cx) {
                Poll::Ready(Some(data)) => {
                    let len = std::cmp::min(buf.remaining(), data.len());
                    buf.put_slice(&data[..len]);
                    if data.len() > len {
                         self.read_buffer.extend_from_slice(&data[len..]);
                    }
                    Poll::Ready(Ok(()))
                }
                Poll::Ready(None) => Poll::Ready(Ok(())),
                Poll::Pending => Poll::Pending
            }
        }
    }

    impl AsyncWrite for UnixStream {
        fn poll_write(self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &[u8]) -> Poll<Result<usize>> {
             if chaos::should_fail(ChaosOperation::UnixSend) {
                return Poll::Ready(Err(Error::new(ErrorKind::BrokenPipe, "fracture: UnixSend failed (chaos)")));
            }
            
            let bytes = Bytes::copy_from_slice(buf);
            match self.tx.try_send(bytes) {
                Ok(_) => Poll::Ready(Ok(buf.len())),
                Err(TrySendError::Closed(_)) => Poll::Ready(Err(Error::new(ErrorKind::BrokenPipe, "fracture: Broken pipe"))),
                Err(TrySendError::Full(_)) => {
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }
            }
        }

        fn poll_flush(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<()>> {
            Poll::Ready(Ok(()))
        }

        fn poll_shutdown(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<()>> {
            self.tx.close();
            Poll::Ready(Ok(()))
        }
    }

    pub struct UnixDatagram {
        path: PathBuf,
        mailbox: AsyncReceiver<(Bytes, PathBuf)>
    }

    impl UnixDatagram {
        pub fn bind<P: AsRef<Path>>(path: P) -> Result<UnixDatagram> {
            let path = path.as_ref().to_path_buf();
            let handle = Handle::current();
            let core_rc = handle.core.upgrade().unwrap();
            let mut core = core_rc.borrow_mut();

            if core.network.unix_dgram.contains_key(&path) {
                return Err(Error::new(ErrorKind::AddrInUse, "fracture: Address already in use"));
            }

            let (tx, rx) = channel(1024);
            core.network.unix_dgram.insert(path.clone(), tx);

            Ok(UnixDatagram { path, mailbox: rx })
        }

        pub fn unbound() -> Result<UnixDatagram> {
            let handle = Handle::current();
            let core_rc = handle.core.upgrade().unwrap();
            let mut core = core_rc.borrow_mut();
            use rand::Rng;
            let id = core.rng.r#gen();
            let path = PathBuf::from(format!("/tmp/fracture_dgram_{}", id));
             
            let (tx, rx) = channel(1024);
            core.network.unix_dgram.insert(path.clone(), tx);
             
            Ok(UnixDatagram { path, mailbox: rx })
        }

        pub async fn send_to<P: AsRef<Path>>(&self, buf: &[u8], target: P) -> Result<usize> {
            if chaos::should_fail(ChaosOperation::UnixDatagramSend) {
                return Err(Error::new(ErrorKind::Other, "fracture: UnixDgramSend failed (chaos)"));
            }
            
            let target = target.as_ref().to_path_buf();
            let handle = Handle::current();
            let core_rc = handle.core.upgrade().unwrap();
            let target_mailbox = {
                let core = core_rc.borrow();
                core.network.unix_dgram.get(&target).cloned()
            };

            if let Some(mailbox) = target_mailbox {
                let data = Bytes::copy_from_slice(buf);
                mailbox.try_send((data, self.path.clone())).map_err(|_| Error::new(ErrorKind::Other, "fracture: Buffer full"))?;
                Ok(buf.len())
            }
            else {
                Err(Error::new(ErrorKind::NotFound, "fracture: Socket not found"))
            }
        }

        pub async fn recv_from(&self, buf: &mut [u8]) -> Result<(usize, SocketAddr)> {
            if chaos::should_fail(ChaosOperation::UnixDatagramRecv) {
                 return Err(Error::new(ErrorKind::Other, "fracture: UnixDgramRecv failed (chaos)"));
            }

            match self.mailbox.recv().await {
                Some((data, src)) => {
                    let len = std::cmp::min(buf.len(), data.len());
                    buf[..len].copy_from_slice(&data[..len]);
                    Ok((len, SocketAddr(src)))
                }
                None => Err(Error::new(ErrorKind::BrokenPipe, "fracture: Socket closed"))
            }
        }
    }
}