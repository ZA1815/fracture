use tokio::time::Instant;
use turmoil;
use std::io::{self, Result};
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr, ToSocketAddrs};
use std::sync::{Arc, Mutex as StdMutex};
use std::pin::Pin;
use std::task::{Context, Poll};
use std::time::Duration;
use std::ops::BitOr;
use std::collections::HashMap;
use tokio::io::{AsyncRead, AsyncWrite, ReadBuf};
use parking_lot::RwLock;

use crate::chaos::{self, ChaosOperation};
use crate::time::sleep;

pub struct TcpStream {
    inner: turmoil::net::TcpStream,
    local_addr: SocketAddr,
    peer_addr: SocketAddr,
    options: TcpSocketOptions,
    state: TcpStreamState,
    read_chaos_state: std::cell::RefCell<ReadChaosState>,
    write_chaos_state: std::cell::RefCell<WriteChaosState>
}

#[derive(Default)]
struct ReadChaosState {
    bytes_until_failure: Option<usize>,
    corruption_pattern: Option<Vec<u8>>,
    delay_remaining: Option<Duration>
}

#[derive(Default)]
struct WriteChaosState {
    bytes_until_failure: Option<usize>,
    partial_write_remaining: Option<usize>,
    delay_remaining: Option<Duration>
}

impl TcpStream {
    pub async fn connect<A: ToSocketAddrs>(addr: A) -> Result<Self> {
        chaos::trace::record(chaos::trace::TraceEvent::ConnectionAttempt {
            from: "client".to_string(),
            to: addr.to_socket_addrs()?.next().unwrap().to_string(),
            addr: addr.to_socket_addrs()?.next().unwrap().to_string()
        });

        if chaos::should_fail(ChaosOperation::TcpConnect) {
            chaos::trace::record(chaos::trace::TraceEvent::ConnectionFailed {
                from: "client".to_string(),
                to: addr.to_socket_addrs()?.next().unwrap().to_string(),
                error: "fracture: Connection refused (chaos)".to_string()
            });

            return Err(io::Error::new(
                io::ErrorKind::ConnectionRefused,
                "fracture: Connection refused (chaos)"
            ))
        }

        if chaos::should_fail(ChaosOperation::ConnectionTimeout) {
            tokio::time::sleep(Duration::from_secs(30)).await;
            return Err(io::Error::new(
                io::ErrorKind::TimedOut,
                "fracture: Connection timeout (chaos)"
            ));
        }

        if chaos::should_fail(ChaosOperation::DnsFailure) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: DNS resolution failed (chaos)"
            ));
        }

        let init_addr = addr.to_socket_addrs()?.next()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "fracture: No addresses"))?;
        let resolved_addrs = lookup_host(init_addr.to_string()).await?;
        let addr = resolved_addrs.get(0)
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "fracture: No address resolved"))?;

        if let Ok(hostname) = std::env::var("FRACTURE_HOSTNAME") {
            let target = addr.ip().to_string();
            if chaos::is_partitioned(&hostname, &target) {
                return Err(io::Error::new(
                    io::ErrorKind::NetworkUnreachable,
                    "fracture: Network partitioned"
                ));
            }
        }

        if let Some(delay) = chaos::get_delay(&addr.ip().to_string()) {
            tokio::time::sleep(delay).await;
        }

        let inner = turmoil::net::TcpStream::connect(addr).await?;
        let local_addr = inner.local_addr()?;
        let peer_addr = inner.peer_addr()?;

        chaos::trace::record(chaos::trace::TraceEvent::ConnectionSuccess { from: "client".to_string(), to: peer_addr.to_string() });

        Ok(Self {
            inner,
            local_addr,
            peer_addr,
            options: TcpSocketOptions::default(),
            state: TcpStreamState::new(local_addr, peer_addr),
            read_chaos_state: std::cell::RefCell::new(ReadChaosState::default()),
            write_chaos_state: std::cell::RefCell::new(WriteChaosState::default())
        })
    }

    pub fn local_addr(&self) -> Result<SocketAddr> {
        Ok(self.local_addr)
    }

    pub fn peer_addr(&self) -> Result<SocketAddr> {
        Ok(self.peer_addr)
    }

    pub fn no_delay(&self) -> Result<bool> {
        Ok(self.options.nodelay)
    }

    pub fn set_nodelay(&mut self, nodelay: bool) -> Result<()> {
        if chaos::should_fail(ChaosOperation::TcpSetNodelay) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Set nodelay failed (chaos)"
            ));
        }
        self.options.nodelay = nodelay;


        Ok(())
    }

    pub fn ttl(&self) -> Result<u32> {
        Ok(self.options.ttl)
    }

    pub fn set_ttl(&mut self, ttl: u32) -> Result<()> {
        if chaos::should_fail(ChaosOperation::TcpSetTtl) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Set TTL failed (chaos)"
            ));
        }
        self.options.ttl = ttl;
        Ok(())
    }

    pub fn linger(&self) -> Result<Option<Duration>> {
        Ok(self.options.linger)
    }

    pub fn set_linger(&mut self, linger: Option<Duration>) -> Result<()> {
        if chaos::should_fail(ChaosOperation::TcpSetLinger) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Set linger failed (chaos)"
            ));
        }
        self.options.linger = linger;
        Ok(())
    }

    pub fn poll_read_ready(&self, cx: &mut Context<'_>) -> Poll<Result<()>> {
        if crate::chaos::should_fail(crate::chaos::ChaosOperation::TcpRead) {
            return Poll::Ready(Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: Read failed (chaos)"
            )));
        }
        else {
            Poll::Ready(Ok(()))
        }
    }

    pub fn poll_write_ready(&self, cx: &mut Context<'_>) -> Poll<Result<()>> {
        if crate::chaos::should_fail(crate::chaos::ChaosOperation::TcpWrite) {
            return Poll::Ready(Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: Write failed (chaos)"
            )));
        }
        else {
            Poll::Ready(Ok(()))
        }
    }

    pub async fn readable(&self) -> Result<()> {
        if chaos::should_fail(ChaosOperation::TcpRead) {
            return Err(io::Error::new(
                io::ErrorKind::WouldBlock,
                "fracture: Not readable (chaos)"
            ));
        }
        Ok(())
    }

    pub async fn writeable(&self) -> Result<()> {
        if chaos::should_fail(ChaosOperation::TcpWrite) {
            return Err(io::Error::new(
                io::ErrorKind::WouldBlock,
                "fracture: Not writable (chaos)"
            ));
        }
        Ok(())
    }

    pub fn try_read(&self, buf: &mut [u8]) -> Result<usize> {
        if chaos::should_fail(ChaosOperation::TcpTryRead) {
            return Err(io::Error::new(
                io::ErrorKind::WouldBlock,
                "fracture: Try read would block (chaos)"
            ));
        }

        if chaos::should_fail(ChaosOperation::IoPartialRead) && buf.len() > 1 {
            return Ok(buf.len() / 2);
        }

        Ok(0)
    }

    pub fn try_write(&self, buf: &mut [u8]) -> Result<usize> {
        if chaos::should_fail(ChaosOperation::TcpTryWrite) {
            return Err(io::Error::new(
                io::ErrorKind::WouldBlock,
                "fracture: Try write would block (chaos)"
            ));
        }

        if chaos::should_fail(ChaosOperation::TcpPartialWrite) && buf.len() > 1 {
            return Ok(buf.len() / 2)
        }

        Ok(buf.len())
    }

    pub async fn peek(&self, buf: &mut [u8]) -> Result<usize> {
        if chaos::should_fail(ChaosOperation::TcpPeek) {
            return Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: Peek failed (chaos)"
            ));
        }

        Ok(0)
    }

    pub fn split<'a>(&'a mut self) -> (ReadHalf<'a>, WriteHalf<'a>) {
        (ReadHalf { stream: self }, WriteHalf { stream: self })
    }

    pub fn into_split(self) -> (OwnedReadHalf, OwnedWriteHalf) {
        todo!("Owned split requires Arc")
    }

    pub async fn ready(&self, interest: Interest) -> Result<Ready> {
        if interest.is_readable() && chaos::should_fail(ChaosOperation::TcpRead) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Not ready for reading (chaos)"
            ));
        }
        if interest.is_writable() && chaos::should_fail(ChaosOperation::TcpWrite) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Not ready for writing (chaos)"
            ));
        }

        Ok(Ready::READABLE | Ready::WRITABLE)
    }
}

impl AsyncRead for TcpStream {
    fn poll_read(mut self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &mut ReadBuf<'_>) -> Poll<Result<()>> {
        if chaos::should_fail(ChaosOperation::TcpRead) {
            return Poll::Ready(Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: Read failed (chaos)"
            )));
        }

        if chaos::should_fail(ChaosOperation::ConnectionReset) {
            return Poll::Ready(Err(io::Error::new(
                io::ErrorKind::ConnectionReset,
                "fracture: Connection reset by peer (chaos)"
            )));
        }

        if chaos::should_drop_packet(&self.peer_addr.ip().to_string()) {
            chaos::trace::record(chaos::trace::TraceEvent::PacketDropped {
                node: self.peer_addr.to_string(),
                packet_id: rand::random()
            });
            return Poll::Pending;
        }

        if chaos::should_fail(ChaosOperation::IoCorruption) {
            let filled_before = buf.filled().len();
            let result = Pin::new(&mut self.inner).poll_read(cx, buf);
            if let Poll::Ready(Ok(())) = result {
                let filled_after = buf.filled().len();
                let new_bytes = filled_after - filled_before;
                if new_bytes > 0 {
                    let filled_mut = buf.filled_mut();
                    for i in filled_before..filled_after {
                        filled_mut[i] ^= 0xFF;
                    }
                }
            }
            return result;
        }

        if chaos::should_fail(ChaosOperation::IoEof) {
            return Poll::Ready(Ok(()));
        }

        let filled_before = buf.filled().len();
        let result = Pin::new(&mut self.inner).poll_read(cx, buf);
        if let Poll::Ready(Ok(())) = &result {
            let filled_after = buf.filled().len();
            let new_bytes = filled_after - filled_before;
            self.state.bytes_read += new_bytes as u64;
        }

        result
    }
}

#[cfg(feature = "simulation")]
impl AsyncWrite for TcpStream {
    fn poll_write(mut self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &[u8]) -> Poll<Result<usize>> {
        if crate::chaos::should_fail(crate::chaos::ChaosOperation::TcpWrite) {
            return Poll::Ready(Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: Write failed (chaos)"
            )));
        }

        if chaos::should_fail(crate::chaos::ChaosOperation::TcpPartialWrite) && buf.len() > 1 {
            let partial = buf.len() / 2;
            if partial > 0 {
                let result = Pin::new(&mut self.inner).poll_write(cx, &buf[..partial]);
                if let Poll::Ready(Ok(n)) = &result {
                    self.state.bytes_written += *n as u64;
                }
                return result;
            }
        }

        if chaos::should_fail(ChaosOperation::BandwidthThrottle) {
            let max_bytes = 1024;
            let limited_buf = if buf.len() > max_bytes {
                &buf[..max_bytes]
            }
            else {
                buf
            };
            let result = Pin::new(&mut self.inner).poll_write(cx, limited_buf);
            if let Poll::Ready(Ok(n)) = &result {
                self.state.bytes_written += *n as u64;
                return result;
            }
        }

        chaos::trace::record(chaos::trace::TraceEvent::DataSent{
            from: self.local_addr.to_string(),
            to: self.peer_addr.to_string(),
            bytes: buf.len()
        });

        let result = Pin::new(&mut self.inner).poll_write(cx, buf);
        if let Poll::Ready(Ok(n)) = &result {
            self.state.bytes_written += *n as u64;
        }

        result
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>> {
        if chaos::should_fail(ChaosOperation::TcpFlush) {
            return Poll::Ready(Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Flush failed (chaos)"
            )));
        }
        Pin::new(&mut self.inner).poll_flush(cx)
    }

    fn poll_shutdown(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>> {
        if chaos::should_fail(ChaosOperation::TcpShutdown) {
            return Poll::Ready(Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Shutdown failed (chaos)"
            )));
        }

        let result = Pin::new(&mut self.inner).poll_shutdown(cx);
        if let Poll::Ready(Ok(())) = &result {
            self.state.half_closed = true;
        }

        result
    }

    fn poll_write_vectored(
            mut self: Pin<&mut Self>,
            cx: &mut Context<'_>,
            bufs: &[io::IoSlice<'_>],
        ) -> Poll<std::result::Result<usize, io::Error>> {
        if chaos::should_fail(ChaosOperation::TcpWrite) {
            return Poll::Ready(Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
            "fracture: Write vectored failed (chaos)"
            )));
        }

        let result = Pin::new(&mut self.inner).poll_write_vectored(cx, bufs);
        if let Poll::Ready(Ok(n)) = &result {
            self.state.bytes_written += *n as u64;
        }

        result
    }

    fn is_write_vectored(&self) -> bool {
        true
    }
}

pub struct ReadHalf<'a> {
    stream: &'a TcpStream
}

pub struct WriteHalf<'a> {
    stream: &'a TcpStream
}

pub struct OwnedReadHalf {
    // Placeholder
}

pub struct OwnedWriteHalf {
    // Placeholder
}

pub struct TcpListener {
    inner: turmoil::net::TcpListener,
    local_addr: SocketAddr
}

#[cfg(feature = "simulation")]
impl TcpListener {
    pub async fn bind<A: ToSocketAddrs>(addr: A) -> Result<Self> {
        let addr = addr.to_socket_addrs()?.next()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "fracture: No addresses"))?;

        if chaos::should_fail(ChaosOperation::FileDescriptorExhaustion) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Too many open files (chaos)"
            ));
        }

        let inner = turmoil::net::TcpListener::bind(addr).await?;
        let local_addr = inner.local_addr()?;

        Ok(Self { inner, local_addr })
    }

    pub async fn accept(&self) -> Result<(TcpStream, SocketAddr)> {
        if crate::chaos::should_fail(chaos::ChaosOperation::TcpAccept) {
            return Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: Accept failed (chaos)"
            ));
        }

        if chaos::should_fail(ChaosOperation::ThunderingHerdTrigger) {
            tokio::time::sleep(Duration::from_millis(rand::random::<u64>() % 100)).await;
        }

        let (stream, addr) = self.inner.accept().await?;
        let local_addr = stream.local_addr()?;
        let peer_addr = stream.peer_addr()?;

        Ok((
            TcpStream {
                inner: stream,
                local_addr,
                peer_addr,
                options: TcpSocketOptions::default(),
                state: TcpStreamState::new(local_addr, peer_addr),
                read_chaos_state: std::cell::RefCell::new(ReadChaosState::default()),
                write_chaos_state: std::cell::RefCell::new(WriteChaosState::default())
            },
            addr
        ))
    }

    pub fn local_addr(&self) -> Result<SocketAddr> {
        Ok(self.local_addr)
    }

    pub fn ttl(&self) -> Result<u32> {
        Ok(64)
    }

    pub fn set_ttl(&self, ttl: u32) -> Result<()> {
        Ok(())
    }

    pub fn poll_accept(&self, cx: &mut Context<'_>) -> Poll<Result<(TcpStream, SocketAddr)>> {
        if chaos::should_fail(ChaosOperation::TcpAccept) {
            return Poll::Ready(Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: Poll accept failed (chaos)"
            )));
        }
        Poll::Pending // Placeholder
    }
}

#[cfg(feature = "simulation")]
pub struct UdpSocket {
    inner: turmoil::net::UdpSocket,
    local_addr: SocketAddr
}

#[cfg(feature = "simulation")]
impl UdpSocket {
    pub async fn bind<A: ToSocketAddrs>(addr: A) -> io::Result<Self> {
        if chaos::should_fail(ChaosOperation::UdpBind) {
            return Err(io::Error::new(
                io::ErrorKind::AddrInUse,
                "fracture: Address already in use (chaos)"
            ));
        }

        let addr = addr.to_socket_addrs()?.next()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "fracture: No addresses"))?;
        let inner = turmoil::net::UdpSocket::bind(addr).await?;
        let local_addr = inner.local_addr()?;
        Ok(Self { inner, local_addr })
    }

    pub fn local_addr(&self) -> io::Result<SocketAddr> {
        Ok(self.local_addr)
    }

    pub async fn connect<A: ToSocketAddrs>(&self, addr: A) -> Result<()> {
        if chaos::should_fail(ChaosOperation::UdpConnect) {
            return Err(io::Error::new(
                io::ErrorKind::ConnectionRefused,
                "fracture: UDP connect failed (chaos)"
            ));
        }

        Ok(())
    }

    pub async fn send(&self, buf: &[u8]) -> Result<usize> {
        if chaos::should_fail(ChaosOperation::UdpSend) {
            return Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: UDP send failed (chaos)"
            ));
        }

        Ok(buf.len())
    }

    pub async fn recv(&self, buf: &mut [u8]) -> Result<usize> {
        if chaos::should_fail(ChaosOperation::UdpRecv) {
            return Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: UDP recv failed (chaos)"
            ));
        }

        Ok(0)
    }

    pub async fn send_to(&self, buf: &[u8], target: impl ToSocketAddrs) -> io::Result<usize> {
        if chaos::should_fail(ChaosOperation::UdpSendTo) {
            return Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: UDP send_to failed (chaos)"
            ));
        }

        let target = target.to_socket_addrs()?.next()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "fracture: No addresses"))?;

        if chaos::should_drop_packet(&target.ip().to_string()) {
            return Ok(buf.len());
        }

        if chaos::should_fail(ChaosOperation::PacketDuplicate) {
            self.inner.send_to(buf, target).await;
        }

        if chaos::should_fail(ChaosOperation::PacketCorruption) {
            let mut corrupted = buf.to_vec();
            if !corrupted.is_empty() {
                corrupted[0] ^= 0xFF;
            }
            return self.inner.send_to(&corrupted, target).await;
        }

        self.inner.send_to(buf, target).await
    }

    pub async fn recv_from(&self, buf: &mut [u8]) -> io::Result<(usize, SocketAddr)> {
        if crate::chaos::should_fail(crate::chaos::ChaosOperation::UdpRecvFrom) {
            return Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: UDP recv_from failed (chaos)"
            ));
        }

        if chaos::should_fail(ChaosOperation::PacketReorder) {
            tokio::time::sleep(Duration::from_millis(rand::random::<u64>() % 50)).await;
        }

        self.inner.recv_from(buf).await
    }

    pub async fn peek_from(&self, buf: &mut [u8]) -> Result<(usize, SocketAddr)> {
        if chaos::should_fail(ChaosOperation::UdpPeekFrom) {
            return Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: UDP peek_from failed (chaos)"
            ));
        }

        Ok((0, self.local_addr))
    }

    pub fn try_recv(&self, buf: &mut [u8]) -> Result<usize> {
        if chaos::should_fail(ChaosOperation::UdpTryRecv) {
            return Err(io::Error::new(
                io::ErrorKind::WouldBlock,
                "fracture: try_recv would block (chaos)"
            ));
        }

        Ok(0)
    }

    pub fn try_send_to(&self, buf: &[u8], target: impl ToSocketAddrs) -> Result<usize> {
        if chaos::should_fail(ChaosOperation::UdpTrySendTo) {
            return Err(io::Error::new(
                io::ErrorKind::WouldBlock,
                "fracture: try_send_to would block (chaos)"
            ));
        }

        Ok(buf.len())
    }

    pub fn try_recv_from(&self, buf: &mut [u8]) -> Result<(usize, SocketAddr)> {
        if chaos::should_fail(ChaosOperation::UdpTryRecvFrom) {
            return Err(io::Error::new(
                io::ErrorKind::WouldBlock,
                "fracture: try_recv_from would block (chaos)"
            ));
        }

        Ok((0, self.local_addr))
    }

    pub async fn peek(&self, buf: &mut [u8]) -> Result<usize> {
        if chaos::should_fail(ChaosOperation::UdpPeek) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: UDP peek failed (chaos)"
            ));
        }

        Ok(0)
    }

    pub fn broadcast(&self) -> Result<bool> {
        Ok(false)
    }

    pub fn set_broadcast(&self, on: bool) -> Result<()> {
        if chaos::should_fail(ChaosOperation::UdpBroadcast) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Set broadcast failed (chaos)"
            ));
        }

        Ok(())
    }

    pub fn multicast_loop_v4(&self) -> Result<bool> {
        Ok(false)
    }

    pub fn set_multicast_loop_v4(&self, on: bool) -> Result<()> {
        if chaos::should_fail(ChaosOperation::UdpMulticast) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Set multicast loop failed (chaos)"
            ));
        }

        Ok(())
    }

    pub fn multicast_ttl_v4(&self) -> Result<u32> {
        Ok(1)
    }

    pub fn set_multicast_ttl_v4(&self, ttl: u32) -> Result<()> {
        if chaos::should_fail(ChaosOperation::UdpMulticast) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Set multicast TTL failed (chaos)"
            ));
        }

        Ok(())
    }

    pub fn join_multicast_v4(&self, multiaddr: std::net::Ipv4Addr, interface: std::net::Ipv4Addr) -> Result<()> {
        if chaos::should_fail(ChaosOperation::UdpJoinMulticast) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
            "fracture: Join multicast failed (chaos)"
            ));
        }

        Ok(())
    }

    pub fn leave_multicast_v4(&self, multiaddr: std::net::Ipv4Addr, interface: std::net::Ipv4Addr) -> Result<()> {
        if chaos::should_fail(ChaosOperation::UdpLeaveMulticast) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Leave multicast failed"
            ));
        }

        Ok(())
    }

    pub fn ttl(&self) -> Result<u32> {
        Ok(64)
    }

    pub fn set_ttl(&self, ttl: u32) -> Result<()> {
        if chaos::should_fail(ChaosOperation::UdpSetTtl) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Set TTL failed (chaos)"
            ));
        }

        Ok(())
    }
}

pub struct Interest {
    readable: bool,
    writeable: bool
}

impl Interest {
    pub const READABLE: Interest = Interest { readable: true, writeable: false };
    pub const WRITABLE: Interest = Interest { readable: false, writeable: true };

    pub fn is_readable(&self) -> bool {
        self.readable
    }

    pub fn is_writable(&self) -> bool {
        self.writeable
    }
}

pub struct Ready {
    value: u8
}

impl Ready {
    pub const READABLE: Ready = Ready { value: 1 };
    pub const WRITABLE: Ready = Ready { value: 2 };
}

impl BitOr for Ready {
    type Output = Ready;

    fn bitor(self, rhs: Self) -> Ready {
        Ready {
            value: self.value | rhs.value
        }
    }
}

pub struct DnsResolver {
    cache: Arc<RwLock<HashMap<String, Vec<IpAddr>>>>,
    config: DnsConfig
}

#[derive(Clone)]
pub struct DnsConfig {
    pub cache_ttl: Duration,
    pub timeout: Duration,
    pub max_cache_entries: usize,
    pub enable_ipv6: bool
}

impl Default for DnsConfig {
    fn default() -> Self {
        Self {
            cache_ttl: Duration::from_secs(300),
            timeout: Duration::from_secs(5),
            max_cache_entries: 1000,
            enable_ipv6: true,
        }
    }
}

impl DnsResolver {
    pub fn new() -> Self {
        Self {
            cache: Arc::new(RwLock::new(HashMap::new())),
            config: DnsConfig::default(),
        }
    }

    pub fn with_config(config: DnsConfig) -> Self {
        Self {
            cache: Arc::new(RwLock::new(HashMap::new())),
            config,
        }
    }

    pub async fn lookup_host(&self, host: impl AsRef<str>) -> Result<Vec<SocketAddr>> {
        let host = host.as_ref();

        if chaos::should_fail(ChaosOperation::DnsFailure) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: DNS resolution failed (chaos)",
            ));
        }

        if chaos::should_fail(ChaosOperation::DnsLookup) {
            sleep(self.config.timeout * 2).await;
            return Err(io::Error::new(
                io::ErrorKind::TimedOut,
                "fracture: DNS timeout (chaos)",
            ));
        }

        {
            let cache = self.cache.read();
            if let Some(addrs) = cache.get(host) {
                return Ok(addrs.iter().map(|ip| SocketAddr::new(*ip, 0)).collect());
            }
        }

        let resolved_addrs = self.resolve_internal(host).await?;

        {
            let mut cache = self.cache.write();
            if cache.len() >= self.config.max_cache_entries {
                if let Some(key) = cache.keys().next().cloned() {
                    cache.remove(&key);
                }
            }
            cache.insert(host.to_string(), resolved_addrs.clone());
        }

        Ok(resolved_addrs.iter().map(|ip| SocketAddr::new(*ip, 0)).collect())
    }

    async fn resolve_internal(&self, host: &str) -> Result<Vec<IpAddr>> {
        sleep(Duration::from_millis(10)).await;

        if let Ok(ip) = host.parse::<IpAddr>() {
            return Ok(vec![ip]);
        }

        let hash = host.bytes().fold(0u32, |acc, b| acc.wrapping_mul(31).wrapping_add(b as u32));

        let ipv4 = IpAddr::V4(Ipv4Addr::new(
            127,
            ((hash >> 16) & 0xFF) as u8,
            ((hash >> 8) & 0xFF) as u8,
            (hash & 0xFF) as u8
        ));

        let mut addrs = vec![ipv4];

        if self.config.enable_ipv6 {
            let ipv6 = IpAddr::V6(Ipv6Addr::new(
                0x2001,
                0xdb8,
                ((hash >> 16) & 0xFFFF) as u16,
                (hash & 0xFFFF) as u16,
                0, 0, 0, 1
            ));
            addrs.push(ipv6);
        }

        Ok(addrs)
    }

    pub async fn lookup_ip(&self, ip: IpAddr) -> Result<String> {
        if chaos::should_fail(ChaosOperation::DnsReverseLookup) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Reverse DNS failed (chaos)",
            ));
        }

        sleep(Duration::from_millis(5)).await;

        Ok(format!("host-{}.example.com", ip))
    }

    pub fn clear_cache(&self) {
        self.cache.write().clear();
    }
}

impl Default for DnsResolver {
    fn default() -> Self {
        Self::new()
    }
}

static DNS_RESOLVER: std::sync::LazyLock<DnsResolver> = std::sync::LazyLock::new(DnsResolver::new);

pub async fn lookup_host(host: impl AsRef<str>) -> Result<Vec<SocketAddr>> {
    DNS_RESOLVER.lookup_host(host).await
}

#[derive(Clone, Debug)]
pub struct TcpSocketOptions {
    pub nodelay: bool,
    pub keepalive: Option<Duration>,
    pub linger: Option<Duration>,
    pub ttl: u32,
    pub recv_buffer_size: Option<usize>,
    pub send_buffer_size: Option<usize>,
    pub read_timeout: Option<Duration>,
    pub write_timeout: Option<Duration>,
}

impl Default for TcpSocketOptions {
    fn default() -> Self {
        Self {
            nodelay: false,
            keepalive: None,
            linger: None,
            ttl: 64,
            recv_buffer_size: None,
            send_buffer_size: None,
            read_timeout: None,
            write_timeout: None,
        }
    }
}

pub struct TcpStreamState {
    pub local_addr: SocketAddr,
    pub peer_addr: SocketAddr,
    pub options: TcpSocketOptions,
    pub read_buffer: Vec<u8>,
    pub write_buffer: Vec<u8>,
    pub half_closed: bool,
    pub fully_closed: bool,
    pub bytes_read: u64,
    pub bytes_written: u64,
    pub connection_time: tokio::time::Instant,
}

impl TcpStreamState {
    pub fn new(local_addr: SocketAddr, peer_addr: SocketAddr) -> Self {
        Self {
            local_addr,
            peer_addr,
            options: TcpSocketOptions::default(),
            read_buffer: Vec::new(),
            write_buffer: Vec::new(),
            half_closed: false,
            fully_closed: false,
            bytes_read: 0,
            bytes_written: 0,
            connection_time: tokio::time::Instant::now(),
        }
    }

    pub fn uptime(&self) -> Duration {
        self.connection_time.elapsed()
    }
}

#[derive(Clone, Debug)]
pub struct UdpSocketOptions {
    pub broadcast: bool,
    pub multicast_loop_v4: bool,
    pub multicast_loop_v6: bool,
    pub multicast_ttl_v4: u32,
    pub ttl: u32,
    pub recv_buffer_size: Option<usize>,
    pub send_buffer_size: Option<usize>,
    pub read_timeout: Option<Duration>,
    pub write_timeout: Option<Duration>,
}

impl Default for UdpSocketOptions {
    fn default() -> Self {
        Self {
            broadcast: false,
            multicast_loop_v4: true,
            multicast_loop_v6: true,
            multicast_ttl_v4: 1,
            ttl: 64,
            recv_buffer_size: None,
            send_buffer_size: None,
            read_timeout: None,
            write_timeout: None,
        }
    }
}

pub struct MulticastGroup {
    pub multicast_addr: IpAddr,
    pub interface: IpAddr
}

pub struct MulticastManager {
    joined_groups: Arc<StdMutex<Vec<MulticastGroup>>>
}

impl MulticastManager {
    pub fn new() -> Self {
        Self { joined_groups: Arc::new(StdMutex::new(Vec::new())) }
    }

    pub fn join_multicast_v4(&self, multicast_addr: Ipv4Addr, interface: Ipv4Addr) -> Result<()> {
        if chaos::should_fail(ChaosOperation::UdpJoinMulticast) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Join multicast failed (chaos)",
            ));
        }

        let mut groups = self.joined_groups.lock().unwrap();
        groups.push(MulticastGroup {
            multicast_addr: IpAddr::V4(multicast_addr),
            interface: IpAddr::V4(interface)
        });

        Ok(())
    }

    pub fn join_multicast_v6(&self, multicast_addr: &Ipv6Addr, interface: u32) -> Result<()> {
        if chaos::should_fail(ChaosOperation::UdpJoinMulticast) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Join multicast v6 failed (chaos)",
            ));
        }

        let interface_ip = IpAddr::V6(Ipv6Addr::UNSPECIFIED);

        let mut groups = self.joined_groups.lock().unwrap();
        groups.push(MulticastGroup {
            multicast_addr: IpAddr::V6(*multicast_addr),
            interface: interface_ip
        });

        Ok(())
    }

    pub fn leave_multicast_v4(&self, multicast_addr: Ipv4Addr, interface: Ipv4Addr) -> Result<()> {
        if chaos::should_fail(ChaosOperation::UdpLeaveMulticast) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Leave multicast failed (chaos)",
            ));
        }

        let mut groups = self.joined_groups.lock().unwrap();
        groups.retain(|g| {
            !(g.multicast_addr == IpAddr::V4(multicast_addr) && g.interface == IpAddr::V4(interface))
        });

        Ok(())
    }

    pub fn leave_multicast_v6(&self, multicast_addr: &Ipv6Addr, interface: u32) -> Result<()> {
        if chaos::should_fail(ChaosOperation::UdpLeaveMulticast) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Leave multicast v6 failed (chaos)",
            ));
        }

        let mut groups = self.joined_groups.lock().unwrap();
        groups.retain(|g| g.multicast_addr != IpAddr::V6(*multicast_addr));

        Ok(())
    }

    pub fn is_member(&self, multicast_addr: IpAddr) -> bool {
        let groups = self.joined_groups.lock().unwrap();
        groups.iter().any(|g| g.multicast_addr == multicast_addr)
    }

    pub fn list_groups(&self) -> Vec<MulticastGroup> {
        let groups = self.joined_groups.lock().unwrap();
        groups.iter().map(|g| MulticastGroup {
            multicast_addr: g.multicast_addr,
            interface: g.interface
        }).collect()
    }
}

impl Default for MulticastManager {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, Default)]
pub struct SocketStats {
    pub packets_sent: u64,
    pub packets_received: u64,
    pub bytes_sent: u64,
    pub bytes_received: u64,
    pub errors: u64,
    pub timeouts: u64,
    pub retransmissions: u64,
}

impl SocketStats {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn record_send(&mut self, bytes: usize) {
        self.packets_sent += 1;
        self.bytes_sent += bytes as u64;
    }

    pub fn record_receive(&mut self, bytes: usize) {
        self.packets_received += 1;
        self.bytes_received += bytes as u64;
    }

    pub fn record_error(&mut self) {
        self.errors += 1;
    }

    pub fn record_timeout(&mut self) {
        self.timeouts += 1;
    }

    pub fn record_retransmission(&mut self) {
        self.retransmissions += 1;
    }
}

pub struct ConnectionPool {
    connections: Arc<RwLock<HashMap<SocketAddr, Vec<ConnectionSlot>>>>,
    config: PoolConfig
}

#[derive(Clone)]
pub struct PoolConfig {
    pub max_idle_per_host: usize,
    pub idle_timeout: Duration,
    pub max_lifetime: Duration
}

impl Default for PoolConfig {
    fn default() -> Self {
        Self {
            max_idle_per_host: 5,
            idle_timeout: Duration::from_secs(90),
            max_lifetime: Duration::from_secs(600),
        }
    }
}

struct ConnectionSlot {
    created_at: tokio::time::Instant,
    last_used: tokio::time::Instant,
    // Need to add actual connection
}

impl ConnectionPool {
    pub fn new() -> Self {
        Self {
            connections: Arc::new(RwLock::new(HashMap::new())),
            config: PoolConfig::default(),
        }
    }

    pub fn with_config(config: PoolConfig) -> Self {
        Self {
            connections: Arc::new(RwLock::new(HashMap::new())),
            config,
        }
    }

    pub async fn get_or_connect(&self, addr: SocketAddr) -> Result<()> {
        let mut conns = self.connections.write();

        let slots = conns.entry(addr).or_insert_with(Vec::new);

        let now = tokio::time::Instant::now();
        slots.retain(|slot| {
            now.duration_since(slot.created_at) < self.config.max_lifetime
                && now.duration_since(slot.last_used) < self.config.idle_timeout
        });

        if let Some(slot) = slots.pop() {
            Ok(())
        }
        else {
            let slot = ConnectionSlot {
                created_at: now,
                last_used: now
            };
            slots.push(slot);

            Ok(())
        }
    }

    pub fn return_connection(&self, addr: SocketAddr) {
        let mut conns = self.connections.write();
        if let Some(slots) = conns.get_mut(&addr) {
            if slots.len() < self.config.max_idle_per_host {
                let now = Instant::now();
                slots.push(ConnectionSlot {
                    created_at: now,
                    last_used: now
                });
            }
        }
    }

    pub fn clear(&self) {
        self.connections.write().clear();
    }

    pub fn stats(&self) -> HashMap<SocketAddr, usize> {
        let conns = self.connections.read();
        conns.iter().map(|(addr, slots)| (*addr, slots.len())).collect()
    }
}

#[cfg(unix)]
pub mod unix {
    use crate::time::sleep;

    use super::*;
    use std::path::{Path, PathBuf};

    #[derive(Debug, Clone)]
    pub struct UnixSocketAddr {
        pub path: PathBuf
    }

    impl UnixSocketAddr {
        pub fn new(path: impl Into<PathBuf>) -> Self {
            Self {
                path: path.into()
            }
        }

        pub fn as_pathname(&self) -> Option<&Path> {
            Some(&self.path)
        }

        pub fn is_unnamed(&self) -> bool {
            false
        }
    }

    pub struct UnixStream {
        local_addr: UnixSocketAddr,
        peer_addr: UnixSocketAddr
    }

    impl UnixStream {
        pub async fn connect<P: AsRef<Path>>(path: P) -> Result<Self> {
            if chaos::should_fail(ChaosOperation::UnixConnect) {
                return Err(io::Error::new(
                    io::ErrorKind::ConnectionRefused,
                    "fracture: Unix connect failed (chaos)",
                ));
            }

            let peer_addr = UnixSocketAddr::new(path.as_ref());
            let local_addr = UnixSocketAddr::new(format!("/tmp/fracture-{}.sock", rand::random::<u64>()));

            sleep(Duration::from_millis(1)).await;

            Ok(Self { local_addr, peer_addr })
        }

        pub async fn pair() -> Result<(Self, Self)> {
            let addr1 = UnixSocketAddr::new("/tmp/fracture-pair-1.sock");
            let addr2 = UnixSocketAddr::new("/tmp/fracture-pair-2.sock");

            let stream1 = Self {
                local_addr: addr1.clone(),
                peer_addr: addr2.clone()
            };

            let stream2 = Self {
                local_addr: addr2,
                peer_addr: addr1
            };

            Ok((stream1, stream2))
        }

        pub fn local_addr(&self) -> Result<UnixSocketAddr> {
            Ok(self.local_addr.clone())
        }

        pub fn peer_addr(&self) -> Result<UnixSocketAddr> {
            Ok(self.peer_addr.clone())
        }

        pub async fn recv(&self, buf: &mut [u8]) -> Result<usize> {
            if chaos::should_fail(ChaosOperation::UnixRecv) {
                return Err(io::Error::new(
                    io::ErrorKind::BrokenPipe,
                    "fracture: Unix recv failed (chaos)",
                ));
            }

            Ok(0)
        }
    }

    pub struct UnixListener {
        local_addr: UnixSocketAddr
    }

    impl UnixListener {
        pub async fn bind<P: AsRef<Path>>(path: P) -> Result<Self> {
            if chaos::should_fail(ChaosOperation::UnixBind) {
                return Err(io::Error::new(
                    io::ErrorKind::AddrInUse,
                    "fracture: Unix bind failed (chaos)",
                ));
            }

            Ok(Self { local_addr: UnixSocketAddr::new(path.as_ref()) })
        }

        pub async fn accept(&self) -> Result<(UnixStream, UnixSocketAddr)> {
            if chaos::should_fail(ChaosOperation::UnixAccept) {
                return Err(io::Error::new(
                    io::ErrorKind::WouldBlock,
                    "fracture: Unix accept failed (chaos)",
                ));
            }

            let peer_addr = UnixSocketAddr::new(format!("/tmp/fracture-client-{}.sock", rand::random::<u64>()));
            let stream = UnixStream {
                local_addr: self.local_addr.clone(),
                peer_addr: peer_addr.clone()
            };

            Ok((stream, peer_addr))
        }

        pub fn local_addr(&self) -> Result<UnixSocketAddr> {
            Ok(self.local_addr.clone())
        }
    }

    pub struct UnixDatagram {
        local_addr: UnixSocketAddr
    }

    impl UnixDatagram {
        pub async fn bind<P: AsRef<Path>>(path: P) -> Result<Self> {
            if chaos::should_fail(ChaosOperation::UdpBind) {
                return Err(io::Error::new(
                    io::ErrorKind::AddrInUse,
                    "fracture: Unix datagram bind failed (chaos)",
                ));
            }

            Ok(Self { local_addr: UnixSocketAddr::new(path.as_ref()) })
        }

        pub async fn send_to<P: AsRef<Path>>(&self, buf: &[u8], target: P) -> Result<usize> {
            if chaos::should_fail(ChaosOperation::UnixDatagramSend) {
                return Err(io::Error::new(
                    io::ErrorKind::BrokenPipe,
                    "fracture: Unix datagram send failed (chaos)",
                ));
            }

            Ok(buf.len())
        }

        pub async fn recv_from(&self, buf: &[u8]) -> Result<(usize, UnixSocketAddr)> {
            if chaos::should_fail(ChaosOperation::UnixDatagramRecv) {
                return Err(io::Error::new(
                    io::ErrorKind::WouldBlock,
                    "fracture: Unix datagram recv failed (chaos)",
                ));
            }

            let peer_addr = UnixSocketAddr::new("/tmp/fracture-peer.sock");
            Ok((0, peer_addr))
        }

        pub fn local_addr(&self) -> Result<UnixSocketAddr> {
            Ok(self.local_addr.clone())
        }
    }
}