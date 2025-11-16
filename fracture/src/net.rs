use turmoil;
use std::io::{self, Result};
use std::net::{SocketAddr, ToSocketAddrs, Shutdown};
use std::pin::Pin;
use std::task::{Context, Poll};
use std::time::Duration;
use std::ops::BitOr;
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt, ReadBuf};
use bytes::{Buf, BufMut};

use crate::chaos::{self, ChaosOperation};

pub struct TcpStream {
    inner: turmoil::net::TcpStream,
    local_addr: SocketAddr,
    peer_addr: SocketAddr,
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

        let addr = addr.to_socket_addrs()?.next()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "fracture: No addresses"))?;

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

    pub fn no_delay(&self) -> Result<bool> {
        Ok(true)
    }

    pub fn set_nodelay(&self, nodelay: bool) -> Result<()> {
        if chaos::should_fail(ChaosOperation::TcpSetNodelay) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Set nodelay failed (chaos)"
            ));
        }

        Ok(())
    }

    pub fn set_ttl(&self, ttl: u32) -> Result<()> {
        if chaos::should_fail(ChaosOperation::TcpSetTtl) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Set TTL failed (chaos)"
            ));
        }

        Ok(())
    }

    pub fn linger(&self) -> Result<Option<Duration>> {
        Ok(None)
    }

    pub fn set_linger(&self, linger: Option<Duration>) -> Result<()> {
        if chaos::should_fail(ChaosOperation::TcpSetLinger) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fracture: Set linger failed (chaos)"
            ));
        }

        Ok(())
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

        Pin::new(&mut self.inner).poll_read(cx, buf)
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
                return Pin::new(&mut self.inner).poll_write(cx, &buf[..partial]);
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
            return Pin::new(&mut self.inner).poll_write(cx, limited_buf);
        }

        chaos::trace::record(chaos::trace::TraceEvent::DataSent{
            from: self.local_addr.to_string(),
            to: self.peer_addr.to_string(),
            bytes: buf.len()
        });

        Pin::new(&mut self.inner).poll_write(cx, buf)
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
        Pin::new(&mut self.inner).poll_shutdown(cx)
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

        Pin::new(&mut self.inner).poll_write_vectored(cx, bufs)
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

// Unix domain sockets (partial implementation)
pub mod unix {
    use super::*;

    pub struct UnixStream;
    pub struct UnixListener;
    pub struct UnixDatagram;

    impl UnixStream {
        pub async fn connect<P: AsRef<std::path::Path>>(path: P) -> Result<Self> {
            if chaos::should_fail(ChaosOperation::UnixConnect) {
                return Err(io::Error::new(
                    io::ErrorKind::ConnectionRefused,
                    "fracture: Unix connect failed (chaos)"
                ));
            }

            Ok(UnixStream)
        }
    }

    impl UnixListener {
        pub async fn bind<P: AsRef<std::path::Path>>(path: P) -> Result<Self> {
            if chaos::should_fail(ChaosOperation::UnixBind) {
                return Err(io::Error::new(
                    io::ErrorKind::AddrInUse,
                    "fracture: Unix bind failed (chaos)"
                ));
            }

            Ok(UnixListener)
        }

        pub async fn accept(&self) -> Result<(UnixStream, std::net::SocketAddr)> {
            if chaos::should_fail(ChaosOperation::UnixAccept) {
                return Err(io::Error::new(
                    io::ErrorKind::BrokenPipe,
                    "fracture: Unix accept failed (chaos)"
                ));
            }

            Err(io::Error::new(io::ErrorKind::Other, "fracture: Not implemented"))
        }
    }
}