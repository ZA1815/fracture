#[cfg(feature = "simulation")]
use turmoil;
use std::io::{self, Result};
use std::net::{SocketAddr, ToSocketAddrs};
use std::pin::Pin;
use std::task::{Context, Poll};
use tokio::io::{AsyncRead, AsyncWrite, ReadBuf};

#[cfg(feature = "simulation")]
pub struct TcpStream {
    inner: turmoil::net::TcpStream,
    local_addr: SocketAddr,
    peer_addr: SocketAddr
}

#[cfg(feature = "simulation")]
impl TcpStream {
    pub async fn connect<A: ToSocketAddrs>(addr: A) -> Result<Self> {
        if crate::chaos::should_fail("tcp_connect") {
            return Err(io::Error::new(
                io::ErrorKind::ConnectionRefused,
                "fracture: Connection refused (chaos)"
            ));
        }

        let addr = addr.to_socket_addrs()?.next().ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "fracture: No addresses"))?;

        if let Ok(hostname) = std::env::var("FRACTURE_HOSTNAME") {
            let target = addr.ip().to_string();
            if crate::chaos::is_partitioned(&hostname, &target) {
                return Err(io::Error::new(
                    io::ErrorKind::NetworkUnreachable,
                    "fracture: Network partitioned"
                ));
            }
        }

        let inner = turmoil::net::TcpStream::connect(addr).await?;
        let local_addr = inner.local_addr()?;
        let peer_addr = inner.peer_addr()?;

        Ok(Self { inner, local_addr, peer_addr })
    }

    pub fn local_addr(&self) -> Result<SocketAddr> {
        Ok(self.local_addr)
    }

    pub fn peer_addr(&self) -> Result<SocketAddr> {
        Ok(self.peer_addr)
    }

    pub fn poll_read_ready(&self, cx: &mut Context<'_>) -> Poll<Result<()>> {
        if crate::chaos::should_fail("tcp_read") {
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
        if crate::chaos::should_fail("tcp_write") {
            return Poll::Ready(Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: Write failed (chaos)"
            )));
        }
        else {
            Poll::Ready(Ok(()))
        }
    }
}

#[cfg(feature = "simulation")]
impl AsyncRead for TcpStream {
    fn poll_read(mut self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &mut ReadBuf<'_>) -> Poll<Result<()>> {
        if crate::chaos::should_fail("tcp_read_bytes") {
            return Poll::Ready(Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: Read failed (chaos)"
            )));
        }

        Pin::new(&mut self.inner).poll_read(cx, buf)
    }
}

#[cfg(feature = "simulation")]
impl AsyncWrite for TcpStream {
    fn poll_write(mut self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &[u8]) -> Poll<Result<usize>> {
        if crate::chaos::should_fail("tcp_write_bytes") {
            return Poll::Ready(Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: Write failed (chaos)"
            )));
        }

        if crate::chaos::should_fail("tcp_partial_write") {
            let partial = buf.len() / 2;
            if partial > 0 {
                return Pin::new(&mut self.inner).poll_write(cx, &buf[..partial]);
            }
        }

        Pin::new(&mut self.inner).poll_write(cx, buf)
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>> {
        Pin::new(&mut self.inner).poll_flush(cx)
    }

    fn poll_shutdown(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>> {
        Pin::new(&mut self.inner).poll_shutdown(cx)
    }
}

#[cfg(feature = "simulation")]
pub struct TcpListener {
    inner: turmoil::net::TcpListener,
    local_addr: SocketAddr
}

#[cfg(feature = "simulation")]
impl TcpListener {
    pub async fn bind<A: ToSocketAddrs>(addr: A) -> Result<Self> {
        let addr = addr.to_socket_addrs()?.next().ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "fracture: No addresses"))?;

        let inner = turmoil::net::TcpListener::bind(addr).await?;
        let local_addr = inner.local_addr()?;

        Ok(Self { inner, local_addr })
    }

    pub async fn accept(&self) -> Result<(TcpStream, SocketAddr)> {
        if crate::chaos::should_fail("tcp_accept") {
            return Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: Accept failed (chaos)"
            ));
        }

        let (stream, addr) = self.inner.accept().await?;
        let local_addr = stream.local_addr()?;
        let peer_addr = stream.peer_addr()?;

        Ok((
            TcpStream {
                inner: stream,
                local_addr,
                peer_addr
            },
            addr
        ))
    }

    pub fn local_addr(&self) -> Result<SocketAddr> {
        Ok(self.local_addr)
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
        let addr = addr.to_socket_addrs()?.next().ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "fracture: No addresses"))?;
        let inner = turmoil::net::UdpSocket::bind(addr).await?;
        let local_addr = inner.local_addr()?;
        Ok(Self { inner, local_addr })
    }

    pub fn local_addr(&self) -> io::Result<SocketAddr> {
        Ok(self.local_addr)
    }

    pub async fn send_to(&self, buf: &[u8], target: SocketAddr) -> io::Result<usize> {
        if crate::chaos::should_fail("udp_send") {
            return Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: UDP send failed (chaos)"
            ));
        }
        self.inner.send_to(buf, target).await
    }

    pub async fn recv_from(&self, buf: &mut [u8]) -> io::Result<(usize, SocketAddr)> {
        if crate::chaos::should_fail("udp_recv") {
            return Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "fracture: UDP recv failed (chaos)"
            ));
        }
        self.inner.recv_from(buf).await
    }
}


#[cfg(not(feature = "simulation"))]
pub use tokio::net::{TcpStream, TcpListener, UdpSocket};