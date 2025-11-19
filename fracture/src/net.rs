use core::fmt;
use std::collections::{HashMap, VecDeque};
use std::net::SocketAddr;
use std::sync::{Arc, Mutex};
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll, Waker};
use std::io::{Result, Error, ErrorKind};
use bytes::{Buf, BufMut, Bytes, BytesMut};
use futures::TryFutureExt;
use crate::io::{AsyncRead, AsyncWrite, ReadBuf};
use crate::runtime::Handle;

const DEFAULT_TCP_WINDOW_SIZE: usize = 65535;

pub(crate) struct NetworkState {
    listeners: HashMap<SocketAddr, Sender<TcpConnection>>
}

impl NetworkState {
    pub fn new() -> Self {
        Self { listeners: HashMap::new() }
    }
}

pub struct TcpConnection {
    pub remote_addr: SocketAddr,
    pub tx: Sender<Bytes>,
    pub rx: AsyncReceiver<Bytes>
}

pub struct TcpListener {
    local_addr: SocketAddr,
    accept_rx: AsyncReceiver<TcpConnection>
}

impl TcpListener {
    pub async fn bind(addr: SocketAddr) -> Result<Self> {
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
}

pub struct TcpStream {
    tx: Sender<Bytes>,
    rx: AsyncReceiver<Bytes>,
    read_buffer: BytesMut,
    local_addr: SocketAddr,
    peer_addr: SocketAddr
}

impl TcpStream {
    fn new(tx: Sender<Bytes>, rx: AsyncReceiver<Bytes>, local_addr: SocketAddr, peer_addr: SocketAddr) -> Self {
        Self {
            tx,
            rx,
            read_buffer: BytesMut::new(),
            local_addr,
            peer_addr
        }
    }

    pub async fn connect(addr: SocketAddr) -> Result<Self> {
        let handle = Handle::current();
        let core_rc = handle.core.upgrade().ok_or_else(|| Error::new(ErrorKind::Other, "fracture: Runtime dropped"))?;

        let listener_tx = {
            let core = core_rc.borrow();
            core.network.listeners.get(&addr).cloned()
        };

        let listener_tx = listener_tx.ok_or_else(|| Error::new(ErrorKind::ConnectionRefused, "fracture: Connection refused"))?;

        let (tx1, rx1) = channel(DEFAULT_TCP_WINDOW_SIZE);
        let (tx2, rx2) = channel(DEFAULT_TCP_WINDOW_SIZE);

        let local_addr = SocketAddr::from(([127, 0, 0, 1], 10000 + (rand::random::<u16>() % 50000)));

        let server_conn = TcpConnection {
            remote_addr: local_addr,
            tx: tx2,
            rx: rx1
        };

        listener_tx.send(server_conn).map_err(|_| Error::new(ErrorKind::ConnectionRefused, "fracture: Remote backlog full"))?;

        Ok(Self {
            tx: tx1,
            rx: rx2,
            read_buffer: BytesMut::new(),
            local_addr,
            peer_addr: addr
        })
    }

    pub fn local_addr(&self) -> Result<SocketAddr> {
        Ok(self.local_addr)
    }

    pub fn peer_addr(&self) -> Result<SocketAddr> {
        Ok(self.peer_addr)
    }
}

impl AsyncRead for TcpStream {
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        mut buf: &mut ReadBuf<'_>
    ) -> Poll<Result<()>> {
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
        if let Poll::Pending = self.tx.poll_reserve(cx, buf.len()) {
            return Poll::Pending;
        }

        let bytes = Bytes::copy_from_slice(buf);

        match self.tx.send(bytes) {
            Ok(_) => Poll::Ready(Ok(buf.len())),
            Err(TrySendError::Closed(_)) => Poll::Ready(Err(Error::new(ErrorKind::BrokenPipe, "fracture: Connection reset"))),
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

struct ChannelState<T> {
    queue: VecDeque<T>,
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

        if let Some(item) = s.queue.pop_front() {
            s.current_size -= item.remaining();

            if let Some(w) = s.send_waiters.pop_front() {
                w.wake();
            }

            Poll::Ready(Some(item))
        }
        else if s.closed {
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