use std::ops::{Deref, DerefMut};
use std::pin::Pin;
use std::task::{Context, Poll};
use std::time::Duration;
use std::sync::Arc;
use tokio::sync;

use crate::chaos::{self, ChaosOperation};

pub struct Mutex<T> {
    inner: Arc<sync::Mutex<T>>,
    chaos_state: Arc<std::sync::Mutex<MutexChaosState>>
}

#[derive(Default)]
struct MutexChaosState {
    force_deadlock: bool,
    starve_next_lock: bool,
    poison_on_unlock: bool,
    lock_holders: Vec<std::thread::ThreadId>
}

pub struct MutexGuard<'a, T> {
    inner: Option<sync::MutexGuard<'a, T>>,
    chaos_state: Arc<std::sync::Mutex<MutexChaosState>>
}

impl<T> Mutex<T> {
    pub fn new(value: T) -> Self {
        Self {
            inner: Arc::new(sync::Mutex::new(value)),
            chaos_state: Arc::new(std::sync::Mutex::new(MutexChaosState::default()))
        }
    }

    pub async fn lock(&self) -> MutexGuard<'_, T> {
        if chaos::should_fail(ChaosOperation::MutexLock) {
            std::future::pending::<()>().await;
        }

        if chaos::should_fail(ChaosOperation::MutexDeadlock) {
            let thread_id = std::thread::current().id();
            if let Ok(mut state) = self.chaos_state.lock() {
                if state.lock_holders.contains(&thread_id) {
                    std::future::pending::<()>().await;
                }
                state.lock_holders.push(thread_id);
            }
        }

        if chaos::should_fail(ChaosOperation::MutexStarvation) {
            let delay = Duration::from_millis(rand::random::<u64>() & 1000);
            tokio::time::sleep(delay).await;
        }

        if chaos::should_fail(ChaosOperation::MutexPoisoned) {
            panic!("fracture: Mutex poisoned (chaos)");
        }

        let guard = self.inner.lock().await;
        MutexGuard {
            inner: Some(guard),
            chaos_state: Arc::clone(&self.chaos_state)
        }
    }

    pub fn try_lock(&self) -> Result<MutexGuard<'_, T>, TryLockError> {
        if chaos::should_fail(ChaosOperation::MutexTryLock) {
            return Err(TryLockError::WouldBlock)
        }

        self.inner.try_lock().map(|guard| MutexGuard {
            inner: Some(guard),
            chaos_state: Arc::clone(&self.chaos_state)
        })
        .map_err(|_| TryLockError::WouldBlock)
    }

    pub async fn lock_owned(self: Arc<Self>) -> OwnedMutexGuard<T> {
        if chaos::should_fail(ChaosOperation::MutexLock) {
            std::future::pending::<()>().await;
        }

        let guard = Arc::clone(&self.inner).lock_owned().await;
        OwnedMutexGuard {
            inner: Some(guard),
            chaos_state: Arc::clone(&self.chaos_state)
        }
    }

    pub fn blocking_lock(&self) -> MutexGuard<'_, T> {
        let guard = self.inner.blocking_lock();
        MutexGuard {
            inner: Some(guard),
            chaos_state: Arc::clone(&self.chaos_state)
        }
    }
}

impl <'a, T> Drop for MutexGuard<'a, T> {
    fn drop(&mut self) {
        if let Ok(mut state) = self.chaos_state.lock() {
            let thread_id = std::thread::current().id();
            state.lock_holders.retain(|&id| id != thread_id);

            if state.poison_on_unlock {
                // Placeholder
            }
        }
    }
}

impl<'a, T> std::ops::Deref for MutexGuard<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.inner.as_ref().unwrap()
    }
}

impl<'a, T> std::ops::DerefMut for MutexGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner.as_mut().unwrap()
    }
}

pub struct OwnedMutexGuard<T> {
    inner: Option<sync::OwnedMutexGuard<T>>,
    chaos_state: Arc<std::sync::Mutex<MutexChaosState>>
}

impl<T> Deref for OwnedMutexGuard<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner.as_ref().unwrap()
    }
}

impl<T> DerefMut for OwnedMutexGuard<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner.as_mut().unwrap()
    }
}

impl<T> Drop for OwnedMutexGuard<T> {
    fn drop(&mut self) {
        if let Ok(mut state) = self.chaos_state.lock() {
            let thread_id = std::thread::current().id();
            state.lock_holders.retain(|&id| id != thread_id);
        }
    }
}

#[derive(Debug)]
pub enum TryLockError {
    Poisoned,
    WouldBlock
}

pub struct RwLock<T> {
    inner: sync::RwLock<T>,
    chaos_state: Arc<std::sync::Mutex<RwLockChaosState>>
}

#[derive(Default)]
struct RwLockChaosState {
    reader_count: usize,
    has_writer: bool,
    starve_readers: bool,
    starve_writers: bool
}

impl<T> RwLock<T> {
    pub fn new(value: T) -> Self {
        Self {
            inner: sync::RwLock::new(value),
            chaos_state: Arc::new(std::sync::Mutex::new(RwLockChaosState::default()))
        }
    }

    pub async fn read(&self) -> RwLockReadGuard<'_, T> {
        if chaos::should_fail(ChaosOperation::RwLockRead) {
            std::future::pending::<()>().await;
        }

        if chaos::should_fail(ChaosOperation::RwLockReaderStarvation) {
            tokio::time::sleep(Duration::from_millis(rand::random::<u64>() % 500)).await;
        }

        if chaos::should_fail(ChaosOperation::RwLockDeadlock) {
            if let Ok(state) = self.chaos_state.lock() {
                if state.has_writer {
                    std::future::pending::<()>().await;
                }
            }
        }

        let guard = self.inner.read().await;

        if let Ok(mut state) = self.chaos_state.lock() {
            state.reader_count += 1;
        }

        RwLockReadGuard {
            inner: Some(guard),
            chaos_state: Arc::clone(&self.chaos_state)
        }
    }

    pub async fn write(&self) -> RwLockWriteGuard<'_, T> {
        if chaos::should_fail(ChaosOperation::RwLockWrite) {
            std::future::pending::<()>().await;
        }

        if chaos::should_fail(ChaosOperation::RwLockWriterStarvation) {
            tokio::time::sleep(Duration::from_millis(rand::random::<u64>() % 1000)).await;
        }

        if chaos::should_fail(ChaosOperation::RwLockDeadlock) {
            if let Ok(state) = self.chaos_state.lock() {
                if state.has_writer || state.reader_count > 0 {
                    if rand::random::<bool>() {
                        std::future::pending::<()>().await;
                    }
                }
            }
        }

        let guard = self.inner.write().await;

        if let Ok(mut state) = self.chaos_state.lock() {
            state.has_writer = true;
        }

        RwLockWriteGuard {
            inner: Some(guard),
            chaos_state: Arc::clone(&self.chaos_state)
        }
    }

    pub fn try_read(&self) -> Result<RwLockReadGuard<'_, T>, TryLockError> {
        if chaos::should_fail(ChaosOperation::RwLockTryRead) {
            return Err(TryLockError::WouldBlock)
        }

        self.inner.try_read().map(|guard| RwLockReadGuard {
            inner: Some(guard),
            chaos_state: Arc::clone(&self.chaos_state)
        })
        .map_err(|_| TryLockError::WouldBlock)
    }

    pub fn try_write(&self) -> Result<RwLockWriteGuard<'_, T>, TryLockError> {
        if chaos::should_fail(ChaosOperation::RwLockTryWrite) {
            return Err(TryLockError::WouldBlock);
        }

        self.inner.try_write().map(|guard| RwLockWriteGuard {
            inner: Some(guard),
            chaos_state: Arc::clone(&self.chaos_state)
        })
        .map_err(|_| TryLockError::WouldBlock)
    }
}

pub struct RwLockReadGuard<'a, T> {
    inner: Option<sync::RwLockReadGuard<'a, T>>,
    chaos_state: Arc<std::sync::Mutex<RwLockChaosState>>
}

impl<'a, T> Drop for RwLockReadGuard<'a, T> {
    fn drop(&mut self) {
        if let Ok(mut state) = self.chaos_state.lock() {
            state.reader_count = state.reader_count.saturating_sub(1);
        }
    }
}

impl<'a, T> Deref for RwLockReadGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner.as_ref().unwrap().deref()
    }
}

impl<'a, T> RwLockReadGuard<'a, T> {
    pub fn map<U, F>(mut this: Self, f: F) -> RwLockMappedReadGuard<'a, T, U>
    where F: FnOnce(&T) -> &U, U: ?Sized {
        let data = f(&*this) as *const U;

        RwLockMappedReadGuard {
            _inner: this.inner.take().unwrap(),
            data,
            _phantom: std::marker::PhantomData
        }
    }

    pub fn try_map<U, F>(mut this: Self, f: F) -> Result<RwLockMappedReadGuard<'a, T, U>, Self>
    where F: FnOnce(&T) -> Option<&U>, U: ?Sized {
        match f(&*this) {
            Some(data) => {
                let data_ptr = data as *const U;
                Ok(RwLockMappedReadGuard {
                    _inner: this.inner.take().unwrap(),
                    data: data_ptr,
                    _phantom: std::marker::PhantomData
                })
            }
            None => Err(this)
        }
    }
}

pub struct RwLockWriteGuard<'a, T> {
    inner: Option<sync::RwLockWriteGuard<'a, T>>,
    chaos_state: Arc<std::sync::Mutex<RwLockChaosState>>
}

impl<'a, T> Drop for RwLockWriteGuard<'a, T> {
    fn drop(&mut self) {
        if let Ok(mut state) = self.chaos_state.lock() {
            state.has_writer = false;
        }
    }
}

impl<'a, T> Deref for RwLockWriteGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner.as_ref().unwrap().deref()
    }
}

impl<'a, T> RwLockWriteGuard<'a, T> {
    pub fn map<U, F>(mut this: Self, f: F) -> RwLockMappedWriteGuard<'a, T, U>
    where F: FnOnce(&mut T) -> &mut U, U: ?Sized {
        let data = f(&mut *const_cast_mut(&*this)) as *mut U;

        RwLockMappedWriteGuard {
            _inner: this.inner.take().unwrap(),
            data,
            _phantom: std::marker::PhantomData
        }
    }

    pub fn downgrade(mut this: Self) -> RwLockReadGuard<'a, T> {
        let inner = this.inner.take().unwrap();
        let downgraded = sync::RwLockWriteGuard::downgrade(inner);

        RwLockReadGuard {
            inner: Some(downgraded),
            chaos_state: this.chaos_state.clone()
        }
    }
}

fn const_cast_mut<T: ?Sized>(reference: &T) -> &mut T {
    unsafe { &mut *(reference as *const T as *mut T) }
}

pub struct Semaphore {
    inner: Arc<tokio::sync::Semaphore>,
    max_permits: usize
}

pub struct SemaphorePermit<'a> {
    inner: Option<tokio::sync::SemaphorePermit<'a>>
}

impl<'a> SemaphorePermit<'a> {
    pub fn forget(mut self) {
        if let Some(permit) = self.inner.take() {
            permit.forget();
        }
    }
}

pub struct OwnedSemaphorePermit {
    inner: Option<tokio::sync::OwnedSemaphorePermit>,
    _semaphore: Arc<Semaphore>
}

impl OwnedSemaphorePermit {
    pub fn forget(mut self) {
        if let Some(permit) = self.inner.take() {
            permit.forget();
        }
    }
}

impl Drop for OwnedSemaphorePermit {
    fn drop(&mut self) {
        
    }
}

impl Semaphore {
    pub fn new(permits: usize) -> Self {
        Self {
            inner: Arc::new(tokio::sync::Semaphore::new(permits)),
            max_permits: permits
        }
    }

    pub async fn acquire(&self) -> SemaphorePermit<'_> {
        if chaos::should_fail(ChaosOperation::SemaphoreAcquire) {
            std::future::pending::<()>().await;
        }

        if chaos::should_fail(ChaosOperation::SemaphoreExhaustion) {
            tokio::time::sleep(Duration::from_secs(10)).await;
        }

        if chaos::should_fail(ChaosOperation::SemaphoreAcquireTimeout) {
            tokio::time::sleep(Duration::from_secs(30)).await;
            panic!("fracture: Semaphore acquire timeout (chaos)")
        }

        let permit = self.inner.acquire().await.unwrap();
        SemaphorePermit {
            inner: Some(permit)
        }
    }

    pub fn try_acquire(&self) -> Result<SemaphorePermit<'_>, TryAcquireError> {
        if chaos::should_fail(ChaosOperation::SemaphoreTryAcquire) {
            return Err(TryAcquireError::NoPermits)
        }

        self.inner.try_acquire()
            .map(|permit| SemaphorePermit { inner: Some(permit)} )
            .map_err(|_| TryAcquireError::NoPermits)
    }

    pub async fn acquire_many(&self, n: u32) -> SemaphorePermit<'_> {
        if chaos::should_fail(ChaosOperation::SemaphoreAcquireMany) {
            std::future::pending::<()>().await;
        }

        let permit = self.inner.acquire_many(n).await.unwrap();
        SemaphorePermit {
            inner: Some(permit)
        }
    }

    pub fn available_permits(&self) -> usize {
        self.inner.available_permits()
    }

    pub fn add_permits(&self, n: usize) {
        if chaos::should_fail(ChaosOperation::SemaphoreRelease) {
            return;
        }

        self.inner.add_permits(n);
    }

    pub async fn acquire_owned(self: Arc<Self>) -> OwnedSemaphorePermit {
        if chaos::should_fail(ChaosOperation::SemaphoreAcquire) {
            std::future::pending::<()>().await;
        }

        let permit = self.inner.clone().acquire_owned().await.unwrap();
        OwnedSemaphorePermit {
            inner: Some(permit),
            _semaphore: self
        }
    }
}

#[derive(Debug)]
pub enum TryAcquireError {
    NoPermits,
    Closed
}

pub fn channel<T>(buffer: usize) -> (Sender<T>, Receiver<T>) {
    let (tx, rx) = sync::mpsc::channel(buffer);
    (
        Sender {
            inner: tx,
            chaos_state: Arc::new(std::sync::Mutex::new(ChannelChaosState::default()))
        },
        Receiver {
            inner: rx,
            chaos_state: Arc::new(std::sync::Mutex::new(ChannelChaosState::default()))
        }
    )
}

#[derive(Default)]
struct ChannelChaosState {
    drop_rate: f32,
    reorder_buffer: Vec<u64>
}

pub struct Sender<T> {
    inner: sync::mpsc::Sender<T>,
    chaos_state: Arc<std::sync::Mutex<ChannelChaosState>>
}

impl<T> Sender<T> {
    pub async fn send(&self, value: T) -> Result<(), SendError<T>> {
        if chaos::should_fail(ChaosOperation::MpscSend) {
            return Err(SendError(value))
        }

        if chaos::should_fail(ChaosOperation::MpscChannelFull) {
            tokio::time::sleep(Duration::from_millis(100)).await;
        }

        if chaos::should_fail(ChaosOperation::MpscChannelClosed) {
            return Err(SendError(value));
        }

        self.inner.send(value).await.map_err(|e| SendError(e.0))
    }

    pub fn try_send(&self, value: T) -> Result<(), TrySendError<T>> {
        if chaos::should_fail(ChaosOperation::MpscTrySend) {
            return Err(TrySendError::Full(value))
        }

        self.inner.try_send(value).map_err(|e| match e {
            sync::mpsc::error::TrySendError::Full(v) => TrySendError::Full(v),
            sync::mpsc::error::TrySendError::Closed(v) => TrySendError::Closed(v)
        })
    }

    pub async fn reserve(&self) -> Result<Permit<'_, T>, SendError<()>> {
        if chaos::should_fail(ChaosOperation::MpscPermitReserve) {
            return Err(SendError(()));
        }

        self.inner.reserve().await.map(|permit| Permit { inner: permit })
            .map_err(|_| SendError(()))
    }

    pub fn is_closed(&self) -> bool {
        self.inner.is_closed()
    }
}

impl<T> Clone for Sender<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            chaos_state: Arc::clone(&self.chaos_state)
        }
    }
}

pub struct Receiver<T> {
    inner: sync::mpsc::Receiver<T>,
    chaos_state: Arc<std::sync::Mutex<ChannelChaosState>>
}

impl<T> Receiver<T> {
    pub async fn recv(&mut self) -> Option<T> {
        if chaos::should_fail(ChaosOperation::MpscRecv) {
            return None;
        }

        if chaos::should_fail(ChaosOperation::PacketReorder) {
            tokio::time::sleep(Duration::from_millis(rand::random::<u64>() % 50)).await;
        }

        self.inner.recv().await
    }

    pub fn try_recv(&mut self) -> Result<T, TryRecvError> {
        if chaos::should_fail(ChaosOperation::MpscTryRecv) {
            return Err(TryRecvError::Empty)
        }

        self.inner.try_recv().map_err(|e| match e {
            sync::mpsc::error::TryRecvError::Empty => TryRecvError::Empty,
            sync::mpsc::error::TryRecvError::Disconnected => TryRecvError::Disconnected
        })
    }

    pub fn close(&mut self) {
        self.inner.close();
    }
}

pub struct UnboundedSender<T> {
    inner: sync::mpsc::UnboundedSender<T>,
    chaos_state: Arc<std::sync::Mutex<ChannelChaosState>>
}

pub struct UnboundedReceiver<T> {
    inner: sync::mpsc::UnboundedReceiver<T>,
    chaos_state: Arc<std::sync::Mutex<ChannelChaosState>>
}

pub struct Permit<'a, T> {
    inner: sync::mpsc::Permit<'a, T>
}

#[derive(Debug)]
pub struct SendError<T>(pub T);

#[derive(Debug)]
pub enum TrySendError<T> {
    Full(T),
    Closed(T)
}

#[derive(Debug)]
pub enum TryRecvError {
    Empty,
    Disconnected
}

pub fn oneshot<T>() -> (OneshotSender<T>, OneshotReceiver<T>) {
    let (tx, rx) = sync::oneshot::channel();

    (
        OneshotSender {
            inner: Some(tx),
            should_fail: chaos::should_fail(ChaosOperation::OneshotSend)
        },
        OneshotReceiver {
            inner: Some(rx),
            should_fail: chaos::should_fail(ChaosOperation::OneshotRecv)
        }
    )
}

pub struct OneshotSender<T> {
    inner: Option<sync::oneshot::Sender<T>>,
    should_fail: bool
}

impl<T> OneshotSender<T> {
    pub fn send(mut self, value: T) -> Result<(), T> {
        if self.should_fail {
            return Err(value)
        }

        if chaos::should_fail(ChaosOperation::OneshotClosed) {
            return Err(value)
        }

        if let Some(sender) = self.inner.take() {
            sender.send(value).map_err(|v| v)
        }
        else {
            Err(value)
        }
    }

    pub fn is_closed(&self) -> bool {
        self.inner.as_ref().map(|s| s.is_closed()).unwrap_or(true)
    }
}

pub struct OneshotReceiver<T> {
    inner: Option<sync::oneshot::Receiver<T>>,
    should_fail: bool
}

impl<T> Future for OneshotReceiver<T> {
    type Output = Result<T, RecvError>;

    fn poll(mut self: std::pin::Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        if self.should_fail {
            return Poll::Ready(Err(RecvError))
        }

        if chaos::should_fail(ChaosOperation::OneshotTimeout) {
            return Poll::Pending;
        }

        if let Some(mut receiver) = self.inner.take() {
            match Pin::new(&mut receiver).poll(cx) {
                Poll::Ready(Ok(value)) => Poll::Ready(Ok(value)),
                Poll::Ready(Err(_)) => Poll::Ready(Err(RecvError)),
                Poll::Pending => {
                    self.inner = Some(receiver);
                    Poll::Pending
                }
            }
        }
        else {
            Poll::Ready(Err(RecvError))
        }
    }
}

#[derive(Debug)]
pub struct RecvError;

pub fn broadcast<T: Clone>(capacity: usize) -> (BroadcastSender<T>, BroadcastReceiver<T>) {
    let (tx, rx) = sync::broadcast::channel(capacity);

    (
        BroadcastSender { inner: tx },
        BroadcastReceiver { inner: rx }
    )
}

pub struct BroadcastSender<T> {
    inner: sync::broadcast::Sender<T>
}

impl<T> BroadcastSender<T> {
    pub fn send(&self, value: T) -> Result<usize, BroadcastSendError<T>> {
        if chaos::should_fail(ChaosOperation::BroadcastSend) {
            return Err(BroadcastSendError(value))
        }

        self.inner.send(value).map_err(|e| BroadcastSendError(e.0))
    }

    pub fn subscribe(&self) -> BroadcastReceiver<T> {
        BroadcastReceiver { inner: self.inner.subscribe() }
    }
}

impl<T> Clone for BroadcastSender<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone()
        }
    }
}

pub struct BroadcastReceiver<T> {
    inner: sync::broadcast::Receiver<T>
}

impl<T: Clone> BroadcastReceiver<T> {
    pub async fn recv(&mut self) -> Result<T, BroadcastRecvError> {
        if chaos::should_fail(ChaosOperation::BroadcastRecv) {
            return Err(BroadcastRecvError::Closed)
        }

        if chaos::should_fail(ChaosOperation::BroadcastLagged) {
            return Err(BroadcastRecvError::Lagged(10))
        }

        self.inner.recv().await.map_err(|e| match e {
            sync::broadcast::error::RecvError::Closed => BroadcastRecvError::Closed,
            sync::broadcast::error::RecvError::Lagged(n) => BroadcastRecvError::Lagged(n),
        })
    }

    pub fn try_recv(&mut self) -> Result<T, TryBroadcastRecvError> {
        self.inner.try_recv().map_err(|e| match e {
            sync::broadcast::error::TryRecvError::Empty => TryBroadcastRecvError::Empty,
            sync::broadcast::error::TryRecvError::Closed => TryBroadcastRecvError::Closed,
            sync::broadcast::error::TryRecvError::Lagged(n) => TryBroadcastRecvError::Lagged(n),
        })
    }
}

#[derive(Debug)]
pub struct BroadcastSendError<T>(pub T);

#[derive(Debug)]
pub enum BroadcastRecvError {
    Closed,
    Lagged(u64)
}

#[derive(Debug)]
pub enum TryBroadcastRecvError {
    Empty,
    Closed,
    Lagged(u64)
}

pub fn watch<T>(init: T) -> (WatchSender<T>, WatchReceiver<T>) {
    let (tx, rx) = sync::watch::channel(init);

    (
        WatchSender { inner: tx },
        WatchReceiver { inner: rx }
    )
}

pub struct WatchSender<T> {
    inner: sync::watch::Sender<T>
}

impl<T> WatchSender<T> {
    pub fn send(&self, value: T) -> Result<(), WatchSendError<T>> {
        if chaos::should_fail(ChaosOperation::WatchSend) {
            return Err(WatchSendError(value))
        }

        self.inner.send(value).map_err(|e| WatchSendError(e.0))
    }

    pub fn subscribe(&self) -> WatchReceiver<T> {
        WatchReceiver {
            inner: self.inner.subscribe()
        }
    }

    pub fn is_closed(&self) -> bool {
        self.inner.is_closed()
    }
}

pub struct WatchReceiver<T> {
    inner: sync::watch::Receiver<T>
}

impl <T: Clone> WatchReceiver<T> {
    pub fn borrow(&self) -> sync::watch::Ref<'_, T> {
        self.inner.borrow()
    }

    pub fn borrow_and_update(&mut self) -> sync::watch::Ref<'_, T> {
        self.inner.borrow_and_update()
    }

    pub async fn changed(&mut self) -> Result<(), WatchRecvError> {
        if chaos::should_fail(ChaosOperation::WatchChanged) {
            return Err(WatchRecvError)
        }

        if chaos::should_fail(ChaosOperation::WatchUnchanged) {
            std::future::pending::<()>().await;
        }

        self.inner.changed().await.map_err(|_| WatchRecvError)
    }

    pub fn has_changed(&self) -> Result<bool, WatchRecvError> {
        self.inner.has_changed().map_err(|_| WatchRecvError)
    }
}

#[derive(Debug)]
pub struct WatchSendError<T>(pub T);

#[derive(Debug)]
pub struct WatchRecvError;

pub struct Notify {
    inner: sync::Notify,
    chaos_state: Arc<std::sync::Mutex<NotifyChaosState>>
}

#[derive(Default)]
struct NotifyChaosState {
    miss_notifications: bool
}

impl Notify {
    pub fn new() -> Self {
        Self {
            inner: sync::Notify::new(),
            chaos_state: Arc::new(std::sync::Mutex::new(NotifyChaosState::default()))
        }
    }

    pub fn notify_one(&self) {
        if chaos::should_fail(ChaosOperation::NotifyNotifyOne) {
            return;
        }

        if chaos::should_fail(ChaosOperation::NotifyMissed) {
            if let Ok(mut state) = self.chaos_state.lock() {
                state.miss_notifications = true;
            }
        }

        self.inner.notify_one();
    }

    pub fn notify_waiters(&self) {
        self.inner.notify_waiters();
    }

    pub async fn notified(&self) {
        if chaos::should_fail(ChaosOperation::NotifyNotified) {
            std::future::pending::<()>().await;
        }

        if chaos::should_fail(ChaosOperation::NotifyNotifyWaitTimeout) {
            tokio::time::sleep(Duration::from_secs(30)).await;
            return;
        }

        if let Ok(state) = self.chaos_state.lock() {
            if state.miss_notifications {
                tokio::time::sleep(Duration::from_millis(100)).await;
            }
        }

        self.inner.notified().await;
    }

    pub fn enable() -> NotifyHandle {
        NotifyHandle {
            inner: Arc::new(Notify::new())
        }
    }
}

pub struct NotifyHandle {
    inner: Arc<Notify>
}

impl NotifyHandle {
    pub fn notify_one(&self) {
        self.inner.notify_one();
    }

    pub fn notify_waiters(&self) {
        self.inner.notify_waiters();
    }

    pub async fn notified(&self) {
        self.inner.notified().await;
    }
}

impl Clone for NotifyHandle {
    fn clone(&self) -> Self {
        Self {
            inner: Arc::clone(&self.inner)
        }
    }
}

impl Default for Notify {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Barrier {
    inner: sync:: Barrier
}

impl Barrier {
    pub fn new(n: usize) -> Self {
        Self {
            inner: sync::Barrier::new(n)
        }
    }

    pub async fn wait(&self) -> BarrierWaitResult {
        if chaos::should_fail(ChaosOperation::BarrierWait) {
            std::future::pending::<()>().await;
        }

        if chaos::should_fail(ChaosOperation::BarrierTimeout) {
            tokio::time::sleep(Duration::from_secs(30)).await;
            panic!("fracture: Barrier timeout (chaos)");
        }

        if chaos::should_fail(ChaosOperation::BarrierBroken) {
            panic!("fracture: Barrier broken (chaos)");
        }

        let result = self.inner.wait().await;
        BarrierWaitResult {
            is_leader: result.is_leader()
        }
    }
}

pub struct BarrierWaitResult {
    is_leader: bool
}

impl BarrierWaitResult {
    pub fn is_leader(&self) -> bool {
        self.is_leader
    }
}

pub struct RwLockMappedReadGuard<'a, T: ?Sized, U: ?Sized> {
    _inner: sync::RwLockReadGuard<'a, T>,
    data: *const U,
    _phantom: std::marker::PhantomData<&'a U>
}

impl<'a, T: ?Sized, U: ?Sized> Deref for RwLockMappedReadGuard<'a, T, U> {
    type Target = U;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.data }
    }
}

pub struct RwLockMappedWriteGuard<'a, T: ?Sized, U: ?Sized> {
    _inner: sync::RwLockWriteGuard<'a, T>,
    data: *mut U,
    _phantom: std::marker::PhantomData<&'a mut U>
}

impl<'a, T: ?Sized, U: ?Sized> Deref for RwLockMappedWriteGuard<'a, T, U> {
    type Target = U;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.data }
    }
}

impl<'a, T: ?Sized, U: ?Sized> DerefMut for RwLockMappedWriteGuard<'a, T, U> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.data }
    }
}