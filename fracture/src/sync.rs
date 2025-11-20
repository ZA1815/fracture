use std::cell::UnsafeCell;
use std::collections::VecDeque;
use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, Mutex as StdMutex};
use std::task::{Context, Poll, Waker};

use crate::chaos::{self, ChaosOperation};

pub struct Mutex<T: ?Sized> {
    state: StdMutex<MutexState>,
    data: UnsafeCell<T>,
}

struct MutexState {
    locked: bool,
    waiters: VecDeque<Waker>,
}

unsafe impl<T: Send + ?Sized> Send for Mutex<T> {}
unsafe impl<T: Send + ?Sized> Sync for Mutex<T> {}

impl<T> Mutex<T> {
    pub fn new(data: T) -> Self {
        Self {
            state: StdMutex::new(MutexState { locked: false, waiters: VecDeque::new() }),
            data: UnsafeCell::new(data),
        }
    }
}

impl<T: ?Sized> Mutex<T> {
    pub async fn lock(&self) -> MutexGuard<'_, T> {
        if chaos::should_fail(ChaosOperation::MutexDeadlock) {
            std::future::pending::<()>().await;
        }

        struct LockFuture<'a, T: ?Sized> { mutex: &'a Mutex<T> }
        impl<T: ?Sized> Future for LockFuture<'_, T> {
            type Output = ();
            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<()> {
                if chaos::should_fail(ChaosOperation::MutexLockTimeout) {
                    return Poll::Pending;
                }

                let mut state = self.mutex.state.lock().unwrap();
                if !state.locked {
                    state.locked = true;
                    Poll::Ready(())
                }
                else {
                    state.waiters.push_back(cx.waker().clone());
                    Poll::Pending
                }
            }
        }
        LockFuture { mutex: self }.await;
        MutexGuard { lock: self }
    }

    pub fn try_lock(&self) -> Result<MutexGuard<'_, T>, ()> {
        if chaos::should_fail(ChaosOperation::MutexTryLock) {
            return Err(());
        }

        let mut state = self.state.lock().unwrap();
        if !state.locked {
            state.locked = true;
            Ok(MutexGuard { lock: self })
        } else {
            Err(())
        }
    }
}

pub struct MutexGuard<'a, T: ?Sized> { lock: &'a Mutex<T> }

impl<T: ?Sized> Drop for MutexGuard<'_, T> {
    fn drop(&mut self) {
        let mut state = self.lock.state.lock().unwrap();
        state.locked = false;
        if let Some(waker) = state.waiters.pop_front() {
            waker.wake();
        }
    }
}

impl<T: ?Sized> std::ops::Deref for MutexGuard<'_, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target { unsafe { &*self.lock.data.get() } }
}
impl<T: ?Sized> std::ops::DerefMut for MutexGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target { unsafe { &mut *self.lock.data.get() } }
}

pub struct RwLock<T: ?Sized> {
    state: StdMutex<RwLockState>,
    data: UnsafeCell<T>,
}

struct RwLockState {
    readers: usize,
    writer: bool,
    writer_waiters: VecDeque<Waker>,
    reader_waiters: VecDeque<Waker>,
}

unsafe impl<T: Send + Sync + ?Sized> Send for RwLock<T> {}
unsafe impl<T: Send + Sync + ?Sized> Sync for RwLock<T> {}

impl<T> RwLock<T> {
    pub fn new(data: T) -> Self {
        Self {
            state: StdMutex::new(RwLockState { readers: 0, writer: false, writer_waiters: VecDeque::new(), reader_waiters: VecDeque::new() }),
            data: UnsafeCell::new(data),
        }
    }
}

impl<T: ?Sized> RwLock<T> {
    pub async fn read(&self) -> RwLockReadGuard<'_, T> {
        if chaos::should_fail(ChaosOperation::RwLockDeadlock) {
            std::future::pending::<()>().await;
        }

        struct ReadFuture<'a, T: ?Sized> { lock: &'a RwLock<T> }
        impl<T: ?Sized> Future for ReadFuture<'_, T> {
            type Output = ();
            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<()> {
                if chaos::should_fail(ChaosOperation::RwLockRead) {
                    return Poll::Pending;
                }

                let mut state = self.lock.state.lock().unwrap();
                if !state.writer {
                    state.readers += 1;
                    Poll::Ready(())
                } else {
                    state.reader_waiters.push_back(cx.waker().clone());
                    Poll::Pending
                }
            }
        }
        ReadFuture { lock: self }.await;
        RwLockReadGuard { lock: self }
    }

    pub async fn write(&self) -> RwLockWriteGuard<'_, T> {
        if chaos::should_fail(ChaosOperation::RwLockDeadlock) {
            std::future::pending::<()>().await;
        }

        struct WriteFuture<'a, T: ?Sized> { lock: &'a RwLock<T> }
        impl<T: ?Sized> Future for WriteFuture<'_, T> {
            type Output = ();
            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<()> {
                if chaos::should_fail(ChaosOperation::RwLockWrite) {
                    return Poll::Pending;
                }

                let mut state = self.lock.state.lock().unwrap();
                if !state.writer && state.readers == 0 {
                    state.writer = true;
                    Poll::Ready(())
                } else {
                    state.writer_waiters.push_back(cx.waker().clone());
                    Poll::Pending
                }
            }
        }
        WriteFuture { lock: self }.await;
        RwLockWriteGuard { lock: self }
    }
}

pub struct RwLockReadGuard<'a, T: ?Sized> { lock: &'a RwLock<T> }
impl<T: ?Sized> Drop for RwLockReadGuard<'_, T> {
    fn drop(&mut self) {
        let mut state = self.lock.state.lock().unwrap();
        state.readers -= 1;
        if state.readers == 0 {
            if let Some(waker) = state.writer_waiters.pop_front() {
                waker.wake();
            }
        }
    }
}
impl<T: ?Sized> std::ops::Deref for RwLockReadGuard<'_, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target { unsafe { &*self.lock.data.get() } }
}

pub struct RwLockWriteGuard<'a, T: ?Sized> { lock: &'a RwLock<T> }
impl<T: ?Sized> Drop for RwLockWriteGuard<'_, T> {
    fn drop(&mut self) {
        let mut state = self.lock.state.lock().unwrap();
        state.writer = false;
        if let Some(waker) = state.writer_waiters.pop_front() {
            waker.wake();
        }
        else {
            while let Some(waker) = state.reader_waiters.pop_front() {
                waker.wake();
            }
        }
    }
}
impl<T: ?Sized> std::ops::Deref for RwLockWriteGuard<'_, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target { unsafe { &*self.lock.data.get() } }
}
impl<T: ?Sized> std::ops::DerefMut for RwLockWriteGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target { unsafe { &mut *self.lock.data.get() } }
}

pub struct Notify {
    waiters: StdMutex<VecDeque<Waker>>,
    notified: StdMutex<bool>,
}

impl Notify {
    pub fn new() -> Self {
        Self { waiters: StdMutex::new(VecDeque::new()), notified: StdMutex::new(false) }
    }
    pub fn notify_one(&self) {
        if chaos::should_fail(ChaosOperation::NotifyMissed) {
            return;
        }

        let mut notified = self.notified.lock().unwrap();
        if let Some(waker) = self.waiters.lock().unwrap().pop_front() {
            waker.wake();
        } else {
            *notified = true;
        }
    }
    pub async fn notified(&self) {
        struct NotifyFuture<'a> { n: &'a Notify }
        impl Future for NotifyFuture<'_> {
            type Output = ();
            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<()> {
                if chaos::should_fail(ChaosOperation::NotifyNotifyWaitTimeout) {
                    // Placeholder for chaos
                    return Poll::Pending;
                }

                let mut notified = self.n.notified.lock().unwrap();
                if *notified {
                    *notified = false;
                    Poll::Ready(())
                } else {
                    self.n.waiters.lock().unwrap().push_back(cx.waker().clone());
                    Poll::Pending
                }
            }
        }
        NotifyFuture { n: self }.await
    }
}

pub struct Semaphore {
    permits: StdMutex<usize>,
    waiters: StdMutex<VecDeque<(usize, Waker)>>,
}

impl Semaphore {
    pub fn new(permits: usize) -> Self {
        Self { permits: StdMutex::new(permits), waiters: StdMutex::new(VecDeque::new()) }
    }

    pub async fn acquire(&self) -> Result<SemaphorePermit<'_>, ()> {
        self.acquire_many(1).await
    }

    pub async fn acquire_many(&self, n: u32) -> Result<SemaphorePermit<'_>, ()> {
        if chaos::should_fail(ChaosOperation::SemaphoreAcquire) {
            return Err(());
        }

        let n = n as usize;
        struct AcquireFuture<'a> { sem: &'a Semaphore, n: usize }
        impl Future for AcquireFuture<'_> {
            type Output = ();
            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<()> {
                if chaos::should_fail(ChaosOperation::SemaphoreAcquireTimeout) {
                    return Poll::Pending;
                }

                let mut permits = self.sem.permits.lock().unwrap();
                if *permits >= self.n {
                    *permits -= self.n;
                    Poll::Ready(())
                } else {
                    self.sem.waiters.lock().unwrap().push_back((self.n, cx.waker().clone()));
                    Poll::Pending
                }
            }
        }
        AcquireFuture { sem: self, n }.await;
        Ok(SemaphorePermit { sem: self, permits: n })
    }

    pub fn add_permits(&self, n: usize) {
        let mut permits = self.permits.lock().unwrap();
        *permits += n;
        let mut waiters = self.waiters.lock().unwrap();
        // Naive wake: wake everyone to check again. Inefficient but correct.
        while let Some((req, waker)) = waiters.front() {
            if *permits >= *req {
                let (_, waker) = waiters.pop_front().unwrap();
                waker.wake();
            } else {
                break;
            }
        }
    }
}

pub struct SemaphorePermit<'a> { sem: &'a Semaphore, permits: usize }
impl Drop for SemaphorePermit<'_> {
    fn drop(&mut self) {
        self.sem.add_permits(self.permits);
    }
}

pub struct Barrier {
    state: StdMutex<BarrierState>
}

struct BarrierState {
    n: usize,
    count: usize,
    generation: usize,
    waiters: VecDeque<Waker>
}

#[derive(Debug, Clone)]
pub struct BarrierWaitResult(bool);

impl BarrierWaitResult {
    pub fn is_leader(&self) -> bool { self.0 }
}

impl Barrier {
    pub fn new(n: usize) -> Self {
        Self {
            state: StdMutex::new(BarrierState {
                n,
                count: 0,
                generation: 0,
                waiters: VecDeque::new(),
            }),
        }
    }

    pub async fn wait(&self) -> BarrierWaitResult {
        if chaos::should_fail(ChaosOperation::BarrierWait) {
            std::future::pending::<()>().await;
        }

        struct WaitFuture<'a> {
            barrier: &'a Barrier,
            my_generation: Option<usize>,
        }

        impl Future for WaitFuture<'_> {
            type Output = BarrierWaitResult;

            fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                let mut state = self.barrier.state.lock().unwrap();

                if self.my_generation.is_none() {
                    self.my_generation = Some(state.generation);
                    state.count += 1;

                    if state.count >= state.n {
                        state.count = 0;
                        state.generation += 1;
                        for waker in state.waiters.drain(..) {
                            waker.wake();
                        }
                        return Poll::Ready(BarrierWaitResult(true));
                    }
                }

                if state.generation != self.my_generation.unwrap() {
                    return Poll::Ready(BarrierWaitResult(false));
                }

                if chaos::should_fail(ChaosOperation::BarrierTimeout) {
                     return Poll::Pending;
                }

                state.waiters.push_back(cx.waker().clone());
                Poll::Pending
            }
        }

        WaitFuture { barrier: self, my_generation: None }.await
    }
}


pub mod mpsc {
    use super::*;

    #[derive(Debug)]
    pub struct SendError<T>(pub T);

    #[derive(Debug)]
    pub enum TrySendError<T> {
        Full(T),
        Closed(T),
    }

    pub fn unbounded<T>() -> (UnboundedSender<T>, UnboundedReceiver<T>) {
        let shared = Arc::new(StdMutex::new(UnboundedState {
            queue: VecDeque::new(),
            waker: None,
            closed: false,
            rx_count: 1,
        }));
        (UnboundedSender { shared: shared.clone() }, UnboundedReceiver { shared })
    }

    struct UnboundedState<T> { queue: VecDeque<T>, waker: Option<Waker>, closed: bool, rx_count: usize }
    
    pub struct UnboundedSender<T> { shared: Arc<StdMutex<UnboundedState<T>>> }
    impl<T> UnboundedSender<T> {
        pub fn send(&self, value: T) -> Result<(), SendError<T>> {
            if chaos::should_fail(ChaosOperation::MpscSend) {
                return Err(SendError(value));
            }
            let mut s = self.shared.lock().unwrap();
            if s.closed { return Err(SendError(value)); }
            s.queue.push_back(value);
            if let Some(w) = s.waker.take() { w.wake(); }
            Ok(())
        }
    }
    impl<T> Clone for UnboundedSender<T> {
        fn clone(&self) -> Self { Self { shared: self.shared.clone() } }
    }

    pub struct UnboundedReceiver<T> { shared: Arc<StdMutex<UnboundedState<T>>> }
    impl<T> UnboundedReceiver<T> {
        pub async fn recv(&mut self) -> Option<T> {
            struct RecvFuture<'a, T> { rx: &'a mut UnboundedReceiver<T> }
            impl<T> Future for RecvFuture<'_, T> {
                type Output = Option<T>;
                fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<T>> {
                    if chaos::should_fail(ChaosOperation::MpscRecv) {
                        return Poll::Ready(None);
                    }
                    let mut s = self.rx.shared.lock().unwrap();
                    if let Some(v) = s.queue.pop_front() { Poll::Ready(Some(v)) }
                    else if s.closed { Poll::Ready(None) }
                    else { s.waker = Some(cx.waker().clone()); Poll::Pending }
                }
            }
            RecvFuture { rx: self }.await
        }

        pub fn close(&mut self) {
            let mut s = self.shared.lock().unwrap();
            s.closed = true;
        }
    }
    impl<T> Drop for UnboundedReceiver<T> {
        fn drop(&mut self) {
            let mut s = self.shared.lock().unwrap();
            s.rx_count -= 1;
            if s.rx_count == 0 { s.closed = true; }
        }
    }
    
    pub fn channel<T>(capacity: usize) -> (Sender<T>, Receiver<T>) {
        assert!(capacity > 0, "capacity must be > 0");
        let state = Arc::new(StdMutex::new(BoundedState {
            queue: VecDeque::with_capacity(capacity),
            capacity,
            tx_waiters: VecDeque::new(),
            rx_waker: None,
            closed: false,
            sender_count: 1,
        }));
        (Sender { state: state.clone() }, Receiver { state })
    }

    struct BoundedState<T> {
        queue: VecDeque<T>,
        capacity: usize,
        tx_waiters: VecDeque<Waker>,
        rx_waker: Option<Waker>,
        closed: bool,
        sender_count: usize,
    }

    pub struct Sender<T> { state: Arc<StdMutex<BoundedState<T>>> }
    
    impl<T> Sender<T> {
        pub async fn send(&self, value: T) -> Result<(), SendError<T>> {
            struct SendFuture<'a, T> { sender: &'a Sender<T>, value: Option<T> }
            
            impl<T> Future for SendFuture<'_, T> {
                type Output = Result<(), SendError<T>>;

                fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                    if chaos::should_fail(ChaosOperation::MpscSend) {
                        return Poll::Ready(Err(SendError(self.value.take().unwrap())));
                    }

                    let mut s = self.sender.state.lock().unwrap();
                    
                    if s.closed {
                        return Poll::Ready(Err(SendError(self.value.take().unwrap())));
                    }

                    if s.queue.len() < s.capacity {
                        s.queue.push_back(self.value.take().unwrap());
                        if let Some(w) = s.rx_waker.take() { w.wake(); }
                        Poll::Ready(Ok(()))
                    }
                    else {
                        s.tx_waiters.push_back(cx.waker().clone());
                        Poll::Pending
                    }
                }
            }

            SendFuture { sender: self, value: Some(value) }.await
        }

        pub fn try_send(&self, value: T) -> Result<(), TrySendError<T>> {
            if chaos::should_fail(ChaosOperation::MpscTrySend) {
                return Err(TrySendError::Full(value));
            }

            let mut s = self.state.lock().unwrap();
            if s.closed {
                return Err(TrySendError::Closed(value));
            }

            if s.queue.len() < s.capacity {
                s.queue.push_back(value);
                if let Some(w) = s.rx_waker.take() { w.wake(); }
                Ok(())
            }
            else {
                Err(TrySendError::Full(value))
            }
        }
    }

    impl<T> Clone for Sender<T> {
        fn clone(&self) -> Self {
            let mut s = self.state.lock().unwrap();
            s.sender_count += 1;
            Self { state: self.state.clone() }
        }
    }

    impl<T> Drop for Sender<T> {
        fn drop(&mut self) {
            let mut s = self.state.lock().unwrap();
            s.sender_count -= 1;
            if s.sender_count == 0 {
                s.closed = true;
                if let Some(w) = s.rx_waker.take() { w.wake(); }
            }
        }
    }

    pub struct Receiver<T> { state: Arc<StdMutex<BoundedState<T>>> }

    impl<T> Receiver<T> {
        pub async fn recv(&mut self) -> Option<T> {
             struct RecvFuture<'a, T> { rx: &'a mut Receiver<T> }
            impl<T> Future for RecvFuture<'_, T> {
                type Output = Option<T>;
                fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<T>> {
                    if chaos::should_fail(ChaosOperation::MpscRecv) {
                        return Poll::Ready(None);
                    }

                    let mut s = self.rx.state.lock().unwrap();
                    if let Some(v) = s.queue.pop_front() {
                        if let Some(w) = s.tx_waiters.pop_front() { w.wake(); }
                        Poll::Ready(Some(v))
                    }
                    else if s.closed && s.sender_count == 0 {
                        Poll::Ready(None)
                    }
                    else {
                        s.rx_waker = Some(cx.waker().clone());
                        Poll::Pending
                    }
                }
            }
            RecvFuture { rx: self }.await
        }
    }
}

pub mod oneshot {
    use super::*;
    
    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    pub struct RecvError;

    pub fn channel<T>() -> (Sender<T>, Receiver<T>) {
        let shared = Arc::new(StdMutex::new(State { value: None, waker: None, closed: false }));
        (Sender { shared: shared.clone() }, Receiver { shared })
    }
    struct State<T> { value: Option<T>, waker: Option<Waker>, closed: bool }

    pub struct Sender<T> { shared: Arc<StdMutex<State<T>>> }

    impl<T> Sender<T> {
        pub fn send(self, t: T) -> Result<(), T> {
            if chaos::should_fail(ChaosOperation::OneshotSend) {
                return Err(t);
            }
            let mut s = self.shared.lock().unwrap();
            if s.closed {
                return Err(t);
            }
            s.value = Some(t);
            if let Some(w) = s.waker.take() {
                w.wake();
            }
            Ok(())
        }
    }

    pub struct Receiver<T> { shared: Arc<StdMutex<State<T>>> }
    impl<T> Future for Receiver<T> {
        type Output = Result<T, RecvError>;
        fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
            if chaos::should_fail(ChaosOperation::OneshotRecv) {
                return Poll::Ready(Err(RecvError));
            }
            let mut s = self.shared.lock().unwrap();
            if let Some(v) = s.value.take() { Poll::Ready(Ok(v)) }
            else if s.closed {
                Poll::Ready(Err(RecvError))
            }
            else {
                s.waker = Some(cx.waker().clone());
                Poll::Pending
            }
        }
    }
    impl<T> Drop for Receiver<T> {
        fn drop(&mut self) {
            let mut s = self.shared.lock().unwrap();
            s.closed = true;
        }
    }
}

pub mod watch {
    use super::*;

    #[derive(Debug, Clone)]
    pub struct SendError<T>(pub T);

    #[derive(Debug, Clone)]
    pub struct RecvError;

    pub fn channel<T: Clone + Send + Sync + 'static>(init: T) -> (Sender<T>, Receiver<T>) {
        let state = Arc::new(StdMutex::new(State {
            value: init.clone(),
            version: 0,
            waiters: Vec::new(),
            closed: false,
        }));
        (Sender { state: state.clone() }, Receiver { state, version: 0, value: init })
    }

    struct State<T> {
        value: T,
        version: u64,
        waiters: Vec<Waker>,
        closed: bool,
    }

    pub struct Sender<T> { state: Arc<StdMutex<State<T>>> }
    
    impl<T: Clone> Sender<T> {
        pub fn send(&self, value: T) -> Result<(), SendError<T>> {
            if chaos::should_fail(ChaosOperation::WatchSend) {
                return Err(SendError(value));
            }
            
            let mut s = self.state.lock().unwrap();
            if s.closed { return Err(SendError(value)); }
            
            s.value = value;
            s.version += 1;
            for w in s.waiters.drain(..) { w.wake(); }
            Ok(())
        }

        pub fn send_modify<F>(&self, modify: F) 
        where F: FnOnce(&mut T) {
            let mut s = self.state.lock().unwrap();
            if s.closed { return; }
            modify(&mut s.value);
            s.version += 1;
            for w in s.waiters.drain(..) { w.wake(); }
        }

        pub fn borrow(&self) -> Ref<'_, T> {
            let s = self.state.lock().unwrap();
            Ref { inner: std::sync::MutexGuard::map(s, |x| &mut x.value) } // Simplification
        }
    }

    impl<T> Drop for Sender<T> {
        fn drop(&mut self) {
            let mut s = self.state.lock().unwrap();
            s.closed = true;
            for w in s.waiters.drain(..) { w.wake(); }
        }
    }

    // Placeholder for Ref implementation simplification
    pub struct Ref<'a, T> { inner: T } // Simplified, fix later

    #[derive(Clone)]
    pub struct Receiver<T> {
        state: Arc<StdMutex<State<T>>>,
        version: u64,
        value: T,
    }

    impl<T: Clone> Receiver<T> {
        pub fn borrow(&self) -> &T { &self.value }
        
        pub fn borrow_and_update(&mut self) -> &T {
            let s = self.state.lock().unwrap();
            self.value = s.value.clone();
            self.version = s.version;
            &self.value
        }

        pub async fn changed(&mut self) -> Result<(), RecvError> {
            struct ChangedFuture<'a, T> { rx: &'a mut Receiver<T> }
            impl<T: Clone> Future for ChangedFuture<'_, T> {
                type Output = Result<(), RecvError>;
                fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                    if chaos::should_fail(ChaosOperation::WatchChanged) {
                        return Poll::Ready(Err(RecvError));
                    }

                    let mut s = self.rx.state.lock().unwrap();
                    if s.version > self.rx.version {
                        self.rx.value = s.value.clone();
                        self.rx.version = s.version;
                        return Poll::Ready(Ok(()));
                    }
                    if s.closed { return Poll::Ready(Err(RecvError)); }
                    
                    s.waiters.push(cx.waker().clone());
                    Poll::Pending
                }
            }
            ChangedFuture { rx: self }.await
        }
    }
}

pub mod broadcast {
    use super::*;

    #[derive(Debug, Clone)]
    pub struct SendError<T>(pub T);

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum RecvError {
        Closed,
        Lagged(u64),
    }

    #[derive(Clone)]
    struct Message<T> {
        val: T,
        seq: u64,
    }

    pub fn channel<T: Clone + Send + 'static>(capacity: usize) -> (Sender<T>, Receiver<T>) {
        let state = Arc::new(StdMutex::new(State {
            buffer: VecDeque::with_capacity(capacity),
            capacity,
            tail_seq: 0,
            closed: false,
            senders: 1,
            waiters: Vec::new(),
        }));
        (Sender { state: state.clone() }, Receiver { state, next_seq: 0 })
    }

    struct State<T> {
        buffer: VecDeque<Message<T>>,
        capacity: usize,
        tail_seq: u64,
        closed: bool,
        senders: usize,
        waiters: Vec<Waker>,
    }

    pub struct Sender<T> { state: Arc<StdMutex<State<T>>> }

    impl<T: Clone> Sender<T> {
        pub fn send(&self, value: T) -> Result<usize, SendError<T>> {
             if chaos::should_fail(ChaosOperation::BroadcastSend) {
                return Err(SendError(value));
            }

             let mut s = self.state.lock().unwrap();
             if s.closed { return Err(SendError(value)); }

             if s.buffer.len() == s.capacity {
                 s.buffer.pop_front();
             }
             
             s.buffer.push_back(Message { val: value, seq: s.tail_seq });
             s.tail_seq += 1;

             for w in s.waiters.drain(..) { w.wake(); }
             
             Ok(1)
        }
    }

    impl<T> Clone for Sender<T> {
        fn clone(&self) -> Self {
            let mut s = self.state.lock().unwrap();
            s.senders += 1;
            Self { state: self.state.clone() }
        }
    }

    impl<T> Drop for Sender<T> {
        fn drop(&mut self) {
            let mut s = self.state.lock().unwrap();
            s.senders -= 1;
            if s.senders == 0 {
                s.closed = true;
                for w in s.waiters.drain(..) { w.wake(); }
            }
        }
    }

    pub struct Receiver<T> {
        state: Arc<StdMutex<State<T>>>,
        next_seq: u64,
    }

    impl<T: Clone> Receiver<T> {
        pub async fn recv(&mut self) -> Result<T, RecvError> {
            struct RecvFuture<'a, T> { rx: &'a mut Receiver<T> }
            impl<T: Clone> Future for RecvFuture<'_, T> {
                type Output = Result<T, RecvError>;
                fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                    if chaos::should_fail(ChaosOperation::BroadcastRecv) {
                        return Poll::Ready(Err(RecvError::Closed));
                    }
                    
                    let mut s = self.rx.state.lock().unwrap();
                    
                    if let Some(head) = s.buffer.front() {
                        if self.rx.next_seq < head.seq {
                            let missed = head.seq - self.rx.next_seq;
                            self.rx.next_seq = s.tail_seq;
                            return Poll::Ready(Err(RecvError::Lagged(missed)));
                        }
                    }

                    for msg in &s.buffer {
                        if msg.seq == self.rx.next_seq {
                            self.rx.next_seq += 1;
                            return Poll::Ready(Ok(msg.val.clone()));
                        }
                    }

                    if s.closed && s.senders == 0 {
                        return Poll::Ready(Err(RecvError::Closed));
                    }

                    s.waiters.push(cx.waker().clone());
                    Poll::Pending
                }
            }
            RecvFuture { rx: self }.await
        }

        pub fn resubscribe(&self) -> Self {
             let s = self.state.lock().unwrap();
             Self { state: self.state.clone(), next_seq: s.tail_seq }
        }
    }
    
    impl<T> Clone for Receiver<T> {
        fn clone(&self) -> Self {
             Self { state: self.state.clone(), next_seq: self.next_seq }
        }
    }
}