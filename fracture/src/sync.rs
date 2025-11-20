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


pub mod mpsc {
    use super::*;
    
    pub fn channel<T>(buffer: usize) -> (Sender<T>, Receiver<T>) {
        // Placeholder
        unbounded()
    }
    pub fn unbounded<T>() -> (Sender<T>, Receiver<T>) {
        let shared = Arc::new(StdMutex::new(State { queue: VecDeque::new(), waker: None, closed: false }));
        (Sender { shared: shared.clone() }, Receiver { shared })
    }

    struct State<T> { queue: VecDeque<T>, waker: Option<Waker>, closed: bool }
    
    pub struct Sender<T> { shared: Arc<StdMutex<State<T>>> }

    impl<T> Sender<T> {
        pub async fn send(&self, value: T) -> Result<(), T> {
            if chaos::should_fail(ChaosOperation::MpscSend) {
                return Err(value);
            }

            let mut s = self.shared.lock().unwrap();
            if s.closed { return Err(value); }
            s.queue.push_back(value);
            if let Some(w) = s.waker.take() { w.wake(); }
            Ok(())
        }
    }
    impl<T> Clone for Sender<T> {
        fn clone(&self) -> Self { Self { shared: self.shared.clone() } }
    }

    pub struct Receiver<T> { shared: Arc<StdMutex<State<T>>> }
    impl<T> Receiver<T> {
        pub async fn recv(&mut self) -> Option<T> {
            struct RecvFuture<'a, T> { rx: &'a mut Receiver<T> }
            impl<T> Future for RecvFuture<'_, T> {
                type Output = Option<T>;
                fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<T>> {
                    if chaos::should_fail(ChaosOperation::MpscRecv) {
                        return Poll::Ready(None);
                    }

                    let mut s = self.rx.shared.lock().unwrap();
                    if let Some(v) = s.queue.pop_front() {
                        Poll::Ready(Some(v))
                    } else if s.closed {
                        Poll::Ready(None)
                    } else {
                        s.waker = Some(cx.waker().clone());
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
            if s.closed { return Err(t); }
            s.value = Some(t);
            if let Some(w) = s.waker.take() { w.wake(); }
            Ok(())
        }
    }

    pub struct Receiver<T> { shared: Arc<StdMutex<State<T>>> }
    impl<T> Future for Receiver<T> {
        type Output = Result<T, ()>; // Error type simplified
        fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
            if chaos::should_fail(ChaosOperation::OneshotRecv) {
                return Poll::Ready(Err(()));
            }

            let mut s = self.shared.lock().unwrap();
            if let Some(v) = s.value.take() {
                Poll::Ready(Ok(v))
            }
            else if s.closed {
                Poll::Ready(Err(())) 
            }
            else {
                s.waker = Some(cx.waker().clone());
                Poll::Pending
            }
        }
    }
}