use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};
use std::time::Duration;

use crate::chaos::{self, ChaosOperation};

pub fn spawn<F>(future: F) -> JoinHandle<F::Output>
where F: Future + Send + 'static, F::Output: Send + 'static {
    if chaos::should_fail(ChaosOperation::TaskSpawn) {
        return JoinHandle {
            inner: None,
            chaos_state: JoinHandleChaos::AlwaysFail
        };
    }

    if chaos::should_fail(ChaosOperation::TaskScheduleDelay) {
        let delayed_future = async move {
            tokio::time::sleep(Duration::from_millis(rand::random::<u64>() % 100)).await;
            future.await
        };

        return JoinHandle {
            inner: Some(tokio::task::spawn(delayed_future)),
            chaos_state: JoinHandleChaos::None
        };
    }

    if chaos::should_fail(ChaosOperation::TaskPanic) {
        let panic_future = async move {
            let result = future.await;
            panic!("fracture: Task panic (chaos)");
            #[allow(unreachable_code)]
            result
        };

        return JoinHandle {
            inner: Some(tokio::task::spawn(panic_future)),
            chaos_state: JoinHandleChaos::None
        };
    }

    if chaos::should_fail(ChaosOperation::TaskDeadlock) {
        let deadlock_future = async move {
            std::future::pending::<()>().await;
            #[allow(unreachable_code)]
            future.await
        };

        return JoinHandle {
            inner: Some(tokio::task::spawn(deadlock_future)),
            chaos_state: JoinHandleChaos::None
        };
    }

    if chaos::should_fail(ChaosOperation::TaskStarvation) {
        let starved_future = async move {
            for _ in 0..10 {
                tokio::task::yield_now().await;
            }
            future.await
        };

        return JoinHandle {
            inner: Some(tokio::task::spawn(starved_future)),
            chaos_state: JoinHandleChaos::None
        };
    }

    JoinHandle {
        inner: Some(tokio::task::spawn(future)),
        chaos_state: JoinHandleChaos::None
    }
}

pub fn spawn_local<F>(future: F) -> JoinHandle<F::Output>
where F: Future + 'static, F::Output: 'static {
    if chaos::should_fail(ChaosOperation::TaskSpawnLocal) {
        return JoinHandle {
            inner: None,
            chaos_state: JoinHandleChaos::AlwaysFail
        };
    }

    JoinHandle {
        inner: Some(tokio::task::spawn_local(future)),
        chaos_state: JoinHandleChaos::None
    }
}

pub fn spawn_blocking<F, R>(f: F) -> JoinHandle<R>
where F: FnOnce() -> R + Send + 'static, R: Send + 'static {
    if chaos::should_fail(ChaosOperation::TaskSpawnBlocking) {
        return JoinHandle {
            inner: None,
            chaos_state: JoinHandleChaos::AlwaysFail
        };
    }

    if chaos::should_fail(ChaosOperation::ThreadPoolExhaustion) {
        let delayed_fn = move || {
            std::thread::sleep(Duration::from_secs(10));
            f()
        };

        return JoinHandle {
            inner: Some(tokio::task::spawn_blocking(delayed_fn)),
            chaos_state: JoinHandleChaos::None
        };
    }

    if chaos::should_fail(ChaosOperation::TaskBlock) {
        let blocking_fn = move || {
            std::thread::sleep(Duration::from_secs(60));
            f()
        };

        return JoinHandle {
            inner: Some(tokio::task::spawn_blocking(blocking_fn)),
            chaos_state: JoinHandleChaos::None
        }
    }

    JoinHandle {
        inner: Some(tokio::task::spawn_blocking(f)),
        chaos_state: JoinHandleChaos::None
    }
}

pub fn block_in_place<F, R>(f: F) -> R
where F: FnOnce() -> R {
    if chaos::should_fail(ChaosOperation::TaskBlock) {
        std::thread::sleep(Duration::from_secs(1));
    }

    tokio::task::block_in_place(f)
}

pub async fn yield_now() {
    if chaos::should_fail(ChaosOperation::TaskYieldNever) {
        return;
    }

    if chaos::should_fail(ChaosOperation::TaskYield) {
        for _ in 0..5 {
            tokio::task::yield_now().await;
        }

        return;
    }

    if chaos::should_fail(ChaosOperation::TaskStarvation) {
        for _ in 0..100 {
            tokio::task::yield_now().await;
        }

        return;
    }

    tokio::task::yield_now().await;
}

pub struct JoinHandle<T> {
    inner: Option<tokio::task::JoinHandle<T>>,
    chaos_state: JoinHandleChaos
}

enum JoinHandleChaos {
    None,
    AlwaysFail,
    AlwaysPanic,
    Timeout
}

impl<T> JoinHandle<T> {
    pub fn abort(&self) {
        if chaos::should_fail(ChaosOperation::TaskAbort) {
            return;
        }

        if let Some(ref inner) = self.inner {
            inner.abort();
        }
    }

    pub fn is_finished(&self) -> bool {
        if let JoinHandleChaos::AlwaysFail = self.chaos_state {
            return true;
        }

        self.inner.as_ref().map(|h| h.is_finished()).unwrap_or(true)
    }

    pub async fn cancel(self) -> Option<T> {
        if let Some(inner) = self.inner {
            match inner.await {
                Ok(value) => Some(value),
                Err(_) => None
            }
        }
        else {
            None
        }
    }
}

impl<T> Future for JoinHandle<T> {
    type Output = Result<T, JoinError>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match self.chaos_state {
            JoinHandleChaos::AlwaysFail => {
                return Poll::Ready(Err(JoinError::Cancelled));
            }
            JoinHandleChaos::AlwaysPanic => {
                return Poll::Ready(Err(JoinError::Panic));
            }
            JoinHandleChaos::Timeout => {
                return Poll::Pending;
            }
            JoinHandleChaos::None => {}
        }

        if chaos::should_fail(ChaosOperation::TaskJoinError) {
            return Poll::Ready(Err(JoinError::Cancelled));
        }

        if chaos::should_fail(ChaosOperation::TaskJoinTimeout) {
            return Poll::Pending;
        }

        if let Some(ref mut inner) = self.inner {
            Pin::new(inner).poll(cx).map(|result| result.map_err(|e| {
                if e.is_cancelled() {
                    JoinError::Cancelled
                }
                else if e.is_panic() {
                    JoinError::Panic
                }
                else {
                    JoinError::Cancelled
                }
            }))
        }
        else {
            Poll::Ready(Err(JoinError::Cancelled))
        }
    }
}

#[derive(Debug)]
pub enum JoinError {
    Cancelled,
    Panic
}

impl JoinError {
    pub fn is_cancelled(&self) -> bool {
        matches!(self, JoinError::Cancelled)
    }

    pub fn is_panic(&self) -> bool {
        matches!(self, JoinError::Panic)
    }
}

pub struct JoinSet<T> {
    inner: tokio::task::JoinSet<T>,
    chaos_state: JoinSetChaos
}

struct JoinSetChaos {
    fail_rate: f32
}

impl<T> JoinSet<T> {
    pub fn new() -> Self {
        Self {
            inner: tokio::task::JoinSet::new(),
            chaos_state: JoinSetChaos {
                fail_rate: if chaos::should_fail(ChaosOperation::JoinError) { 0.1 } else { 0.0 }
            }
        }
    }

    pub fn spawn<F>(&mut self, task: F)
    where F: Future<Output = T> + Send + 'static, T: Send + 'static {
        if chaos::should_fail(ChaosOperation::TaskSpawn) {
            return;
        }

        if self.chaos_state.fail_rate > 0.0 && rand::random::<f32>() < self.chaos_state.fail_rate {
            let failing_task = async move {
                panic!("fracture: JoinSet task panic (chaos)");
            };
            self.inner.spawn(failing_task);
            return;
        }

        self.inner.spawn(task);
    }

    pub fn spawn_local<F>(&mut self, task: F)
    where F: Future<Output = T> + 'static, T: 'static {
        self.inner.spawn_local(task);
    }

    pub async fn join_next(&mut self) -> Option<Result<T, JoinError>>
    where T: 'static {
        if chaos::should_fail(ChaosOperation::JoinError) {
            return Some(Err(JoinError::Panic));
        }

        self.inner.join_next().await.map(|result| result.map_err(|e| {
            if e.is_cancelled() {
                JoinError::Cancelled
            }
            else {
                JoinError::Panic
            }
        }))
    }

    pub fn abort_all(&mut self) where T: 'static {
        self.inner.abort_all();
    }

    pub fn detatch_all(&mut self) where T: 'static {
        self.inner.detach_all();
    }

    pub async fn shutdown(&mut self) where T: 'static {
        self.inner.shutdown().await;
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }
}

impl<T> Default for JoinSet<T> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct LocalSet {
    inner: tokio::task::LocalSet,
    chaos_enabled: bool
}

impl LocalSet {
    pub fn new() -> Self {
        Self {
            inner: tokio::task::LocalSet::new(),
            chaos_enabled: chaos::should_fail(ChaosOperation::LocalSetRun)
        }
    }

    pub fn spawn_local<F>(&self, future: F) -> JoinHandle<F::Output>
    where F: Future + 'static, F::Output: 'static {
        if self.chaos_enabled {
            return JoinHandle {
                inner: None,
                chaos_state: JoinHandleChaos::AlwaysFail
            }
        }

        JoinHandle {
            inner: Some(self.inner.spawn_local(future)),
            chaos_state: JoinHandleChaos::None
        }
    }
}

impl Default for LocalSet {
    fn default() -> Self {
        Self::new()
    }
}

pub struct AbortHandle {
    inner: tokio::task::AbortHandle
}

impl AbortHandle {
    pub fn abort(&self) {
        self.inner.abort();
    }

    pub fn is_finished(&self) -> bool {
        self.inner.is_finished()
    }
}

pub fn unconstrained<F>(inner: F) -> Unconstrained<F> 
where F: Future {
    Unconstrained { inner }
}

pub struct Unconstrained<F> {
    inner: F
}

impl<F> Future for Unconstrained<F> 
where F: Future {
    type Output = F::Output;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        if chaos::should_fail(ChaosOperation::TaskScheduleDelay) {
            cx.waker().wake_by_ref();
            return Poll::Pending;
        }

        unsafe { self.map_unchecked_mut(|s| &mut s.inner).poll(cx) }
    }
}

pub fn set_priority(priority: TaskPriority) {
    match priority {
        TaskPriority::Low => {
            if chaos::should_fail(ChaosOperation::TaskPriorityInversion) {
                // Placeholder
            }
        }
        TaskPriority::Normal => {}
        TaskPriority::High => {
            // Placeholder
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TaskPriority {
    Low,
    Normal,
    High
}

pub struct LocalKey<T: 'static> {
    inner: tokio::task::LocalKey<T>
}

impl<T: 'static> LocalKey<T> {
    pub async fn scope<F>(&'static self, value: T, f: F) -> F::Output
    where F: Future {
        self.inner.scope(value, f).await
    }

    pub fn sync_scope<F, R>(&'static self, value: T, f: F) -> R
    where F: FnOnce() -> R {
        self.inner.sync_scope(value, f)
    }
}

pub fn consume_budget() -> Poll<()> {
    if chaos::should_fail(ChaosOperation::TaskStarvation) {
        return Poll::Pending;
    }

    Poll::Ready(())
}

pub async fn catch_unwind_async<F>(f: F) -> Result<F::Output, Box<dyn std::any::Any + Send>>
where F: Future + std::panic::UnwindSafe {
    if chaos::should_fail(ChaosOperation::TaskPanic) {
        return Err(Box::new("fracture: Simulated panic"))
    }

    Ok(f.await)
}