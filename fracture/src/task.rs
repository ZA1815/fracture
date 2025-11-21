use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::task::{Context, Poll};
use std::time::Duration;
use rand::Rng;

use crate::chaos::{self, ChaosOperation};
use crate::runtime::Handle;
use crate::runtime::task::TaskId;
use crate::sync::mpsc::{UnboundedReceiver, UnboundedSender, unbounded};
use crate::sync::oneshot;
use crate::time::sleep;

pub fn spawn<F>(future: F) -> JoinHandle<F::Output>
where F: Future + Send + 'static, F::Output: Send + 'static {
    let finished = Arc::new(AtomicBool::new(false));

    if chaos::should_fail(ChaosOperation::TaskSpawn) {
        return JoinHandle {
            rx: None,
            chaos_state: JoinHandleChaos::AlwaysFail,
            task_id: None,
            abort_handle: AbortHandle { task_id: None, finished: finished.clone() },
            finished,
        };
    }

    let (tx, rx) = oneshot::channel();
    let finished_flag = finished.clone();

    let wrapped_future = async move {
        if chaos::should_fail(ChaosOperation::TaskScheduleDelay) {
            let delay = {
                let handle = Handle::current();
                if let Some(core_rc) = handle.core.upgrade() {
                    let mut core = core_rc.borrow_mut();
                    let ms = core.rng.gen_range(1..100);
                    Some(Duration::from_millis(ms))
                }
                else {
                    None
                }
            };

            if let Some(d) = delay {
                sleep(d).await;
            }
        }

        if chaos::should_fail(ChaosOperation::TaskStarvation) {
            for _ in 0..10 {
                yield_now().await;
            }
        }

        if chaos::should_fail(ChaosOperation::TaskPanic) {
            panic!("fracture: Task panic injected (chaos)");
        }

        if chaos::should_fail(ChaosOperation::TaskDeadlock) {
            std::future::pending::<()>().await;
        }

        let output = future.await;

        tx.send(output);
        finished_flag.store(true, Ordering::Release);
    };

    let handle = Handle::current();
    let core_rc = handle.core.upgrade().expect("fracture: Runtime dropped");
    let mut core = core_rc.borrow_mut();

    let id = core.spawn(wrapped_future);

    JoinHandle {
        rx: Some(rx),
        chaos_state: JoinHandleChaos::None,
        task_id: Some(id),
        abort_handle: AbortHandle { task_id: Some(id), finished: finished.clone() },
        finished,
    }
}

pub fn spawn_local<F>(future: F) -> JoinHandle<F::Output>
where F: Future + 'static, F::Output: Send + 'static {
    let future = unsafe { 
        std::mem::transmute::<
            Pin<Box<dyn Future<Output=F::Output>>>, 
            Pin<Box<dyn Future<Output=F::Output> + Send>>
        >(Box::pin(future)) 
    };
    
    spawn(future)
}

pub fn spawn_blocking<F, R>(f: F) -> JoinHandle<R>
where F: FnOnce() -> R + Send + 'static, R: Send + 'static {
    let finished = Arc::new(AtomicBool::new(false));

    if chaos::should_fail(ChaosOperation::TaskSpawnBlocking) {
        return JoinHandle {
            rx: None,
            chaos_state: JoinHandleChaos::AlwaysFail,
            task_id: None,
            abort_handle: AbortHandle { task_id: None, finished: finished.clone() },
            finished,
        };
    }

    let (tx, rx) = oneshot::channel();
    let finished_flag = finished.clone();

    std::thread::spawn(move || {
        if chaos::should_fail(ChaosOperation::ThreadPoolExhaustion) {
            std::thread::sleep(Duration::from_secs(1));
        }

        let result = f();
        tx.send(result);
        finished_flag.store(true, Ordering::Release);
    });

    JoinHandle {
        rx: Some(rx),
        chaos_state: JoinHandleChaos::None,
        task_id: None,
        abort_handle: AbortHandle { task_id: None, finished: finished.clone() },
        finished,
    }
}

pub async fn yield_now() {
    if chaos::should_fail(ChaosOperation::TaskYieldNever) {
        return;
    }

    struct YieldNow {
        yielded: bool
    }

    impl Future for YieldNow {
        type Output = ();

        fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
            if self.yielded {
                Poll::Ready(())
            }
            else {
                self.yielded = true;
                cx.waker().wake_by_ref();
                Poll::Pending
            }
        }
    }

    YieldNow { yielded: false }.await;
}

pub fn block_in_place<F, R>(f: F) -> R
where F: FnOnce() -> R {
    if chaos::should_fail(ChaosOperation::TaskBlock) {
        std::thread::sleep(Duration::from_secs(1));
    }

    f()
}

pub struct JoinHandle<T> {
    rx: Option<oneshot::Receiver<T>>,
    chaos_state: JoinHandleChaos,
    task_id: Option<crate::runtime::task::TaskId>,
    abort_handle: AbortHandle,
    finished: Arc<AtomicBool>,
}

#[derive(Clone)]
pub struct AbortHandle {
    task_id: Option<crate::runtime::task::TaskId>,
    finished: Arc<AtomicBool>,
}

pub enum JoinHandleChaos {
    None,
    AlwaysFail,
    AlwaysPanic,
    Timeout
}

impl<T> JoinHandle<T> {
    pub fn abort(&self) {
        self.abort_handle.abort();
    }

    pub fn is_finished(&self) -> bool {
        if let JoinHandleChaos::AlwaysFail = self.chaos_state {
            return true;
        }

        self.finished.load(Ordering::Acquire)
    }

    pub fn id(&self) -> Option<TaskId> {
        self.task_id
    }

    pub fn abort_handle(&self) -> AbortHandle {
        self.abort_handle.clone()
    }
}

impl AbortHandle {
    pub fn abort(&self) {
        if chaos::should_fail(ChaosOperation::TaskAbort) {
            return;
        }

        if let Some(id) = self.task_id {
            let handle = Handle::current();
            if let Some(core_rc) = handle.core.upgrade() {
                let mut core = core_rc.borrow_mut();
                core.tasks.remove(id.0);
            }
        }

        self.finished.store(true, Ordering::Release);
    }

    pub fn is_finished(&self) -> bool {
        self.finished.load(Ordering::Acquire)
    }

    pub fn id(&self) -> Option<TaskId> {
        self.task_id
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

        match self.rx.as_mut() {
            Some(rx) => {
                match Pin::new(rx).poll(cx) {
                    Poll::Ready(Ok(val)) => Poll::Ready(Ok(val)),
                    Poll::Ready(Err(_)) => Poll::Ready(Err(JoinError::Cancelled)),
                    Poll::Pending => Poll::Pending
                }
            }
            None => Poll::Ready(Err(JoinError::Cancelled))
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

impl std::fmt::Display for JoinError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JoinError::Cancelled => write!(f, "fracture: Task was cancelled"),
            JoinError::Panic => write!(f, "fracture: Task panicked")
        }
    }
}

impl std::error::Error for JoinError {}

pub struct JoinSet<T> {
    tasks: HashMap<u64, TaskId>,
    tx: UnboundedSender<(u64, Result<T, JoinError>)>,
    rx: UnboundedReceiver<(u64, Result<T, JoinError>)>,
    next_key: u64
}

impl<T> JoinSet<T> {
    pub fn new() -> Self {
        let (tx, rx) = unbounded();
        Self { tasks: HashMap::new(), tx, rx, next_key: 0 }
    }

    pub fn spawn<F>(&mut self, task: F)
    where F: Future<Output = T> + Send + 'static, T: Send + 'static {
        if chaos::should_fail(ChaosOperation::TaskSpawn) {
            return;
        }

        let handle = spawn(task);

        let key = self.next_key;
        self.next_key += 1;

        if let Some(task_id) = handle.id() {
            self.tasks.insert(key, task_id);
        }

        let tx = self.tx.clone();
        spawn(async move {
            let result = handle.await;
            let _ = tx.send((key, result));
        });
    }

    pub async fn join_next(&mut self) -> Option<Result<T, JoinError>> {
        if self.tasks.is_empty() {
            return None;
        }

        match self.rx.recv().await {
            Some((key, result)) => {
                self.tasks.remove(&key);
                Some(result)
            }
            None => None
        }
    }

    pub fn abort_all(&mut self) {
        for task_id in self.tasks.values() {
            let handle = Handle::current();
            if let Some(core_rc) = handle.core.upgrade() {
                let mut core = core_rc.borrow_mut();
                if core.tasks.contains(task_id.0) {
                    core.tasks.remove(task_id.0);
                }
            }
        }
    }

    pub fn len(&self) -> usize {
        self.tasks.len()
    }

    pub fn is_empty(&self) -> bool {
        self.tasks.is_empty()
    }
    
    pub fn shutdown(&mut self) {
        self.abort_all();
    }
}

impl<T> Default for JoinSet<T> {
    fn default() -> Self {
        Self::new()
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
    #[doc(hidden)]
    pub inner: &'static std::thread::LocalKey<std::cell::RefCell<Option<T>>>,
    #[doc(hidden)]
    pub init: fn() -> T
}

impl<T: 'static> LocalKey<T> {
    pub async fn scope<F>(&'static self, value: T, f: F) -> F::Output
    where F: Future {
        let old_value = crate::runtime::core::with_current_locals(|locals| {
            locals.insert(value)
        }).flatten();

        let result = f.await;

        crate::runtime::core::with_current_locals(|locals| {
            if let Some(old) = old_value {
                locals.insert(old);
            }
            else {
                locals.remove::<T>();
            }
        });

        result
    }

    pub fn with<F, R>(&'static self, f: F) -> R
    where F: FnOnce(&T) -> R {
        crate::runtime::core::with_current_locals(|locals| {
            if let Some(val) = locals.get::<T>() {
                f(val)
            }
            else {
                let val = (self.init)();
                f(&val)
            }
        }).expect("fracture: task_local access outside of task context")
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