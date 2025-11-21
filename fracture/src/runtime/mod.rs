use std::cell::RefCell;
use std::future::Future;
use std::rc::Rc;

pub(crate) mod core;
pub(crate) mod task;
mod waker;
pub(crate) mod blocking;

use self::core::Core;

pub struct Runtime {
    core: Rc<RefCell<Core>>
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

impl Runtime {
    pub fn new() -> Self {
        Builder::new_multi_thread().build().unwrap()
    }

    fn with_seed(seed: u64) -> Self {
        Self {
            core: Rc::new(RefCell::new(Core::new(seed)))
        }
    }

    pub fn block_on<F: Future + 'static>(&self, future: F) -> F::Output
    where F::Output: 'static {
        let handle = Handle {
            core: Rc::downgrade(&self.core)
        };

        CONTEXT.with(|cx| *cx.borrow_mut() = Some(handle));

        let (tx, rx) = std::sync::mpsc::channel();
        let wrapped = async move {
            let res = future.await;
            let _ = tx.send(res);
        };

        self.core.borrow_mut().spawn(wrapped);

        loop {
            if let Ok(result) = rx.try_recv() {
                return result;
            }

            let steps = core::Core::tick(&self.core);

            if steps == 0 {
                let mut core = self.core.borrow_mut();
                if !core.has_pending_tasks() {
                    drop(core);
                    if let Ok(result) = rx.try_recv() { return result; }

                    panic!("fracture: Deadlock detected! No tasks ready and no timers pending.");
                }

                let (advanced, wakers) = core.advance_time();
                drop(core);

                // Wake all the wakers after dropping the borrow
                for waker in wakers {
                    waker.wake();
                }

                if !advanced {
                    if let Ok(result) = rx.try_recv() { return result; }

                    panic!("fracture: Deadlock detected! No tasks ready and no timers pending.");
                }
            }
        }
    }

    pub fn shutdown_timeout(&mut self, _duration: std::time::Duration) {
        self.core.borrow_mut().tasks.clear();
    }

    pub fn shutdown_background(self) {
        drop(self);
    }

    pub fn enter(&self) -> EnterGuard<'_> {
        let handle = Handle {
            core: Rc::downgrade(&self.core)
        };

        let prev = CONTEXT.with(|cx| {
            cx.borrow_mut().replace(handle)
        });

        EnterGuard {
            _runtime: self,
            prev_context: prev,
        }
    }

    pub fn spawn<F>(&self, future: F) -> crate::task::JoinHandle<F::Output>
    where
        F: Future + 'static,
        F::Output: 'static,
    {
        let _guard = self.enter();
        crate::task::spawn(future)
    }

    pub fn handle(&self) -> Handle {
        Handle {
            core: Rc::downgrade(&self.core)
        }
    }
}

pub struct EnterGuard<'a> {
    _runtime: &'a Runtime,
    prev_context: Option<Handle>,
}

impl Drop for EnterGuard<'_> {
    fn drop(&mut self) {
        CONTEXT.with(|cx| {
            *cx.borrow_mut() = self.prev_context.take();
        });
    }
}

#[derive(Clone)]
pub struct Handle {
    pub(crate) core: std::rc::Weak<RefCell<Core>>
}

impl Handle {
    pub fn current() -> Self {
        CONTEXT.with(|cx| {
            cx.borrow().clone().expect("fracture: Must be called from within a fracture runtime")
        })
    }

    pub fn try_current() -> Option<Self> {
        CONTEXT.with(|cx| {
            cx.borrow().clone()
        })
    }

    pub fn spawn<F>(&self, future: F) -> crate::task::JoinHandle<F::Output>
    where
        F: Future + 'static,
        F::Output: 'static,
    {
        let prev = CONTEXT.with(|cx| cx.borrow_mut().replace(self.clone()));
        let result = crate::task::spawn(future);
        CONTEXT.with(|cx| *cx.borrow_mut() = prev);
        result
    }

    pub fn spawn_blocking<F, R>(&self, f: F) -> crate::task::JoinHandle<R>
    where
        F: FnOnce() -> R + Send + 'static,
        R: Send + 'static,
    {
        let prev = CONTEXT.with(|cx| cx.borrow_mut().replace(self.clone()));
        let result = crate::task::spawn_blocking(f);
        CONTEXT.with(|cx| *cx.borrow_mut() = prev);
        result
    }

    pub fn block_on<F: Future + 'static>(&self, future: F) -> F::Output
    where
        F::Output: 'static
    {
        let core_rc = self.core.upgrade().expect("fracture: Runtime has been dropped");

        let prev = CONTEXT.with(|cx| cx.borrow_mut().replace(self.clone()));

        let (tx, rx) = std::sync::mpsc::channel();
        let wrapped = async move {
            let res = future.await;
            let _ = tx.send(res);
        };

        core_rc.borrow_mut().spawn(wrapped);

        loop {
            if let Ok(result) = rx.try_recv() {
                CONTEXT.with(|cx| *cx.borrow_mut() = prev);
                return result;
            }

            let steps = Core::tick(&core_rc);

            if steps == 0 {
                let mut core = core_rc.borrow_mut();
                if !core.has_pending_tasks() {
                    drop(core);
                    if let Ok(result) = rx.try_recv() {
                        CONTEXT.with(|cx| *cx.borrow_mut() = prev);
                        return result;
                    }
                    panic!("fracture: Deadlock detected! No tasks ready and no timers pending.");
                }

                let (advanced, wakers) = core.advance_time();
                drop(core);

                // Wake all the wakers after dropping the borrow
                for waker in wakers {
                    waker.wake();
                }

                if !advanced {
                    if let Ok(result) = rx.try_recv() {
                        CONTEXT.with(|cx| *cx.borrow_mut() = prev);
                        return result;
                    }
                    panic!("fracture: Deadlock detected! No tasks ready and no timers pending.");
                }
            }
        }
    }

    pub fn metrics(&self) -> RuntimeMetrics {
        RuntimeMetrics
    }
}

#[derive(Debug, Clone)]
pub struct RuntimeMetrics;

impl RuntimeMetrics {
    pub fn num_workers(&self) -> usize {
        1
    }

    pub fn num_blocking_threads(&self) -> usize {
        0
    }

    pub fn active_tasks_count(&self) -> usize {
        0 // Would need access to core to implement
    }
}

thread_local! {
    static CONTEXT: RefCell<Option<Handle>> = RefCell::new(None);
}

#[derive(Debug, Clone)]
pub struct Builder {
    worker_threads: Option<usize>,
    max_blocking_threads: Option<usize>,
    thread_name: Option<String>,
    thread_stack_size: Option<usize>,
    global_queue_interval: Option<u32>,
    event_interval: Option<u32>,
    enable_io: bool,
    enable_time: bool,
}

impl Builder {
    pub fn new_multi_thread() -> Self {
        Self {
            worker_threads: None,
            max_blocking_threads: None,
            thread_name: None,
            thread_stack_size: None,
            global_queue_interval: None,
            event_interval: None,
            enable_io: true,
            enable_time: true,
        }
    }

    pub fn new_current_thread() -> Self {
        Self::new_multi_thread()
    }

    pub fn worker_threads(&mut self, val: usize) -> &mut Self {
        self.worker_threads = Some(val);
        self
    }

    pub fn max_blocking_threads(&mut self, val: usize) -> &mut Self {
        self.max_blocking_threads = Some(val);
        self
    }

    pub fn thread_name(&mut self, val: impl Into<String>) -> &mut Self {
        self.thread_name = Some(val.into());
        self
    }

    pub fn thread_stack_size(&mut self, val: usize) -> &mut Self {
        self.thread_stack_size = Some(val);
        self
    }

    pub fn global_queue_interval(&mut self, val: u32) -> &mut Self {
        self.global_queue_interval = Some(val);
        self
    }

    pub fn event_interval(&mut self, val: u32) -> &mut Self {
        self.event_interval = Some(val);
        self
    }

    pub fn enable_all(&mut self) -> &mut Self {
        self.enable_io = true;
        self.enable_time = true;
        self
    }

    pub fn enable_io(&mut self) -> &mut Self {
        self.enable_io = true;
        self
    }

    pub fn enable_time(&mut self) -> &mut Self {
        self.enable_time = true;
        self
    }

    pub fn on_thread_start<F>(&mut self, _f: F) -> &mut Self
    where
        F: Fn() + Send + Sync + 'static,
    {
        self
    }

    pub fn on_thread_stop<F>(&mut self, _f: F) -> &mut Self
    where
        F: Fn() + Send + Sync + 'static,
    {
        self
    }

    pub fn on_thread_park<F>(&mut self, _f: F) -> &mut Self
    where
        F: Fn() + Send + Sync + 'static,
    {
        self
    }

    pub fn on_thread_unpark<F>(&mut self, _f: F) -> &mut Self
    where
        F: Fn() + Send + Sync + 'static,
    {
        self
    }

    pub fn build(&mut self) -> std::io::Result<Runtime> {
        let seed = std::env::var("FRACTURE_SEED")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or_else(rand::random);

        Ok(Runtime::with_seed(seed))
    }
}

impl Default for Builder {
    fn default() -> Self {
        Self::new_multi_thread()
    }
}