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

            let mut core = self.core.borrow_mut();
            let steps = core.tick();

            if steps == 0 {
                if !core.has_pending_tasks() {
                    if let Ok(result) = rx.try_recv() { return result; }

                    panic!("fracture: Deadlock detected! No tasks ready and no timers pending.");
                }

                if !core.advance_time() {
                    if let Ok(result) = rx.try_recv() { return result; }

                    panic!("fracture: Deadlock detected! No tasks ready and no timers pending.");
                }
            }
        }
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
        // In simulation mode, this is a no-op
        self
    }

    pub fn on_thread_stop<F>(&mut self, _f: F) -> &mut Self
    where
        F: Fn() + Send + Sync + 'static,
    {
        // In simulation mode, this is a no-op
        self
    }

    pub fn on_thread_park<F>(&mut self, _f: F) -> &mut Self
    where
        F: Fn() + Send + Sync + 'static,
    {
        // In simulation mode, this is a no-op
        self
    }

    pub fn on_thread_unpark<F>(&mut self, _f: F) -> &mut Self
    where
        F: Fn() + Send + Sync + 'static,
    {
        // In simulation mode, this is a no-op
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