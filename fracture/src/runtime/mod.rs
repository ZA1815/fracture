use std::cell::RefCell;
use std::future::Future;
use std::rc::Rc;
use std::time::Duration;

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
        let seed = std::env::var("FRACTURE_SEED")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or_else(rand::random);

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