use std::collections::{BinaryHeap, VecDeque};
use std::future::Future;
use std::task::{Context, Poll, Waker};
use std::time::{Duration, SystemTime};
use rand::{SeedableRng};
use rand_chacha::ChaCha8Rng;
use slab::Slab;
use std::cell::RefCell;
use std::rc::Rc;

use super::task::{Task, TaskId, TaskLocals};
use super::waker::make_waker;

use crate::net::NetworkState;
use crate::fs::FileSystemState;

thread_local! {
    static CURRENT_TASK_LOCALS: RefCell<Option<*mut TaskLocals>> = RefCell::new(None);
}

pub(crate) fn with_current_locals<F, R>(f: F) -> Option<R>
where F: FnOnce(&mut TaskLocals) -> R {
    CURRENT_TASK_LOCALS.with(|cell| {
        if let Some(ptr) = *cell.borrow() {
            unsafe { Some(f(&mut *ptr)) }
        }
        else {
            None
        }
    })
}

pub(crate) struct Core {
    pub rng: ChaCha8Rng,
    pub tasks: Slab<Task>,
    pub ready_queue: VecDeque<TaskId>,
    pub current_time: Duration,
    pub timers: BinaryHeap<TimerEntry>,

    pub network: NetworkState,
    pub fs: FileSystemState,
}

pub(crate) struct TimerEntry {
    pub deadline: Duration,
    pub waker: Waker,
    pub id: usize
}

impl PartialEq for TimerEntry {
    fn eq(&self, other: &Self) -> bool {
        self.deadline == other.deadline && self.id == other.id
    }
}

impl Eq for TimerEntry {}

impl Ord for TimerEntry {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.deadline.cmp(&self.deadline).then_with(|| other.id.cmp(&self.id))
    }
}

impl PartialOrd for TimerEntry {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Instant(pub(crate) Duration);

impl Instant {
    pub fn now() -> Self {
        if let Some(handle) = crate::runtime::Handle::try_current() {
            if let Some(core_rc) = handle.core.upgrade() {
                if let Ok(core) = core_rc.try_borrow() {
                    let now = core.current_time;
                    return Self(now);
                }
            }
        }
        Self(Duration::ZERO)
    }

    pub fn elapsed(&self) -> Duration {
        Self::now().duration_since(*self)
    }

    pub fn duration_since(&self, earlier: Instant) -> Duration {
        self.0.saturating_sub(earlier.0)
    }

    pub fn checked_add(&self, duration: Duration) -> Option<Instant> {
        self.0.checked_add(duration).map(Instant)
    }

    pub fn checked_sub(&self, duration: Duration) -> Option<Instant> {
        self.0.checked_sub(duration).map(Instant)
    }

    pub fn saturating_duration_since(&self, earlier: Instant) -> Duration {
        self.0.saturating_sub(earlier.0)
    }
}

impl std::ops::Add<Duration> for Instant {
    type Output = Instant;
    fn add(self, other: Duration) -> Instant {
        Instant(self.0 + other)
    }
}

impl std::ops::Sub<Duration> for Instant {
    type Output = Instant;
    fn sub(self, other: Duration) -> Instant {
        Instant(self.0 - other)
    }
}

impl std::ops::Sub<Instant> for Instant {
    type Output = Duration;
    fn sub(self, other: Instant) -> Duration {
        self.0 - other.0
    }
}

impl From<Duration> for Instant {
    fn from(d: Duration) -> Self {
        Instant(d)
    }
}

impl Core {
    pub fn new(seed: u64) -> Self {
        eprintln!("🎲 Fracture Runtime Initialized (Seed: {})", seed);

        Self {
            rng: ChaCha8Rng::seed_from_u64(seed),
            tasks: Slab::new(),
            ready_queue: VecDeque::new(),
            current_time: Duration::ZERO,
            timers: BinaryHeap::new(),
            network: NetworkState::new(),
            fs: FileSystemState::new()
        }
    }

    pub fn sys_now(&self) -> SystemTime {
        SystemTime::UNIX_EPOCH + self.current_time
    }

    pub fn spawn<F>(&mut self, future: F) -> TaskId
    where F: Future<Output = ()> + 'static {
        let future = Box::pin(future);

        let key = self.tasks.insert(Task::new(future, None, self.current_time));
        let id = TaskId(key);

        self.ready_queue.push_back(id);

        id
    }

    pub fn tick(core_rc: &Rc<RefCell<Core>>) -> usize {
        let mut polled_count = 0;

        // Wake expired timers
        {
            let mut core = core_rc.borrow_mut();
            eprintln!("[Core::tick] START - ready_queue: {}, tasks: {}, timers: {}",
                core.ready_queue.len(), core.tasks.len(), core.timers.len());
            while let Some(timer) = core.timers.peek() {
                if timer.deadline <= core.current_time {
                    let timer = core.timers.pop().unwrap();
                    drop(core); // Drop borrow before waking
                    timer.waker.wake();
                    core = core_rc.borrow_mut();
                }
                else {
                    break;
                }
            }
        }

        // Poll ready tasks
        loop {
            let task_id = {
                let mut core = core_rc.borrow_mut();
                core.ready_queue.pop_front()
            };

            match task_id {
                Some(id) => {
                    eprintln!("[Core::tick] Polling task {:?}", id);
                    polled_count += 1;
                    Self::poll_task(core_rc, id);
                }
                None => break,
            }
        }

        eprintln!("[Core::tick] END - polled {} tasks", polled_count);
        polled_count
    }

    fn poll_task(core_rc: &Rc<RefCell<Core>>, id: TaskId) {
        // Swap the task with a dummy to keep it at the same index
        let mut task = {
            let mut core = core_rc.borrow_mut();
            if !core.tasks.contains(id.0) {
                return;
            }
            // Create a dummy task to temporarily occupy the slot
            let dummy = Task::new(Box::pin(async {}), None, Duration::ZERO);
            std::mem::replace(&mut core.tasks[id.0], dummy)
        };

        // Poll the task WITHOUT holding the core borrow
        let waker = make_waker(id);
        let mut cx = Context::from_waker(&waker);

        let locals_ptr: *mut TaskLocals = &mut task.locals;

        CURRENT_TASK_LOCALS.with(|cell| {
            *cell.borrow_mut() = Some(locals_ptr);
        });

        let result = task.future.as_mut().poll(&mut cx);

        CURRENT_TASK_LOCALS.with(|cell| {
            *cell.borrow_mut() = None;
        });

        // Handle the result
        match result {
            Poll::Ready(()) => {
                // Task completed, remove it (and the dummy)
                let mut core = core_rc.borrow_mut();
                core.tasks.remove(id.0);
                eprintln!("[Core::poll_task] Task {:?} completed", id);
            }
            Poll::Pending => {
                // Put the task back at its original index
                let mut core = core_rc.borrow_mut();
                core.tasks[id.0] = task;
                eprintln!("[Core::poll_task] Task {:?} still pending", id);
            }
        }
    }

    pub fn advance_time(&mut self) -> (bool, Vec<Waker>) {
        if let Some(next_timer) = self.timers.peek() {
            let new_time = next_timer.deadline;

            self.current_time = new_time;

            // Collect wakers to return (caller will wake them after dropping the borrow)
            let mut wakers_to_wake = Vec::new();
            while let Some(timer) = self.timers.peek() {
                if timer.deadline <= self.current_time {
                    let timer = self.timers.pop().unwrap();
                    wakers_to_wake.push(timer.waker);
                }
                else {
                    break;
                }
            }

            (true, wakers_to_wake)
        }
        else {
            (false, Vec::new())
        }
    }

    pub fn has_pending_tasks(&self) -> bool {
        !self.tasks.is_empty()
    }
}