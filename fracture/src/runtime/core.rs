use std::collections::{BinaryHeap, VecDeque};
use std::future::Future;
use std::task::{Context, Poll, Waker};
use std::time::{Duration, SystemTime};
use rand::{SeedableRng};
use rand_chacha::ChaCha8Rng;
use slab::Slab;
use std::cell::RefCell;

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

#[derive(Eq, PartialEq)]
pub(crate) struct TimerEntry {
    pub deadline: Duration,
    pub waker: Waker,
    pub id: usize
}

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
        let handle = crate::runtime::Handle::current();
        if let Some(core_rc) = handle.core.upgrade() {
            let now = core_rc.borrow().current_time;
            Self(now)
        }
        else {
            Self(Duration::ZERO)
        }
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

    pub fn tick(&mut self) -> usize {
        let mut polled_count = 0;
        
        while let Some(timer) = self.timers.peek() {
            if timer.deadline <= self.current_time {
                let timer = self.timers.pop().unwrap();
                timer.waker.wake();
            }
            else {
                break;
            }
        }

        let count = self.ready_queue.len();
        for _ in 0..count {
            if let Some(task_id) = self.ready_queue.pop_front() {
                polled_count += 1;
                self.poll_task(task_id);
            }
        }

        polled_count
    }

    fn poll_task(&mut self, id: TaskId) {
        if !self.tasks.contains(id.0) {
            return;
        }

        let waker = make_waker(id);
        let mut cx = Context::from_waker(&waker);
        
        let task = self.tasks.get_mut(id.0).unwrap();

        let locals_ptr: *mut TaskLocals = &mut task.locals;
        
        CURRENT_TASK_LOCALS.with(|cell| {
            *cell.borrow_mut() = Some(locals_ptr);
        });

        let result = task.future.as_mut().poll(&mut cx);

        CURRENT_TASK_LOCALS.with(|cell| {
            *cell.borrow_mut() = None;
        });

        match result {
            Poll::Ready(()) => {
                self.tasks.remove(id.0);
            }
            Poll::Pending => {
                // Task is still alive, waker handles re-queueing
            }
        }
    }

    pub fn advance_time(&mut self) -> bool {
        if let Some(next_timer) = self.timers.peek() {
            let new_time = next_timer.deadline;

            self.current_time = new_time;

            while let Some(timer) = self.timers.peek() {
                if timer.deadline <= self.current_time {
                    let timer = self.timers.pop().unwrap();
                    timer.waker.wake();
                }
                else {
                    break;
                }
            }
            true
        }
        else {
            false
        }
    }

    pub fn has_pending_tasks(&self) -> bool {
        !self.tasks.is_empty()
    }
}