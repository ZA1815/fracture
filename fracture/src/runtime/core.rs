use std::collections::{BinaryHeap, VecDeque};
use std::future::Future;
use std::task::{Context, Poll, Waker};
use std::time::{Duration, SystemTime};
use rand::{SeedableRng};
use rand_chacha::ChaCha8Rng;
use slab::Slab;

use super::task::{Task, TaskId};
use super::waker::make_waker;

use crate::net::NetworkState;
use crate::fs::FileSystemState;
use crate::process::ProcessTable;
use crate::signal::SignalState;

pub(crate) struct Core {
    pub rng: ChaCha8Rng,
    pub tasks: Slab<Task>,
    pub ready_queue: VecDeque<TaskId>,
    pub current_time: Duration,
    pub timers: BinaryHeap<TimerEntry>,

    pub network: NetworkState,
    pub fs: FileSystemState,
    pub processes: ProcessTable,
    pub signals: SignalState
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
            fs: FileSystemState::new(),
            processes: ProcessTable::new(),
            signals: SignalState::new()
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

        match task.future.as_mut().poll(&mut cx) {
            Poll::Ready(()) => {
                self.tasks.remove(id.0);
            }
            Poll::Pending => {
                // Waker logic handles re-queueing
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