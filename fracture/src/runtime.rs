use std::cell::RefCell;
use std::collections::{BinaryHeap, VecDeque};
use std::future::Future;
use std::pin::Pin;
use std::rc::Rc;
use std::task::{Context, Poll, Wake, Waker};
use std::time::Duration;
use tokio::time::Instant;
use parking_lot::Mutex;
use std::sync::Arc;

use crate::chaos::{self, ChaosOperation};
use crate::task::{JoinHandle, JoinHandleChaos};

pub struct Runtime {
    scheduler: Rc<RefCell<Scheduler>>,
    config: RuntimeConfig,
    metrics: Arc<Mutex<RuntimeMetrics>>
}

#[derive(Clone)]
pub struct RuntimeConfig {
    pub worker_threads: usize,
    pub max_blocking_threads: usize,
    pub thread_stack_size: usize,
    pub thread_name: String,
    pub enable_io: bool,
    pub enable_time: bool,
    pub max_io_events_per_tick: usize,
    pub event_interval: Duration,
    pub time_resolution: Duration
}

impl Default for RuntimeConfig {
    fn default() -> Self {
        Self {
            worker_threads: 1,
            max_blocking_threads: 512,
            thread_stack_size: 2 * 1024 * 1024,
            thread_name: "fracture-worker".to_string(),
            enable_io: true,
            enable_time: true,
            max_io_events_per_tick: 1024,
            event_interval: Duration::from_millis(1),
            time_resolution: Duration::from_micros(1),
        }
    }
}

#[derive(Default, Clone)]
pub struct RuntimeMetrics {
    pub tasks_spawned: u64,
    pub tasks_completed: u64,
    pub tasks_cancelled: u64,
    pub poll_count: u64,
    pub yield_count: u64,
    pub block_count: u64,
    pub injection_count: u64,
    pub steal_count: u64
}

pub struct Builder {
    config: RuntimeConfig,
    enable_all: bool
}

impl Builder {
    pub fn new_current_thread() -> Self {
        Self {
            config: RuntimeConfig {
                worker_threads: 1,
                ..Default::default()
            },
            enable_all: false
        }
    }

    pub fn new_multi_thread() -> Self {
        Self {
            config: RuntimeConfig{
                worker_threads: num_cpus::get(),
                ..Default::default()
            },
            enable_all: false
        }
    }

    pub fn worker_threads(mut self, val: usize) -> Self {
        self.config.worker_threads = val.max(1);
        self
    }

    pub fn max_blocking_threads(mut self, val: usize) -> Self {
        self.config.max_blocking_threads = val;
        self
    }

    pub fn thread_name(mut self, val: impl Into<String>) -> Self {
        self.config.thread_name = val.into();
        self
    }

    pub fn thread_stack_size(mut self, val: usize) -> Self {
        self.config.thread_stack_size = val;
        self
    }

    pub fn enable_all(mut self) -> Self {
        self.enable_all = true;
        self.config.enable_io = true;
        self.config.enable_time = true;
        self
    }

    pub fn enable_io(mut self) -> Self {
        self.config.enable_io = true;
        self
    }

    pub fn enable_time(mut self) -> Self {
        self.config.enable_time = true;
        self
    }

    pub fn max_io_events_per_tick(mut self, val: usize) -> Self {
        self.config.max_io_events_per_tick = val;
        self
    }

    pub fn event_interval(mut self, val: Duration) -> Self {
        self.config.event_interval = val;
        self
    }

    pub fn build(self) -> std::io::Result<Runtime> {
        if chaos::should_fail(ChaosOperation::RuntimePanic) {
            panic!("fracture: Runtime build failed (chaos)");
        }

        if chaos::should_fail(ChaosOperation::RuntimeThreadExhaustion) {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "fracture: Thread exhaustion (chaos)",
            ));
        }

        Ok(Runtime {
            scheduler: Rc::new(RefCell::new(Scheduler::new(self.config.clone()))),
            config: self.config,
            metrics: Arc::new(Mutex::new(RuntimeMetrics::default()))
        })
    }
}

impl Default for Builder {
    fn default() -> Self {
        Self::new_multi_thread()
    }
}

type BoxFuture = Pin<Box<dyn Future<Output = ()> + 'static>>;

struct Task {
    id: TaskId,
    future: BoxFuture,
    priority: TaskPriority,
    state: TaskState,
    waker: Option<Waker>,
    spawn_time: Instant,
    poll_count: u64,
    budget: CooperativeBudget
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct TaskId(u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum TaskPriority {
    Low = 0,
    Normal = 1,
    High = 2
}

impl Default for TaskPriority {
    fn default() -> Self {
        TaskPriority::Normal
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TaskState {
    Ready,
    Polling,
    Waiting,
    Completed,
    Cancelled
}

struct CooperativeBudget {
    remaining: u32,
    total: u32
}

impl CooperativeBudget {
    fn new(total: u32) -> Self {
        Self { remaining: total, total }
    }

    fn consume(&mut self) -> bool {
        if self.remaining > 0 {
            self.remaining -= 1;
            true
        }
        else {
            false
        }
    }

    fn reset(&mut self) {
        self.remaining = self.total;
    }

    fn is_exhausted(&self) -> bool {
        self.remaining == 0
    }
}

impl Default for CooperativeBudget {
    fn default() -> Self {
        Self::new(128)
    }
}

struct Scheduler {
    config: RuntimeConfig,
    next_task_id: u64,
    ready_queue: BinaryHeap<QueuedTask>,
    waiting_tasks: Vec<Task>,
    current_task: Option<TaskId>,
    is_running: bool,
    current_time: Instant,
    time_wheel: Vec<Vec<TimerEntry>>,
    blocking_queue: VecDeque<BoxFuture>,
    active_blocking_tasks: usize
}

struct QueuedTask {
    id: TaskId,
    priority: TaskPriority,
    enqueue_time: Instant
}

impl PartialEq for QueuedTask {
    fn eq(&self, other: &Self) -> bool {
        self.priority == other.priority && self.id == other.id
    }
}

impl Eq for QueuedTask {}

impl PartialOrd for QueuedTask {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for QueuedTask {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.priority.cmp(&other.priority).then_with(|| other.enqueue_time.cmp(&self.enqueue_time))
    }
}

#[derive(Clone)]
struct TimerEntry {
    deadline: Instant,
    task_id: TaskId
}

impl Scheduler {
    fn new(config: RuntimeConfig) -> Self {
        Self {
            config,
            next_task_id: 0,
            ready_queue: BinaryHeap::new(),
            waiting_tasks: Vec::new(),
            current_task: None,
            is_running: false,
            current_time: Instant::now(),
            time_wheel: vec![Vec::new(); 256],
            blocking_queue: VecDeque::new(),
            active_blocking_tasks: 0,
        }
    }

    fn spawn_task(&mut self, future: BoxFuture, priority: TaskPriority) -> TaskId {
        let id = TaskId(self.next_task_id);
        self.next_task_id += 1;

        let task = Task {
            id,
            future,
            priority,
            state: TaskState::Ready,
            waker: None,
            spawn_time: self.current_time,
            poll_count: 0,
            budget: CooperativeBudget::default()
        };

        self.waiting_tasks.push(task);
        self.enqueue_task(id, priority);

        id
    }

    fn enqueue_task(&mut self, id: TaskId, priority: TaskPriority) {
        if chaos::should_fail(ChaosOperation::TaskScheduleDelay) {
            return;
        }

        let queued = QueuedTask {
            id,
            priority,
            enqueue_time: self.current_time
        };

        self.ready_queue.push(queued);
    }

    fn poll_task(&mut self, task_id: TaskId) -> bool {
        let task_idx = match self.waiting_tasks.iter().position(|t| t.id == task_id) {
            Some(idx) => idx,
            None => return false
        };

        let task_priority = self.waiting_tasks[task_idx].priority;

        let task = &mut self.waiting_tasks[task_idx];

        if task.state == TaskState::Completed || task.state == TaskState::Cancelled {
            return false;
        }

        if !task.budget.consume() {
            task.budget.reset();
            self.enqueue_task(task_id, task_priority);
            return true;
        }

        task.state = TaskState::Polling;
        task.poll_count += 1;
        self.current_task = Some(task_id);

        let waker = create_waker(task_id);
        let mut context = Context::from_waker(&waker);

        let poll_result = task.future.as_mut().poll(&mut context);

        match poll_result {
            Poll::Ready(()) => {
                task.state = TaskState::Completed;
                self.current_task = None;
                false
            }
            Poll::Pending => {
                task.state = TaskState::Waiting;
                task.waker = Some(waker);
                self.current_task = None;
                true
            }
        }
    }

    fn run_until_idle(&mut self) {
        if chaos::should_fail(ChaosOperation::RuntimeDeadlock) {
            loop {
                std::thread::yield_now();
            }
        }

        self.is_running = true;

        while let Some(queued) = self.ready_queue.pop() {
            if chaos::should_fail(ChaosOperation::TaskStarvation) {
                if rand::random::<f32>() < 0.1 {
                    continue;
                }
            }

            let still_pending = self.poll_task(queued.id);

            if still_pending {
                // Placeholder
            }

            self.advance_timers();

            self.process_blocking_tasks();
        }

        self.is_running = false
    }

    fn advance_timers(&mut self) {
        if !self.config.enable_time {
            return;
        }

        self.current_time += self.config.time_resolution;

        let bucket_idx = (self.current_time.elapsed().as_millis() % 256) as usize;
        let expired_timers: Vec<_> = self.time_wheel[bucket_idx]
            .drain(..)
            .filter(|entry| entry.deadline <= self.current_time)
            .collect();

        for entry in expired_timers {
            if let Some(task) = self.waiting_tasks.iter_mut().find(|t| t.id == entry.task_id) {
                if let Some(waker) = task.waker.take() {
                    waker.wake();
                }
            }
        }
    }

    fn process_blocking_tasks(&mut self) {
        if self.active_blocking_tasks >= self.config.max_blocking_threads {
            return;
        }

        while let Some(_future) = self.blocking_queue.pop_front() {
            // Placeholder, actually submit task to a thread
            self.active_blocking_tasks += 1;
            break;
        }
    }

    fn wake_task(&mut self, task_id: TaskId) {
        let mut task_data: Option<(TaskId, TaskPriority)> = None;
        if let Some(task) = self.waiting_tasks.iter_mut().find(|t| t.id == task_id) {
            if task.state == TaskState::Waiting {
                task.state = TaskState::Ready;
                task_data = Some((task_id, task.priority));
            }
        }

        if let Some((id, priority)) = task_data {
            self.enqueue_task(id, priority);
        }
    }
}

struct TaskWaker {
    task_id: TaskId
}

impl Wake for TaskWaker {
    fn wake(self: Arc<Self>) {
        // This would notify the scheduler
        // Placeholder
    }

    fn wake_by_ref(self: &Arc<Self>) {
        // This would notify the scheduler
        // Placeholder
    }
}

fn create_waker(task_id: TaskId) -> Waker {
    let task_waker = Arc::new(TaskWaker { task_id });
    task_waker.into()
}

pub struct Handle {
    scheduler: Rc<RefCell<Scheduler>>,
    metrics: Arc<Mutex<RuntimeMetrics>>
}

impl Handle {
    fn current() -> Self {
        // Thread-local handle storage would go here
        unimplemented!("Handle::current() requires thread-local storage")
    }

    pub fn spawn<F>(&self, future: F) -> JoinHandle<F::Output>
    where F: Future + Send + 'static, F::Output: Send + 'static {
        if chaos::should_fail(ChaosOperation::TaskSpawn) {
            return JoinHandle {
                inner: None,
                chaos_state: JoinHandleChaos::AlwaysFail
            };
        }

        let mut metrics = self.metrics.lock();
        metrics.tasks_spawned += 1;

        let future = Box::pin(async move {
            future.await;
        });

        let mut scheduler = self.scheduler.borrow_mut();
        scheduler.spawn_task(future, TaskPriority::Normal);

        JoinHandle {
            inner: None, // Need to implement proper join mechanism
            chaos_state: JoinHandleChaos::None
        }
    }

    pub fn spawn_blocking<F, R>(&self, f: F) -> JoinHandle<R>
    where F: FnOnce() -> R + Send + 'static, R: Send + 'static {
        if chaos::should_fail(ChaosOperation::TaskSpawnBlocking) {
            return JoinHandle {
                inner: None,
                chaos_state: JoinHandleChaos::AlwaysFail
            };
        }

        let future = Box::pin(async move {
            f();
        });

        let mut scheduler = self.scheduler.borrow_mut();
        scheduler.blocking_queue.push_back(future);

        JoinHandle {
            inner: None,
            chaos_state: JoinHandleChaos::None
        }
    }

    pub fn block_on<F: Future>(&self, future: F) -> F::Output {
        if chaos::should_fail(ChaosOperation::RuntimeBlock) {
            std::thread::sleep(Duration::from_secs(60));
        }

        // Placeholder
        unimplemented!("block_on requires executor integration")
    }

    pub fn enter<F, R>(&self, f: F) -> R
    where F: FnOnce() -> R {
        f()
    }
}

impl Clone for Handle {
    fn clone(&self) -> Self {
        Self {
            scheduler: Rc::clone(&self.scheduler),
            metrics: Arc::clone(&self.metrics)
        }
    }
}

impl Runtime {
    pub fn new() -> std::io::Result<Self> {
        Builder::new_multi_thread().build()
    }

    pub fn handle(&self) -> Handle {
        Handle {
            scheduler: Rc::clone(&self.scheduler),
            metrics: Arc::clone(&self.metrics)
        }
    }

    pub fn spawn<F>(&self, future: F) -> JoinHandle<F::Output>
    where F: Future + Send + 'static, F::Output: Send + 'static {
        self.handle().spawn(future)
    }

    pub fn spawn_blocking<F, R>(&self, future: F) -> JoinHandle<R>
    where F: FnOnce() -> R + Send + 'static, R: Send + 'static {
        self.handle().spawn_blocking(future)
    }

    pub fn block_on<F: Future>(&self, future: F) -> F::Output {
        let future = Box::pin(future);

        let mut scheduler = self.scheduler.borrow_mut();
        scheduler.spawn_task(future, TaskPriority::High);

        scheduler.run_until_idle();

        // Return result (needs proper implementation)
        unimplemented!("block_on result extraction")
    }

    pub fn enter<F, R>(&self, future: F) -> R
    where F: FnOnce() -> R {
        self.handle().enter(future)
    }

    pub fn shutdown_timeout(self, duration: Duration) {
        if chaos::should_fail(ChaosOperation::RuntimeDeadlock) {
            std::thread::sleep(duration * 2);
            return;
        }

        let start = Instant::now();
        while start.elapsed() < duration {
            let scheduler = self.scheduler.borrow();
            if scheduler.ready_queue.is_empty() && scheduler.waiting_tasks.is_empty() {
                break;
            }
            std::thread::yield_now();
        }
    }

    pub fn shutdown_background(self) {
        drop(self);
    }

    pub fn metrics(&self) -> RuntimeMetrics {
        self.metrics.lock().clone()
    }
}

