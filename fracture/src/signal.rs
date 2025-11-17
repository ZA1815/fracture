
use std::collections::VecDeque;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll, Waker};
use std::time::Duration;
use parking_lot::Mutex;

use crate::chaos::{self, ChaosOperation};
use crate::task::{JoinHandle, spawn};
use crate::time::sleep;

static SIGNAL_REGISTRY: std::sync::LazyLock<SignalRegistry> = std::sync::LazyLock::new(SignalRegistry::new);

struct SignalRegistry {
    queues: Arc<Mutex<Vec<SignalQueue>>>,
    deterministic: bool
}

struct SignalQueue {
    signal_kind: SignalKind,
    pending: VecDeque<()>,
    wakers: Vec<Waker>
}

impl SignalRegistry {
    fn new() -> Self {
        Self {
            queues: Arc::new(Mutex::new(Vec::new())),
            deterministic: true,
        }
    }

    fn register(&self, kind: SignalKind) -> usize {
        let mut queues = self.queues.lock();
        let id = queues.len();

        queues.push(SignalQueue {
            signal_kind: kind,
            pending: VecDeque::new(),
            wakers: Vec::new()
        });

        id
    }

    fn inject_signal(&self, kind: SignalKind) {
        if chaos::should_fail(ChaosOperation::SignalMissed) {
            return;
        }

        if chaos::should_fail(ChaosOperation::SignalDelayed) {
            // Placeholder
        }

        if chaos::should_fail(ChaosOperation::SignalDuplicate) {
            self.deliver_signal(kind);
            self.deliver_signal(kind);
            return;
        }

        self.deliver_signal(kind);
    }

    fn deliver_signal(&self, kind: SignalKind) {
        let mut queues = self.queues.lock();

        for queue in queues.iter_mut() {
            if queue.signal_kind == kind {
                queue.pending.push_back(());
            }

            for waker in queue.wakers.drain(..) {
                waker.wake();
            }
        }
    }

    fn poll_signal(&self, id: usize, cx: &mut Context<'_>) -> Poll<()> {
        let mut queues = self.queues.lock();

        if let Some(queue) = queues.get_mut(id) {
            if queue.pending.pop_front().is_some() {
                return Poll::Ready(());
            }

            queue.wakers.push(cx.waker().clone());

            Poll::Pending
        }
        else {
            Poll::Ready(())
        }
    }

    fn clear(&self) {
        let mut queues = self.queues.lock();
        queues.clear();
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SignalKind {
    Hangup,          // SIGHUP - 1
    Interrupt,       // SIGINT - 2
    Quit,            // SIGQUIT - 3
    Illegal,         // SIGILL - 4
    Trap,            // SIGTRAP - 5
    Abort,           // SIGABRT - 6
    Bus,             // SIGBUS - 7
    FloatingPointException, // SIGFPE - 8
    Kill,            // SIGKILL - 9
    User1,           // SIGUSR1 - 10
    SegmentationViolation, // SIGSEGV - 11
    User2,           // SIGUSR2 - 12
    Pipe,            // SIGPIPE - 13
    Alarm,           // SIGALRM - 14
    Terminate,       // SIGTERM - 15
    Child,           // SIGCHLD - 17
    Continue,        // SIGCONT - 18
    Stop,            // SIGSTOP - 19
    TerminalStop,    // SIGTSTP - 20
    TerminalInput,   // SIGTTIN - 21
    TerminalOutput,  // SIGTTOU - 22
    Urgent,          // SIGURG - 23
    ExceedsCpuQuota, // SIGXCPU - 24
    ExceedsFileSizeQuota, // SIGXFSZ - 25
    VirtualAlarm,    // SIGVTALRM - 26
    Profiling,       // SIGPROF - 27
    WindowChange,    // SIGWINCH - 28
    Io,              // SIGIO - 29
    Power,           // SIGPWR - 30
    #[cfg(windows)]
    CtrlC,
    #[cfg(windows)]
    CtrlBreak,
    #[cfg(windows)]
    CtrlClose,
    #[cfg(windows)]
    CtrlLogoff,
    #[cfg(windows)]
    CtrlShutdown
}

impl SignalKind {
    pub fn as_raw_value(&self) -> i32 {
        match self {
            SignalKind::Hangup => 1,
            SignalKind::Interrupt => 2,
            SignalKind::Quit => 3,
            SignalKind::Illegal => 4,
            SignalKind::Trap => 5,
            SignalKind::Abort => 6,
            SignalKind::Bus => 7,
            SignalKind::FloatingPointException => 8,
            SignalKind::Kill => 9,
            SignalKind::User1 => 10,
            SignalKind::SegmentationViolation => 11,
            SignalKind::User2 => 12,
            SignalKind::Pipe => 13,
            SignalKind::Alarm => 14,
            SignalKind::Terminate => 15,
            SignalKind::Child => 17,
            SignalKind::Continue => 18,
            SignalKind::Stop => 19,
            SignalKind::TerminalStop => 20,
            SignalKind::TerminalInput => 21,
            SignalKind::TerminalOutput => 22,
            SignalKind::Urgent => 23,
            SignalKind::ExceedsCpuQuota => 24,
            SignalKind::ExceedsFileSizeQuota => 25,
            SignalKind::VirtualAlarm => 26,
            SignalKind::Profiling => 27,
            SignalKind::WindowChange => 28,
            SignalKind::Io => 29,
            SignalKind::Power => 30,
            #[cfg(windows)]
            SignalKind::CtrlC => 100,
            #[cfg(windows)]
            SignalKind::CtrlBreak => 101,
            #[cfg(windows)]
            SignalKind::CtrlClose => 102,
            #[cfg(windows)]
            SignalKind::CtrlLogoff => 103,
            #[cfg(windows)]
            SignalKind::CtrlShutdown => 104
        }
    }

    pub fn from_raw(value: i32) -> Option<Self> {
        match value {
            1 => Some(SignalKind::Hangup),
            2 => Some(SignalKind::Interrupt),
            3 => Some(SignalKind::Quit),
            4 => Some(SignalKind::Illegal),
            5 => Some(SignalKind::Trap),
            6 => Some(SignalKind::Abort),
            7 => Some(SignalKind::Bus),
            8 => Some(SignalKind::FloatingPointException),
            9 => Some(SignalKind::Kill),
            10 => Some(SignalKind::User1),
            11 => Some(SignalKind::SegmentationViolation),
            12 => Some(SignalKind::User2),
            13 => Some(SignalKind::Pipe),
            14 => Some(SignalKind::Alarm),
            15 => Some(SignalKind::Terminate),
            17 => Some(SignalKind::Child),
            18 => Some(SignalKind::Continue),
            19 => Some(SignalKind::Stop),
            20 => Some(SignalKind::TerminalStop),
            _ => None
        }
    }
}

pub struct Signal {
    id: usize,
    kind: SignalKind
}

impl Signal {
    pub async fn recv(&mut self) -> Option<()> {
        if chaos::should_fail(ChaosOperation::SignalRecv) {
            std::future::pending().await
        }

        SignalFuture { id: self.id }.await;
        Some(())
    }

    pub fn poll_recv(&mut self, cx: &mut Context<'_>) -> Poll<Option<()>> {
        SIGNAL_REGISTRY.poll_signal(self.id, cx).map(Some)
    }
}

struct SignalFuture {
    id: usize
}

impl Future for SignalFuture {
    type Output = ();

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        SIGNAL_REGISTRY.poll_signal(self.id, cx)
    }
}

#[cfg(unix)]
pub mod unix {
    use super::*;

    pub fn signal(kind: SignalKind) -> std::io::Result<Signal> {
        if chaos::should_fail(ChaosOperation::SignalRecv) {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "fracture: Signal registration failed (chaos)",
            ));
        }

        let id = SIGNAL_REGISTRY.register(kind);
        Ok(Signal { id, kind })
    }

    pub fn hangup() -> std::io::Result<Signal> {
        signal(SignalKind::Hangup)
    }

    pub fn interrupt() -> std::io::Result<Signal> {
        signal(SignalKind::Interrupt)
    }

    pub fn quit() -> std::io::Result<Signal> {
        signal(SignalKind::Quit)
    }

    pub fn terminate() -> std::io::Result<Signal> {
        signal(SignalKind::Terminate)
    }

    pub fn alarm() -> std::io::Result<Signal> {
        signal(SignalKind::Alarm)
    }

    pub fn child() -> std::io::Result<Signal> {
        signal(SignalKind::Child)
    }

    pub fn user_defined1() -> std::io::Result<Signal> {
        signal(SignalKind::User1)
    }

    pub fn user_defined2() -> std::io::Result<Signal> {
        signal(SignalKind::User2)
    }

    pub fn window_change() -> std::io::Result<Signal> {
        signal(SignalKind::WindowChange)
    }

    pub fn pipe() -> std::io::Result<Signal> {
        signal(SignalKind::Pipe)
    }
}

#[cfg(windows)]
pub mod windows {
    use super::*;

    pub async fn ctrl_c() -> std::io::Result<()> {
        if chaos::should_fail(ChaosOperation::SignalRecv) {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "fracture: Ctrl-C handler failed (chaos)",
            ));
        }

        let mut signal = Signal {
            id: SIGNAL_REGISTRY.register(SignalKind::CtrlC),
            kind: SignalKind::CtrlC,
        };

        signal.recv().await;
        Ok(())
    }

    pub struct CtrlBreak {
        inner: Signal,
    }

    impl CtrlBreak {
        pub fn new() -> std::io::Result<Self> {
            let id = SIGNAL_REGISTRY.register(SignalKind::CtrlBreak);
            Ok(Self {
                inner: Signal {
                    id,
                    kind: SignalKind::CtrlBreak,
                },
            })
        }

        pub async fn recv(&mut self) -> Option<()> {
            self.inner.recv().await
        }
    }

    pub struct CtrlClose {
        inner: Signal,
    }

    impl CtrlClose {
        pub fn new() -> std::io::Result<Self> {
            let id = SIGNAL_REGISTRY.register(SignalKind::CtrlClose);
            Ok(Self {
                inner: Signal {
                    id,
                    kind: SignalKind::CtrlClose,
                },
            })
        }

        pub async fn recv(&mut self) -> Option<()> {
            self.inner.recv().await
        }
    }
}

pub fn inject_signal(kind: SignalKind) {
    SIGNAL_REGISTRY.inject_signal(kind);
}

pub fn clear_all_signals() {
    SIGNAL_REGISTRY.clear();
}

pub async fn ctrl_c() -> std::io::Result<()> {
    #[cfg(unix)]
    {
        let mut signal = unix::signal(SignalKind::Interrupt)?;
        signal.recv().await;
        Ok(())
    }

    #[cfg(windows)]
    {
        windows::ctrl_c().await
    }

    #[cfg(not(any(unix, windows)))]
    {
        std::future::pending().await
    }
}

pub mod test {
    use super::*;

    pub fn simulate_signal(kind: SignalKind) {
        inject_signal(kind);
    }

    pub fn simulate_ctrl_c() {
        #[cfg(unix)]
        inject_signal(SignalKind::Interrupt);

        #[cfg(windows)]
        inject_signal(SignalKind::CtrlC);
    }

    pub fn simulate_sigterm() {
        inject_signal(SignalKind::Terminate);
    }

    pub fn simulate_sighup() {
        inject_signal(SignalKind::Hangup);
    }

    pub fn clear() {
        clear_all_signals();
    }
}

pub struct SignalScenario {
    signals: Vec<(SignalKind, Duration)>
}

impl SignalScenario {
    pub fn new() -> Self {
        Self { signals: Vec::new() }
    }
    
    pub fn send_after(mut self, kind: SignalKind, delay: Duration) -> Self {
        self.signals.push((kind, delay));
        self
    }

    pub fn send_ctrl_c_after(self, delay: Duration) -> Self {
        #[cfg(unix)]
        return self.send_after(SignalKind::Interrupt, delay);

        #[cfg(windows)]
        return self.send_after(SignalKind::CtrlC, delay);

        #[cfg(not(any(unix, windows)))]
        self
    }

    pub fn send_sigterm_after(self, delay: Duration) -> Self {
        self.send_after(SignalKind::Terminate, delay)
    }

    pub async fn execute(self) {
        for (kind, delay) in self.signals {
            sleep(delay).await;
            inject_signal(kind);
        }
    }

    pub fn spawn(self) -> JoinHandle<()> {
        spawn(async move {
            self.execute().await;
        })
    }
}

impl Default for SignalScenario {
    fn default() -> Self {
        Self::new()
    }
}