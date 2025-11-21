use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};
use std::time::Duration;

use pin_project::pin_project;
use rand::Rng;

use crate::chaos::{self, ChaosOperation};
use crate::runtime::Handle;

pub type Instant = crate::runtime::core::Instant;

pub struct Sleep {
    deadline: Instant,
    registered: bool
}

impl Sleep {
    pub fn deadline(&self) -> Instant {
        self.deadline
    }
    
    pub fn is_elapsed(&self) -> bool {
        let handle = Handle::current();
        let core = handle.core.upgrade().expect("fracture: Runtime dropped");
        let now = crate::runtime::core::Instant::from(core.borrow().current_time);
        now >= self.deadline
    }
}

impl Future for Sleep {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let handle = Handle::current();
        let core_rc = handle.core.upgrade().expect("fracture: Runtime dropped");
        let mut core = core_rc.borrow_mut();

        if core.current_time >= self.deadline.0 {
            return Poll::Ready(());
        }

        if !self.registered {
            let entry = crate::runtime::core::TimerEntry {
                deadline: self.deadline.0,
                waker: cx.waker().clone(),
                id: core.rng.r#gen::<usize>()
            };

            core.timers.push(entry);
            self.registered = true;
        }

        Poll::Pending
    }
}

pub fn sleep(duration: Duration) -> Sleep {
    let mut duration = duration;
    if chaos::should_fail(ChaosOperation::SleepShort) {
        duration = duration.mul_f64(0.5);
    }
    else if chaos::should_fail(ChaosOperation::SleepLong) {
        duration = duration.mul_f64(2.0);
    }

    let handle = Handle::current();
    let core = handle.core.upgrade().expect("fracture: Runtime dropped");
    let now = core.borrow().current_time;

    Sleep { deadline: Instant::from(now + duration), registered: false }
}

pub fn sleep_until(deadline: Instant) -> Sleep {
    Sleep { deadline, registered: false }
}

#[pin_project]
pub struct Timeout<F> {
    #[pin]
    future: F,
    #[pin]
    sleep: Sleep
}

impl<F: Future> Future for Timeout<F> {
    type Output = Result<F::Output, error::Elapsed>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut this = self.project();

        if let Poll::Ready(v) = this.future.as_mut().poll(cx) {
            return Poll::Ready(Ok(v))
        }

        if let Poll::Ready(()) = this.sleep.as_mut().poll(cx) {
            return Poll::Ready(Err(error::Elapsed::new()));
        }

        Poll::Pending
    }
}

pub fn timeout<F: Future>(duration: Duration, future: F) -> Timeout<F> {
    let mut duration = duration;
    if chaos::should_fail(ChaosOperation::TimeoutEarly) {
        duration = duration.mul_f64(0.8);
    }
    else if chaos::should_fail(ChaosOperation::TimeoutLate) {
        duration = duration.mul_f64(1.5);
    }

    Timeout { future, sleep: sleep(duration) }
}

pub fn timeout_at<F: Future>(deadline: Instant, future: F) -> Timeout<F> {
    Timeout { future, sleep: Sleep { deadline, registered: false } }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MissedTickBehavior {
    Burst,
    Delay,
    Skip,
}

pub fn interval(period: Duration) -> Interval {
    Interval::new(period)
}

pub struct Interval {
    period: Duration,
    next_tick: Instant,
    missed_tick_behavior: MissedTickBehavior
}

impl Interval {
    pub fn new(period: Duration) -> Self {
        let handle = Handle::current();
        let core = handle.core.upgrade().expect("fracture: Runtime dropped");
        let now = crate::runtime::core::Instant::from(core.borrow().current_time);
        Self {
            period,
            next_tick: now,
            missed_tick_behavior: MissedTickBehavior::Burst
        }
    }

    pub fn new_at(start: Instant, period: Duration) -> Self {
        Self {
            period,
            next_tick: start,
            missed_tick_behavior: MissedTickBehavior::Burst
        }
    }

    pub fn set_missed_tick_behavior(&mut self, behavior: MissedTickBehavior) {
        self.missed_tick_behavior = behavior;
    }

    pub async fn tick(&mut self) -> crate::runtime::core::Instant {
        if chaos::should_fail(ChaosOperation::IntervalSkip) {
            self.next_tick = self.next_tick + self.period;
        }

        let handle = Handle::current();
        let core_rc = handle.core.upgrade().unwrap();
        let now = crate::runtime::core::Instant::from(core_rc.borrow().current_time);

        if self.next_tick > now {
            sleep_until(self.next_tick).await;
        }

        let now = crate::runtime::core::Instant::from(core_rc.borrow().current_time);
        let tick_time = self.next_tick;

        match self.missed_tick_behavior {
            MissedTickBehavior::Burst => {
                self.next_tick = self.next_tick + self.period;
            }
            MissedTickBehavior::Delay => {
                self.next_tick = now + self.period;
            }
            MissedTickBehavior::Skip => {
                 let now = now;
                 if now > self.next_tick {
                     let missed = (now.0 - self.next_tick.0).as_millis() / self.period.as_millis();
                     self.next_tick = self.next_tick + self.period * ((missed + 1) as u32);
                 } else {
                     self.next_tick = self.next_tick + self.period;
                 }
            }
        }

        tick_time
    }

    pub fn poll_tick(&mut self, cx: &mut Context<'_>) -> Poll<Instant> {
        let handle = Handle::current();
        let core_rc = handle.core.upgrade().unwrap();
        let now = {
            let core = core_rc.borrow();
            crate::runtime::core::Instant::from(core.current_time)
        };

        if now >= self.next_tick {
            let tick_time = self.next_tick;
            match self.missed_tick_behavior {
                MissedTickBehavior::Burst => {
                    self.next_tick = self.next_tick + self.period;
                }
                MissedTickBehavior::Delay => {
                    self.next_tick = now + self.period;
                }
                MissedTickBehavior::Skip => {
                    if now > self.next_tick {
                        let missed = (now.0 - self.next_tick.0).as_millis() / self.period.as_millis();
                        self.next_tick = self.next_tick + self.period * ((missed + 1) as u32);
                    }
                    else {
                        self.next_tick = self.next_tick + self.period;
                    }
                }
            }
            Poll::Ready(tick_time)
            
        }
        else {
            let waker = cx.waker().clone();
            let id = {
                let mut core = core_rc.borrow_mut();
                core.rng.r#gen::<usize>()
            };
            let mut core = core_rc.borrow_mut();
            core.timers.push(crate::runtime::core::TimerEntry {
                deadline: self.next_tick.0,
                waker,
                id,
            });
            Poll::Pending
        }
    }
}

pub type ChaosInstant = crate::runtime::core::Instant;

pub mod error {
    use std::fmt;

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Elapsed(());

    impl Elapsed {
        pub(crate) fn new() -> Self {
            Elapsed(())
        }
    }

    impl fmt::Display for Elapsed {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "deadline has elapsed")
        }
    }

    impl std::error::Error for Elapsed {}
}