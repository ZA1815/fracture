use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};
use std::time::Duration;

use pin_project::pin_project;
use rand::Rng;

use crate::chaos::{self, ChaosOperation};
use crate::runtime::Handle;

pub struct Sleep {
    deadline: Duration,
    registered: bool
}

impl Future for Sleep {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let handle = Handle::current();
        let core_rc = handle.core.upgrade().expect("fracture: Runtime dropped");
        let mut core = core_rc.borrow_mut();

        if core.current_time >= self.deadline {
            return Poll::Ready(());
        }

        if !self.registered {
            let entry = crate::runtime::core::TimerEntry {
                deadline: self.deadline,
                waker: cx.waker().clone(),
                id: core.rng.rand_u64()
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

    Sleep { deadline: now + duration, registered: false }
}

#[pin_project]
pub struct Timeout<F> {
    #[pin]
    future: F,
    #[pin]
    sleep: Sleep
}

impl<F: Future> Future for Timeout<F> {
    type Output = Result<F::Output, ()>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.project();

        if let Poll::Ready(v) = self.future.poll(cx) {
            return Poll::Ready(Ok(v))
        }

        if let Poll::Ready(()) = self.sleep.poll(cx) {
            return Poll::Ready(Err(()));
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

pub fn interval(period: Duration) -> Interval {
    Interval::new(period)
}

pub struct Interval {
    period: Duration,
    next_tick: Duration,
}

impl Interval {
    pub fn new(period: Duration) -> Self {
        let handle = Handle::current();
        let core = handle.core.upgrade().expect("fracture: Runtime dropped");
        let now = core.borrow().current_time;
        Self {
            period,
            next_tick: now + period,
        }
    }

    pub async fn tick(&mut self) -> crate::runtime::core::Instant {
        if chaos::should_fail(ChaosOperation::IntervalSkip) {
            self.next_tick += self.period;
        }

        let sleep_time = self
            .next_tick
            .saturating_sub(crate::runtime::core::Instant::now());

        // Placeholder: Actually sleep to match next_tick
        let handle = Handle::current();
        let now = handle.core.upgrade().unwrap().borrow().current_time;

        if self.next_tick > now {
            crate::time::sleep(self.next_tick - now).await;
        }

        let now = handle.core.upgrade().unwrap().borrow().current_time;
        self.next_tick += self.period;

        crate::runtime::core::Instant::from(now)
    }

    pub fn poll_tick(&mut self, cx: &mut Context<'_>) -> Poll<crate::runtime::core::Instant> {
        let handle = Handle::current();
        let core_rc = handle.core.upgrade().unwrap();
        let now = core_rc.borrow().current_time;

        if now >= self.next_tick {
            self.next_tick += self.period;
            Poll::Ready(crate::runtime::core::Instant::from(now))
        }
        else {
            let mut core = core_rc.borrow_mut();
            let waker = cx.waker().clone();
            core.timers.push(crate::runtime::core::TimerEntry {
                deadline: self.next_tick,
                waker,
                id: core.rng.r#gen() as usize,
            });
            Poll::Pending
        }
    }
}

pub type ChaosInstant = crate::runtime::core::Instant;

impl crate::runtime::core::Instant {
    pub fn now() -> Self {
        let handle = Handle::current();
        let core = handle.core.upgrade().expect("fracture: Runtime dropped");
        let now = core.borrow().current_time;
        Self(now)
    }
}