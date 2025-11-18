use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};
use std::time::SystemTime;
use tokio::time::{Duration, Instant, MissedTickBehavior};
pub use tokio::time::Interval;
use tokio::time::error::Elapsed;
use pin_project::pin_project;

use crate::chaos::{self, ChaosOperation};

pub fn deterministic_now() -> SystemTime {
    let genesis = SystemTime::UNIX_EPOCH + Duration::from_secs(1735689600);

    let elapsed = tokio::time::Instant::now().elapsed();

    genesis + elapsed
}

pub async fn sleep(duration: Duration) {
    if chaos::should_fail(ChaosOperation::SleepNever) {
        std::future::pending::<()>().await;
    }

    if chaos::should_fail(ChaosOperation::SleepShort) {
        tokio::time::sleep(duration / 2).await;
    }

    if chaos::should_fail(ChaosOperation::SleepLong) {
        tokio::time::sleep(duration * 2).await;
    }

    if chaos::should_fail(ChaosOperation::TimeSkew) {
        let skew = rand::random::<u64>() % 1000;
        let skewed = Duration::from_millis(duration.as_millis() as u64 + skew);
        tokio::time::sleep(skewed).await;
        return;
    }

    tokio::time::sleep(duration).await;
}

pub async fn sleep_until(deadline: Instant) {
    if chaos::should_fail(ChaosOperation::SleepUntilNever) {
        std::future::pending::<()>().await;
    }

    if chaos::should_fail(ChaosOperation::SleepUntilShift) {
        let shift = Duration::from_millis(rand::random::<u64>() % 100);
        tokio::time::sleep_until(deadline + shift).await;
    }

    if chaos::should_fail(ChaosOperation::ClockJump) {
        return;
    }

    tokio::time::sleep_until(deadline).await;
}

pub async fn timeout<F: Future>(duration: Duration, future: F) -> Result<F::Output, Elapsed> {
    if chaos::should_fail(ChaosOperation::TimeoutNever) {
        return Ok(future.await);
    }

    if chaos::should_fail(ChaosOperation::TimeoutEarly) {
        return tokio::time::timeout(duration / 2, future).await;
    }

    if chaos::should_fail(ChaosOperation::TimeoutLate) {
        return tokio::time::timeout(duration * 2, future).await;
    }

    if chaos::should_fail(ChaosOperation::JoinTimeout) {
        return tokio::time::timeout(Duration::from_nanos(0), async {}).await.map(|_| unreachable!());
    }

    tokio::time::timeout(duration, future).await
}

pub async fn timeout_at<F: Future>(deadline: Instant, future: F) -> Result<F::Output, Elapsed> {
    if chaos::should_fail(ChaosOperation::TimeoutAtEarly) {
        let early = deadline - Duration::from_millis(500);
        return tokio::time::timeout_at(early, future).await;
    }

    if chaos::should_fail(ChaosOperation::TimeoutAtLate) {
        let late = deadline + Duration::from_millis(500);
        return tokio::time::timeout_at(late, future).await;
    }

    tokio::time::timeout_at(deadline, future).await
}

pub fn interval(period: Duration) -> tokio::time::Interval {
    if chaos::should_fail(ChaosOperation::IntervalDouble) {
        return tokio::time::interval(period * 2);
    }

    if chaos::should_fail(ChaosOperation::IntervalPeriodSkew) {
        let skew = 1 + (rand::random::<u64>() % 3);
        return tokio::time::interval(period * skew as u32);
    }

    tokio::time::interval(period)
}


pub fn interval_at(start: Instant, period: Duration) -> Interval {
    let actual_start = if chaos::should_fail(ChaosOperation::IntervalStartShift) {
        start + Duration::from_millis(rand::random::<u64>() % 1000)
    }
    else {
        start
    };

    let actual_period = if chaos::should_fail(ChaosOperation::IntervalPeriodSkew) {
        period * 2
    }
    else if chaos::should_fail(ChaosOperation::IntervalDrift) {
        period + Duration::from_millis(10)
    }
    else {
        period
    };

    tokio::time::interval_at(actual_start, actual_period)
}

#[derive(Clone)]
pub struct ChaosInstant {
    inner: Instant,
    skew: i64
}

impl ChaosInstant {
    pub fn now() -> Self {
        let skew = if chaos::should_fail(ChaosOperation::InstantSkew) {
            (rand::random::<i64>() % 1000) - 500
        }
        else if chaos::should_fail(ChaosOperation::ClockReverse) {
            -(rand::random::<i64>() % 100).abs()
        }
        else {
            0
        };

        Self {
            inner: Instant::now(),
            skew
        }
    }

    pub fn duration_since(&self, earlier: Self) -> Duration {
        let base_duration = self.inner.duration_since(earlier.inner);
        let skew_diff = (self.skew - earlier.skew) as i128;

        if chaos::should_fail(ChaosOperation::DurationSkew) {
            let extra = rand::random::<u64>() % 100;
            return base_duration + Duration::from_millis(extra);
        }

        if skew_diff >= 0 {
            base_duration + Duration::from_millis(skew_diff as u64)
        }
        else {
            base_duration.saturating_sub(Duration::from_millis((-skew_diff) as u64))
        }
    }

    pub fn elapsed(&self) -> Duration {
        Self::now().duration_since(self.clone())
    }

    pub fn checked_add(&self, duration: Duration) -> Option<Self> {
        self.inner.checked_add(duration).map(|inner| Self {
            inner,
            skew: self.skew
        })
    }
}

#[pin_project]
pub struct Sleep {
    #[pin]
    inner: tokio::time::Sleep,
    chaos_state: ChaosState
}

struct ChaosState {
    should_never_complete: bool,
    should_complete_early: bool,
    early_completion_time: Option<Instant>
}

impl Sleep {
    pub fn new(deadline: Instant) -> Self {
        let chaos_state = ChaosState {
            should_never_complete: chaos::should_fail(ChaosOperation::SleepNever),
            should_complete_early: chaos::should_fail(ChaosOperation::SleepShort),
            early_completion_time: if chaos::should_fail(ChaosOperation::SleepShort) {
                Some(Instant::now() + Duration::from_millis(1))
            }
            else {
                None
            }
        };

        Self {
            inner: tokio::time::sleep_until(deadline),
            chaos_state
        }
    }

    pub fn deadline(&self) -> Instant {
        self.inner.deadline()
    }

    pub fn is_elapsed(&self) -> bool {
        if self.chaos_state.should_never_complete {
            return false;
        }

        if self.chaos_state.should_complete_early {
            if let Some(early_time) = self.chaos_state.early_completion_time {
                return Instant::now() >= early_time
            }
        }

        self.inner.is_elapsed()
    }

    pub fn reset(&mut self, deadline: Instant) {
        self.inner = tokio::time::sleep_until(deadline);

        self.chaos_state.early_completion_time = if self.chaos_state.should_complete_early {
            Some(Instant::now() + Duration::from_millis(1))
        }
        else {
            None
        };
    }
}

impl Future for Sleep {
    type Output = ();

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.project();

        if this.chaos_state.should_never_complete {
            return Poll::Pending;
        }

        if this.chaos_state.should_complete_early {
            if let Some(early_time) = this.chaos_state.early_completion_time {
                if Instant::now() >= early_time {
                    return Poll::Ready(());
                }
            }
        }

        this.inner.poll(cx)
    }
}

pub struct ChaosInterval {
    inner: Interval,
    skip_counter: u32,
    drift_accumulator: Duration
}

impl ChaosInterval {
    pub fn new(period: Duration) -> Self {
        Self {
            inner: tokio::time::interval(period),
            skip_counter: 0,
            drift_accumulator: Duration::ZERO
        }
    }

    pub async fn tick(&mut self) -> Instant {
        if chaos::should_fail(ChaosOperation::IntervalSkip) {
            self.skip_counter += 1;
            if self.skip_counter % 3 == 0 {
                self.inner.tick().await;
                self.inner.tick().await;
                return self.inner.tick().await;
            }
        }

        if chaos::should_fail(ChaosOperation::IntervalDrift) {
            self.drift_accumulator += Duration::from_millis(10);
            tokio::time::sleep(self.drift_accumulator).await;
        }

        self.inner.tick().await
    }

    pub fn reset(&mut self) {
        self.inner.reset();
        self.skip_counter = 0;
        self.drift_accumulator = Duration::ZERO;
    }

    pub fn reset_immediately(&mut self) {
        self.inner.reset_immediately();
    }

    pub fn reset_after(&mut self, after: Duration) {
        self.inner.reset_after(after);
    }

    pub fn reset_at(&mut self, at: Instant) {
        self.inner.reset_at(at);
    }

    pub fn missed_tick_behavior(&self) -> MissedTickBehavior {
        self.inner.missed_tick_behavior()
    }

    pub fn set_missed_tick_behavior(&mut self, behavior: MissedTickBehavior) {
        self.inner.set_missed_tick_behavior(behavior);
    }

    pub fn period(&self) -> Duration {
        self.inner.period()
    }
}

pub fn pause() {
    if chaos::should_fail(ChaosOperation::TimePause) {
        // Future impl
    }
}

pub fn resume() {
    // Future impl
}

pub fn advance(duration: Duration) {
    if chaos::should_fail(ChaosOperation::TimeAcceleration) {
        // Future impl
    }
    else if chaos::should_fail(ChaosOperation::TimeTravel) {
        // Future impl
    }
}

pub struct Throttle<T> {
    inner: T,
    last_call: Instant,
    min_interval: Duration
}

impl<T> Throttle<T> {
    pub fn new(inner: T, rate_limit: Duration) -> Self {
        Self {
            inner,
            last_call: Instant::now(),
            min_interval: rate_limit
        }
    }

    pub async fn call<F, R>(&mut self, f: F) -> R
    where F: FnOnce(&mut T) -> R {
        if chaos::should_fail(ChaosOperation::StreamThrottle) {
            return f(&mut self.inner)
        }

        let now = Instant::now();
        let elapsed = now.duration_since(self.last_call);

        if elapsed < self.min_interval {
            sleep(self.min_interval - elapsed).await;
        }

        self.last_call = Instant::now();
        f(&mut self.inner)
    }
}

pub struct Deadline {
    instant: Instant
}

impl Deadline {
    pub fn new(instant: Instant) -> Self {
        if chaos::should_fail(ChaosOperation::TimeSkew) {
            let skew = Duration::from_millis(rand::random::<u64>() % 1000);
            return Self { instant: instant + skew };
        }

        Self { instant }
    }

    pub fn from_now(duration: Duration) -> Self {
        Self::new(Instant::now() + duration)
    }

    pub fn has_elapsed(&self) -> bool {
        if chaos::should_fail(ChaosOperation::ClockJump) {
            return rand::random::<bool>();
        }

        Instant::now() >= self.instant
    }

    pub fn time_remaining(&self) -> Duration {
        self.instant.saturating_duration_since(Instant::now())
    }
}