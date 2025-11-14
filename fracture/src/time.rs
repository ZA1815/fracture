use std::time::Duration;

#[cfg(feature = "simulation")]
use std::future::Future;

#[cfg(feature = "simulation")]
use tokio::time::Instant;
#[cfg(feature = "simulation")]
use tokio::time::Interval;
#[cfg(feature = "simulation")]
use tokio::time::error;
#[cfg(not(feature = "simulation"))]
pub use tokio::time::sleep;

#[cfg(feature = "simulation")]
pub async fn sleep(duration: Duration) {
    let actual_duration = if crate::chaos::should_fail("time_skew") {
        duration * 2
    }
    else {
        duration
    };

    tokio::time::sleep(actual_duration).await;
}

#[cfg(not(feature = "simulation"))]
pub use tokio::time::timeout;

#[cfg(feature = "simulation")]
pub async fn timeout<F: Future>(duration: Duration, future: F) -> Result<F::Output, error::Elapsed> {
    let actual_duration = if crate::chaos::should_fail("timeout_early") {
        duration / 2
    }
    else {
        duration
    };

    tokio::time::timeout(actual_duration, future).await
}

#[cfg(not(feature = "simulation"))]
pub use tokio::time::timeout_at;

#[cfg(feature = "simulation")]
pub async fn timeout_at<F: Future>(deadline: Instant, future: F) -> Result<F::Output, error::Elapsed> {
    let actual_deadline = if crate::chaos::should_fail("timeout_at_early") {
        deadline - Duration::from_millis(500)
    }
    else {
        deadline
    };

    tokio::time::timeout_at(actual_deadline, future).await
}

#[cfg(not(feature = "simulation"))]
pub use tokio::time::interval;

#[cfg(feature = "simulation")]
pub fn interval(period: Duration) -> tokio::time::Interval {
    let actual_period = if crate::chaos::should_fail("interval_period_skew") {
        period * 2
    }
    else {
        period
    };

    tokio::time::interval(actual_period)
}

#[cfg(not(feature = "simulation"))]
pub use tokio::time::interval_at;

#[cfg(feature = "simulation")]
pub fn interval_at(start: Instant, period: Duration) -> Interval {
    let actual_start = if crate::chaos::should_fail("interval_start_shift") {
        start - Duration::from_millis(500)
    }
    else {
        start
    };

    let actual_period = if crate::chaos::should_fail("interval_period_skew") {
        period * 2
    }
    else {
        period
    };

    tokio::time::interval_at(actual_start, actual_period)
}

#[cfg(not(feature = "simulation"))]
pub use tokio::time::sleep_until;

#[cfg(feature = "simulation")]
pub async fn sleep_until(deadline: Instant) {
    let actual_deadline = if crate::chaos::should_fail("sleep_until_shift") {
        deadline + Duration::from_millis(500)
    }
    else {
        deadline
    };

    tokio::time::sleep_until(actual_deadline).await
}