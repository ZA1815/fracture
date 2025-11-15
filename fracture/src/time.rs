use std::time::Duration;
use std::future::Future;
use tokio::time::Instant;
use tokio::time::Interval;
use tokio::time::error;

pub async fn sleep(duration: Duration) {
    let actual_duration = if crate::chaos::should_fail(crate::chaos::ChaosOperation::TimeSkew) {
        duration * 2
    }
    else {
        duration
    };

    tokio::time::sleep(actual_duration).await;
}

pub async fn timeout<F: Future>(duration: Duration, future: F) -> Result<F::Output, error::Elapsed> {
    let actual_duration = if crate::chaos::should_fail(crate::chaos::ChaosOperation::TimeoutEarly) {
        duration / 2
    }
    else {
        duration
    };

    tokio::time::timeout(actual_duration, future).await
}

pub async fn timeout_at<F: Future>(deadline: Instant, future: F) -> Result<F::Output, error::Elapsed> {
    let actual_deadline = if crate::chaos::should_fail(crate::chaos::ChaosOperation::TimeoutAtEarly) {
        deadline - Duration::from_millis(500)
    }
    else {
        deadline
    };

    tokio::time::timeout_at(actual_deadline, future).await
}

pub fn interval(period: Duration) -> tokio::time::Interval {
    let actual_period = if crate::chaos::should_fail(crate::chaos::ChaosOperation::IntervalPeriodSkew) {
        period * 2
    }
    else {
        period
    };

    tokio::time::interval(actual_period)
}


pub fn interval_at(start: Instant, period: Duration) -> Interval {
    let actual_start = if crate::chaos::should_fail(crate::chaos::ChaosOperation::IntervalStartShift) {
        start - Duration::from_millis(500)
    }
    else {
        start
    };

    let actual_period = if crate::chaos::should_fail(crate::chaos::ChaosOperation::IntervalPeriodSkew) {
        period * 2
    }
    else {
        period
    };

    tokio::time::interval_at(actual_start, actual_period)
}

pub async fn sleep_until(deadline: Instant) {
    let actual_deadline = if crate::chaos::should_fail(crate::chaos::ChaosOperation::SleepUntilShift) {
        deadline + Duration::from_millis(500)
    }
    else {
        deadline
    };

    tokio::time::sleep_until(actual_deadline).await
}