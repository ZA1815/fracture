use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};
use std::time::Duration;
use pin_project::pin_project;

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
                id: core.rng.r#gen()
            };

            core.timers.push(entry);
            self.registered = true;
        }

        Poll::Pending
    }
}

pub fn sleep(duration: Duration) -> Sleep {
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
    Timeout { future, sleep: sleep(duration) }
}