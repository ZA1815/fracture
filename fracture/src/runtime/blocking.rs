use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};

pub struct BlockingTask<T> {
    rx: std::sync::mpsc::Receiver<T>,
}

impl<T> Future for BlockingTask<T> {
    type Output = T;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match self.rx.try_recv() {
            Ok(val) => Poll::Ready(val),
            Err(std::sync::mpsc::TryRecvError::Empty) => {
                cx.waker().wake_by_ref();
                Poll::Pending
            }
            Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                panic!("fracture: Blocking task sender dropped");
            }
        }
    }
}

pub fn spawn_blocking<F, R>(f: F) -> BlockingTask<R>
where F: FnOnce() -> R + Send + 'static, R: Send + 'static {
    let (tx, rx) = std::sync::mpsc::channel();
    
    std::thread::spawn(move || {
        let result = f();
        let _ = tx.send(result);
    });

    BlockingTask { rx }
}