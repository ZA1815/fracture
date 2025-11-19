use std::net::SocketAddr;
use std::pin::Pin;
use std::task::{Context, Poll};
use std::time::Duration;
pub use futures::Stream;
use pin_project::pin_project;
use tokio::time::Instant;

use crate::chaos::{self, ChaosOperation};
use crate::net::{TcpListener, TcpStream};
use crate::time::{interval, Interval};

pub trait StreamExt: Stream {
    fn next(&mut self) -> Next<'_, Self>
    where Self: Unpin {
        Next { stream: self }
    }

    fn map<T, F>(self, f: F) -> Map<Self, F>
    where F: FnMut(Self::Item) -> T, Self: Sized {
        Map { stream: self, f }
    }

    fn filter<F>(self, f: F) -> Filter<Self, F>
    where F: FnMut(&Self::Item) -> bool, Self: Sized {
        Filter { stream: self, f }
    }

    fn filter_map<T, F>(self, f: F) -> FilterMap<Self, F>
    where F: FnMut(Self::Item) -> Option<T>, Self: Sized {
        FilterMap { stream: self, f }
    }

    fn then<F, Fut>(self, f: F) -> Then<Self, F, Fut>
    where F: FnMut(Self::Item) -> Fut, Fut: Future, Self: Sized {
        Then {
            stream: self,
            f,
            future: None
        }
    }

    fn take(self, n: usize) -> Take<Self>
    where Self: Sized {
        Take {
            stream: self,
            remaining: n
        }
    }

    fn take_while<F>(self, f: F) -> TakeWhile<Self, F>
    where F: FnMut(&Self::Item) -> bool, Self: Sized {
        TakeWhile {
            stream: self,
            f,
            done: false
        }
    }

    fn skip(self, n: usize) -> Skip<Self>
    where Self: Sized {
        Skip {
            stream: self,
            remaining: n
        }
    }

    fn skip_while<F>(self, f: F) -> SkipWhile<Self, F>
    where F: FnMut(&Self::Item) -> bool, Self: Sized {
        SkipWhile {
            stream: self,
            f,
            done: false
        }
    }

    fn fuse(self) -> Fuse<Self>
    where Self: Sized {
        Fuse {
            stream: Some(self)
        }
    }

    fn chain<S>(self, other: S) -> Chain<Self, S>
    where S: Stream<Item = Self::Item>, Self: Sized {
        Chain {
            first: Some(self),
            second: other
        }
    }

    fn zip<S>(self, other: S) -> Zip<Self, S>
    where S: Stream, Self: Sized {
        Zip {
            stream1: self,
            stream2: other
        }
    }

    fn merge<S>(self, other: S) -> Merge<Self, S>
    where S: Stream<Item = Self::Item>, Self: Sized {
        Merge {
            stream1: self,
            stream2: other
        }
    }

    fn timeout(self, duration: Duration) -> Timeout<Self>
    where Self: Sized {
        Timeout {
            stream: self,
            duration,
            deadline: None
        }
    }

    fn throttle(self, duration: Duration) -> Throttle<Self>
    where Self: Sized {
        Throttle {
            stream: self,
            duration,
            last_item: None
        }
    }

    fn chunks(self, capacity: usize) -> Chunks<Self>
    where Self: Sized {
        Chunks {
            stream: self,
            items: Vec::with_capacity(capacity),
            capacity
        }
    }

    fn buffer(self, capacity: usize) -> Buffer<Self>
    where Self: Sized, Self::Item: Unpin {
        Buffer {
            stream: self,
            items: Vec::with_capacity(capacity),
            capacity
        }
    }

    fn collect<C>(self) -> Collect<Self, C>
    where C: Default + Extend<Self::Item>, Self: Sized {
        Collect {
            stream: self,
            collection: C::default()
        }
    }

    fn fold<T, F>(self, init: T, f: F) -> Fold<Self, T, F>
    where F: FnMut(T, Self::Item) -> T, Self: Sized {
        Fold {
            stream: self,
            acc: Some(init),
            f
        }
    }
}

impl<T: ?Sized> StreamExt for T where T: Stream {}

pub struct Next<'a, S: ?Sized> {
    stream: &'a mut S
}

impl<S: Stream + Unpin + ?Sized> Future for Next<'_, S> {
    type Output = Option<S::Item>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        if chaos::should_fail(ChaosOperation::StreamNext) {
            return Poll::Pending;
        }

        Pin::new(&mut *self.stream).poll_next(cx)
    }
}

#[pin_project]
pub struct Map<S, F> {
    #[pin]
    stream: S,
    f: F
}

impl<S, F, T> Stream for Map<S, F>
where S: Stream, F: FnMut(S::Item) -> T {
    type Item = T;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        if chaos::should_fail(ChaosOperation::StreamMap) {
            return Poll::Pending;
        }

        let this = self.project();
        match this.stream.poll_next(cx) {
            Poll::Ready(Some(item)) => Poll::Ready(Some((this.f)(item))),
            Poll::Ready(None) => Poll::Ready(None),
            Poll::Pending => Poll::Pending
        }
    }
}

#[pin_project]
pub struct Filter<S, F> {
    #[pin]
    stream: S,
    f: F
}

impl<S, F> Stream for Filter<S, F>
where S: Stream, F: FnMut(&S::Item) -> bool {
    type Item = S::Item;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        if chaos::should_fail(ChaosOperation::StreamFilter) {
            return Poll::Pending;
        }

        let mut this = self.project();
        loop {
            match this.stream.as_mut().poll_next(cx) {
                Poll::Ready(Some(item)) => {
                    if (this.f)(&item) {
                        return Poll::Ready(Some(item))
                    }
                }
                Poll::Ready(None) => return Poll::Ready(None),
                Poll::Pending => return Poll::Pending
            }
        }
    }
}

#[pin_project]
pub struct FilterMap<S, F> {
    #[pin]
    stream: S,
    f: F
}

impl<S, F, T> Stream for FilterMap<S, F>
where S: Stream, F: FnMut(S::Item) -> Option<T> {
    type Item = T;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let mut this = self.project();
        loop {
            match this.stream.as_mut().poll_next(cx) {
                Poll::Ready(Some(item)) => {
                    if let Some(mapped) = (this.f)(item) {
                        return Poll::Ready(Some(mapped))
                    }
                }
                Poll::Ready(None) => return Poll::Ready(None),
                Poll::Pending => return Poll::Pending,
            }
        }
    }
}

#[pin_project]
pub struct Then<S, F, Fut> {
    #[pin]
    stream: S,
    f: F,
    #[pin]
    future: Option<Fut>
}

impl<S, F, Fut> Stream for Then<S, F, Fut>
where S: Stream, F: FnMut(S::Item) -> Fut, Fut: Future {
    type Item = Fut::Output;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let mut this = self.project();

        loop {
            if let Some(fut) = this.future.as_mut().as_pin_mut() {
                match fut.poll(cx) {
                    Poll::Ready(item) => {
                        this.future.set(None);
                        return Poll::Ready(Some(item));
                    }
                    Poll::Pending => return Poll::Pending
                }
            }

            match this.stream.as_mut().poll_next(cx) {
                Poll::Ready(Some(item)) => {
                    this.future.set(Some((this.f)(item)));
                }
                Poll::Ready(None) => return Poll::Ready(None),
                Poll::Pending => return Poll::Pending,
            }
        }
    }
}

#[pin_project]
pub struct Take<S> {
    #[pin]
    stream: S,
    remaining: usize
}

impl<S: Stream> Stream for Take<S> {
    type Item = S::Item;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        if chaos::should_fail(ChaosOperation::StreamTake) {
            return Poll::Ready(None);
        }

        let this = self.project();
        if *this.remaining == 0 {
            return Poll::Ready(None)
        }

        match this.stream.poll_next(cx) {
            Poll::Ready(Some(item)) => {
                *this.remaining -= 1;
                Poll::Ready(Some(item))
            }
            other => other
        }
    }
}

#[pin_project]
pub struct Skip<S> {
    #[pin]
    stream: S,
    remaining: usize
}

impl<S: Stream> Stream for Skip<S> {
    type Item = S::Item;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        if chaos::should_fail(ChaosOperation::StreamSkip) {
            return Poll::Pending;
        }

        let mut this = self.project();
        loop {
            match this.stream.as_mut().poll_next(cx) {
                Poll::Ready(Some(item)) => {
                    if *this.remaining == 0 {
                        return Poll::Ready(Some(item));
                    }
                    *this.remaining -= 1;
                }
                other => return other
            }
        }
    }
}

#[pin_project]
pub struct TakeWhile<S, F> {
    #[pin]
    stream: S,
    f: F,
    done: bool
}

impl<S, F> Stream for TakeWhile<S, F>
where S: Stream, F: FnMut(&S::Item) -> bool {
    type Item = S::Item;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let this = self.project();
        if *this.done {
            return Poll::Ready(None);
        }

        match this.stream.poll_next(cx) {
            Poll::Ready(Some(item)) => {
                if (this.f)(&item) {
                    Poll::Ready(Some(item))
                }
                else {
                    *this.done = true;
                    Poll::Ready(None)
                }
            }
            other => other
        }
    }
}

#[pin_project]
pub struct SkipWhile<S, F> {
    #[pin]
    stream: S,
    f: F,
    done: bool
}

impl<S, F> Stream for SkipWhile<S, F>
where S: Stream, F: FnMut(&S::Item) -> bool {
    type Item = S::Item;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let mut this = self.project();
        loop {
            match this.stream.as_mut().poll_next(cx) {
                Poll::Ready(Some(item)) => {
                    if *this.done || !(this.f)(&item) {
                        *this.done = true;
                        return Poll::Ready(Some(item));
                    }
                }
                other => return other
            }
        }
    }
}

#[pin_project]
pub struct Fuse<S> {
    #[pin]
    stream: Option<S>
}

impl<S: Stream> Stream for Fuse<S> {
    type Item = S::Item;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let mut this = self.project();
        match this.stream.as_mut().as_pin_mut() {
            Some(stream) => match stream.poll_next(cx) {
                Poll::Ready(None) => {
                    this.stream.set(None);
                    Poll::Ready(None)
                }
                other => other
            },
            None => Poll::Ready(None)
        }
    }
}

#[pin_project]
pub struct Chain<S1, S2> {
    #[pin]
    first: Option<S1>,
    #[pin]
    second: S2
}

impl<S1, S2> Stream for Chain<S1, S2>
where S1: Stream, S2: Stream<Item = S1::Item> {
    type Item = S1::Item;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        if chaos::should_fail(ChaosOperation::StreamChain) {
            return Poll::Pending;
        }

        let mut this = self.project();
        if let Some(first) = this.first.as_mut().as_pin_mut() {
            match first.poll_next(cx) {
                Poll::Ready(Some(item)) => return Poll::Ready(Some(item)),
                Poll::Ready(None) => {
                    this.first.set(None);
                }
                Poll::Pending => return Poll::Pending
            }
        }

        this.second.poll_next(cx)
    }
}

#[pin_project]
pub struct Zip<S1, S2> {
    #[pin]
    stream1: S1,
    #[pin]
    stream2: S2
}

impl<S1, S2> Stream for Zip<S1, S2>
where S1: Stream, S2: Stream {
    type Item = (S1::Item, S2::Item);

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        if chaos::should_fail(ChaosOperation::StreamZip) {
            return Poll::Pending;
        }

        let this = self.project();
        match (this.stream1.poll_next(cx), this.stream2.poll_next(cx)) {
            (Poll::Ready(Some(item1)), Poll::Ready(Some(item2))) => {
                Poll::Ready(Some((item1, item2)))
            }
            (Poll::Ready(None), _) | (_, Poll::Ready(None)) => Poll::Ready(None),
            _ => Poll::Pending
        }
    }
}

#[pin_project]
pub struct Merge<S1, S2> {
    #[pin]
    stream1: S1,
    #[pin]
    stream2: S2
}

impl<S1, S2> Stream for Merge<S1, S2>
where S1: Stream, S2: Stream<Item = S1::Item> {
    type Item = S1::Item;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        if chaos::should_fail(ChaosOperation::StreamMerge) {
            return Poll::Pending;
        }

        let this = self.project();
        match this.stream1.poll_next(cx) {
            Poll::Ready(Some(item)) => return Poll::Ready(Some(item)),
            Poll::Ready(None) => return this.stream2.poll_next(cx),
            Poll::Pending => {}
        }

        match this.stream2.poll_next(cx) {
            Poll::Ready(item) => Poll::Ready(item),
            Poll::Pending => Poll::Pending
        }
    }
}

#[pin_project]
pub struct Timeout<S> {
    #[pin]
    stream: S,
    duration: Duration,
    deadline: Option<Instant>
}

impl<S: Stream> Stream for Timeout<S> {
    type Item = Result<S::Item, ()>;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        if chaos::should_fail(ChaosOperation::StreamTimeout) {
            return Poll::Ready(Some(Err(())));
        }

        let this = self.project();

        if this.deadline.is_none() {
            *this.deadline = Some(Instant::now() + *this.duration);
        }

        if Instant::now() >= this.deadline.unwrap() {
            return Poll::Ready(Some(Err(())));
        }

        match this.stream.poll_next(cx) {
            Poll::Ready(Some(item)) => {
                *this.deadline = Some(Instant::now() + *this.duration);
                Poll::Ready(Some(Ok(item)))
            }
            Poll::Ready(None) => Poll::Ready(None),
            Poll::Pending => {
                cx.waker().wake_by_ref();
                Poll::Pending
            }
        }
    }
}

#[pin_project]
pub struct Throttle<S> {
    #[pin]
    stream: S,
    duration: Duration,
    last_item: Option<Instant>
}

impl<S: Stream> Stream for Throttle<S> {
    type Item = S::Item;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        if chaos::should_fail(ChaosOperation::StreamThrottle) {
            return Poll::Pending;
        }

        let this = self.project();

        if let Some(last) = this.last_item {
            if Instant::now() < *last + *this.duration {
                cx.waker().wake_by_ref();
                return Poll::Pending;
            }
        }

        match this.stream.poll_next(cx) {
            Poll::Ready(Some(item)) => {
                *this.last_item = Some(Instant::now());
                Poll::Ready(Some(item))
            }
            other => other
        }
    }
}

#[pin_project]
pub struct Chunks<S: Stream> {
    #[pin]
    stream: S,
    items: Vec<S::Item>,
    capacity: usize
}

impl<S: Stream> Stream for Chunks<S> {
    type Item = Vec<S::Item>;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let mut this = self.project();

        loop {
            match this.stream.as_mut().poll_next(cx) {
                Poll::Ready(Some(item)) => {
                    this.items.push(item);
                    if this.items.len() >= *this.capacity {
                        let items = std::mem::replace(this.items, Vec::with_capacity(*this.capacity));
                        return Poll::Ready(Some(items));
                    }
                }
                Poll::Ready(None) => {
                    return if this.items.is_empty() {
                        Poll::Ready(None)
                    }
                    else {
                        let items = std::mem::take(this.items);
                        Poll::Ready(Some(items))
                    };
                }
                Poll::Pending => {
                    return if this.items.is_empty() {
                        Poll::Pending
                    }
                    else {
                        let items = std::mem::replace(this.items, Vec::with_capacity(*this.capacity));
                        Poll::Ready(Some(items))
                    }
                }
            }
        }
    }
}

#[pin_project]
pub struct Buffer<S: Stream> {
    #[pin]
    stream: S,
    items: Vec<S::Item>,
    capacity: usize
}

impl<S: Stream> Stream for Buffer<S>
where S::Item: Unpin {
    type Item = S::Item;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        if chaos::should_fail(ChaosOperation::StreamBuffer) {
            return Poll::Pending;
        }

        let mut this = self.project();

        while this.items.len() < *this.capacity {
            match this.stream.as_mut().poll_next(cx) {
                Poll::Ready(Some(item)) => this.items.push(item),
                Poll::Ready(None) => break,
                Poll::Pending => break
            }
        }

        if !this.items.is_empty() {
            Poll::Ready(Some(this.items.remove(0)))
        }
        else {
            Poll::Pending
        }
    }
}

#[pin_project]
pub struct Collect<S, C> {
    #[pin]
    stream: S,
    collection: C
}

impl<S, C> Future for Collect<S, C>
where S: Stream, C: Default + Extend<S::Item> {
    type Output = C;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        if chaos::should_fail(ChaosOperation::StreamCollect) {
            return Poll::Pending;
        }

        let mut this = self.project();

        loop {
            match this.stream.as_mut().poll_next(cx) {
                Poll::Ready(Some(item)) => this.collection.extend(std::iter::once(item)),
                Poll::Ready(None) => {
                    return Poll::Ready(std::mem::take(this.collection));
                }
                Poll::Pending => return Poll::Pending
            }
        }
    }
}

#[pin_project]
pub struct Fold<S, T, F> {
    #[pin]
    stream: S,
    acc: Option<T>,
    f: F
}

impl<S, T, F> Future for Fold<S, T, F>
where S: Stream, F: FnMut(T, S::Item) -> T {
    type Output = T;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut this = self.project();

        loop {
            match this.stream.as_mut().poll_next(cx) {
                Poll::Ready(Some(item)) => {
                    let acc = this.acc.take().unwrap();
                    *this.acc = Some((this.f)(acc, item));
                }
                Poll::Ready(None) => return Poll::Ready(this.acc.take().unwrap()),
                Poll::Pending => return  Poll::Pending
            }
        }
    }
}

pub struct IntervalStream {
    inner: Interval
}

impl IntervalStream {
    pub fn new(period: Duration) -> Self {
        Self { inner: interval(period) }
    }
}

impl Stream for IntervalStream {
    type Item = Instant;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        Poll::Ready(Some(std::task::ready!(self.inner.poll_tick(cx))))
    }
}

pub struct TcpListenerStream {
    listener: TcpListener
}

impl TcpListenerStream {
    pub fn new(listener: TcpListener) -> Self {
        Self { listener }
    }
}

impl Stream for TcpListenerStream {
    type Item = std::io::Result<(TcpStream, SocketAddr)>;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        // Placeholder, need proper async handling
        Poll::Pending
    }
}

pub fn interval_stream(period: Duration) -> IntervalStream {
    IntervalStream::new(period)
}

pub fn tcp_listener_stream(listener: TcpListener) -> TcpListenerStream {
    TcpListenerStream::new(listener)
}