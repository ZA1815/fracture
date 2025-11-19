use std::{future::Future, pin::Pin};
use std::time::Duration;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TaskId(pub usize);

pub(crate) struct Task {
    pub future: Pin<Box<dyn Future<Output = ()>>>,
    pub name: Option<String>,
    pub spawn_time: Duration,
    pub context: HashMap<String, String>
}

impl Task {
    pub fn new(future: Pin<Box<dyn Future<Output = ()>>>, name: Option<String>, spawn_time: Duration) -> Self {
        Self { future, name, spawn_time, context: HashMap::new() }
    }
}