use std::{future::Future, pin::Pin};
use std::time::Duration;
use std::collections::HashMap;
use std::any::{Any, TypeId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TaskId(pub usize);

pub(crate) struct Task {
    pub future: Pin<Box<dyn Future<Output = ()>>>,
    pub name: Option<String>,
    pub spawn_time: Duration,
    pub context: HashMap<String, String>,
    pub locals: TaskLocals
}

impl Task {
    pub fn new(future: Pin<Box<dyn Future<Output = ()>>>, name: Option<String>, spawn_time: Duration) -> Self {
        Self { future, name, spawn_time, context: HashMap::new(), locals: TaskLocals::new() }
    }
}

pub(crate) struct TaskLocals {
    map: HashMap<TypeId, Box<dyn Any>>
}

impl TaskLocals {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn insert<T: 'static>(&mut self, value: T) -> Option<T> {
        self.map
            .insert(TypeId::of::<T>(), Box::new(value))
            .and_then(|boxed| boxed.downcast().ok().map(|v| *v))
    }

    pub fn get<T: 'static>(&self) -> Option<&T> {
        self.map
            .get(&TypeId::of::<T>())
            .and_then(|boxed| boxed.downcast_ref())
    }

    pub fn remove<T: 'static>(&mut self) -> Option<T> {
        self.map
            .remove(&TypeId::of::<T>())
            .and_then(|boxed| boxed.downcast().ok().map(|v| *v))
    }
}