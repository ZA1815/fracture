use std::fmt;
use std::sync::LazyLock;
use parking_lot::RwLock;
use tokio::time::{Duration, Instant};

#[derive(Debug, Clone)]
pub enum TraceEvent {
    ChaosInjected {
        operation: String,
        rate: f64,
        result: bool
    },
    Partition {
        from: String,
        to: String,
        oneway: bool
    },
    PartitionHealed {
        from: String,
        to: String
    },
    PacketLossSet {
        node: String,
        rate: f64
    },
    DelaySet {
        node: String,
        min: Duration,
        max: Duration
    },
    ReorderingSet {
        node: String,
        rate: f64
    },
    ScenarioOp(String),
    InvariantChecked {
        name: String,
        holds: bool
    }
}

impl fmt::Display for TraceEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone)]
pub struct TraceEntry {
    pub timestamp: Instant,
    pub event: TraceEvent
}

#[derive(Default)]
pub struct Tracer {
    log: RwLock<Vec<TraceEntry>>,
    enabled: RwLock<bool>
}

impl Tracer {
    pub fn new() -> Self {
        Self {
            log: RwLock::new(Vec::new()),
            enabled: RwLock::new(true)
        }
    }

    pub fn record(&self, event: TraceEvent) {
        if !*self.enabled.read() {
            return;
        }

        self.log.write().push(TraceEntry { timestamp: Instant::now(), event });
    }

    pub fn clear(&self) {
        self.log.write().clear();
    }

    pub fn disable(&self) {
        *self.enabled.write() = false;
    }

    pub fn enable(&self) {
        *self.enabled.write() = true;
    }

    pub fn get_log(&self) -> Vec<TraceEntry> {
        self.log.read().clone()
    }
}

pub static GLOBAL_TRACER: LazyLock<Tracer> = LazyLock::new(Tracer::new);

#[inline]
pub fn record(event: TraceEvent) {
    GLOBAL_TRACER.record(event);
}

pub fn clear_trace() {
    GLOBAL_TRACER.clear();
}

pub fn disable_tracing() {
    GLOBAL_TRACER.disable();
}

pub fn enable_tracing() {
    GLOBAL_TRACER.enable();
}

pub fn get_trace() -> Vec<TraceEntry> {
    GLOBAL_TRACER.get_log()
}