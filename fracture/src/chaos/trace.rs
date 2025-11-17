use std::collections::{HashMap, HashSet};
use std::fmt;
use std::sync::LazyLock;
use parking_lot::RwLock;
use tokio::time::{Duration, Instant};
use std::time::SystemTime;
use serde::{Serialize, Deserialize};
use std::fs::File;
use std::io::Write;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
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
    PacketDropped {
        node: String,
        packet_id: u64
    },
    DelaySet {
        node: String,
        min: Duration,
        max: Duration
    },
    DelayInjected {
        node: String,
        duration: Duration
    },
    ReorderingSet {
        node: String,
        rate: f64
    },
    PacketReordered {
        node: String,
        packet_id: u64,
        delay: Duration
    },
    ScenarioOp(String),
    InvariantChecked {
        name: String,
        holds: bool
    },
    InvariantViolated {
        name: String,
        message: String
    },
    ConnectionAttempt {
        from: String,
        to: String,
        addr: String
    },
    ConnectionSuccess {
        from: String,
        to: String
    },
    ConnectionFailed {
        from: String,
        to: String,
        error: String
    },
    DataSent {
        from: String,
        to: String,
        bytes: usize
    },
    DataRecieved {
        from: String,
        to: String,
        bytes: usize
    },
    UserMarker {
        label: String,
        data: String
    }
}

impl TraceEvent {
    pub fn to_symbol(&self) -> char {
        use TraceEvent::*;
        match self {
            ChaosInjected { result: true, .. } => '!',
            ChaosInjected { result: false, .. } => '.',
            Partition { oneway: true, .. } => '→',
            Partition { oneway: false, .. } => '↔',
            PartitionHealed { .. } => '✓',
            PacketDropped { .. } => '×',
            DelayInjected { .. } => '~',
            InvariantViolated { .. } => '❌',
            InvariantChecked { holds: false, .. } => '⚠',
            ConnectionFailed { .. } => '✗',
            ConnectionSuccess { .. } => '✔',
            ConnectionAttempt { .. } => '○',
            UserMarker { .. } => '▪',
            _ => '·',
        }
    }

    pub fn legend() -> String {
        vec![
            "! = Chaos Triggered",
            "↔ = Partition",
            "→ = One-way Partition",
            "✓ = Healed",
            "× = Packet Dropped",
            "~ = Delay",
            "❌ = Violation",
            "✗ = Failed",
            "▪ = Marker",
        ].join(", ")
    }

    pub fn is_failure(&self) -> bool {
        use TraceEvent::*;
        matches!(self,
            ConnectionFailed { .. } |
            InvariantViolated { .. } |
            PacketDropped { .. } |
            InvariantChecked { holds: false, .. }
        )
    }

    pub fn is_chaos(&self) -> bool {
        use TraceEvent::*;
        matches!(self,
            ChaosInjected { .. } |
            Partition { .. } |
            PacketLossSet { .. } |
            DelaySet { .. } |
            ReorderingSet { .. }
        )
    }

    pub fn severity(&self) -> Severity {
        use TraceEvent::*;
        match self {
            InvariantViolated { .. } => Severity::Critical,
            ConnectionFailed { .. } | PacketDropped { .. } => Severity::Error,
            ChaosInjected { result: true, .. } => Severity::Warning,
            Partition { .. } => Severity::Warning,
            InvariantChecked { holds: false, .. } => Severity::Warning,
            UserMarker { .. } => Severity::Info,
            _ => Severity::Debug,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Debug,
    Info,
    Warning,
    Error,
    Critical,
}

impl Severity {
    pub fn color(&self) -> &'static str {
        match self {
            Severity::Debug => "\x1b[90m",
            Severity::Info => "\x1b[36m",
            Severity::Warning => "\x1b[33m",
            Severity::Error => "\x1b[31m",
            Severity::Critical => "\x1b[91m",
        }
    }
    
    pub fn reset() -> &'static str {
        "\x1b[0m"
    }
}

impl fmt::Display for TraceEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TraceEvent::*;
        match self {
            ChaosInjected { operation, rate, result } => {
                write!(f, "🎲 Chaos: {} ({}%) = {}", operation, rate * 100.0, 
                    if *result { "TRIGGERED" } else { "skipped" })
            }
            Partition { from, to, oneway } => {
                if *oneway {
                    write!(f, "🔌 Partition: {} → {} (one-way)", from, to)
                } else {
                    write!(f, "🔌 Partition: {} ⟷ {}", from, to)
                }
            }
            PartitionHealed { from, to } => {
                write!(f, "✅ Partition Healed: {} ⟷ {}", from, to)
            }
            InvariantViolated { name, message } => {
                write!(f, "❌ VIOLATION: {} - {}", name, message)
            }
            _ => write!(f, "{:?}", self)
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraceEntry {
    pub timestamp: Instant, // FIX
    pub system_time: SystemTime,
    pub thread_id: String,
    pub event: TraceEvent
}

#[derive(Debug, Serialize)]
struct ChromeTraceEvent {
    name: String,
    cat: String,
    ph: String,
    ts: f64,
    pid: u32,
    tid: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    dur: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    args: Option<serde_json::Value>,
}

#[derive(Default)]
pub struct Tracer {
    log: RwLock<Vec<TraceEntry>>,
    enabled: RwLock<bool>,
    start_time: Instant,
    start_system_time: SystemTime
}

#[derive(Debug, Clone)]
pub enum BugPattern {
    SplitBrain {
        timestamp: Instant,
        active_partitions: HashSet<String>
    },
    CascadingFailure {
        timestamp: Instant,
        failure_count: usize,
        duration: Duration
    },
    DataRace {
        timestamp: Instant,
        conflicting_operations: Vec<String>
    },
    Livelock {
        timestamp: Instant,
        repeated_operations: Vec<String>,
        cycle_count: usize
    },
    ThunderingHerd {
        timestamp: Instant,
        simultaneous_requests: usize
    },
    PoisonPillPropagation {
        timestamp: Instant,
        origin: String,
        affected_nodes: Vec<String>
    }
}

impl BugPattern {
    pub fn is_critical(&self) -> bool {
        match self {
            Self::SplitBrain { .. } => true,
            Self::DataRace { .. } => true,
            Self::PoisonPillPropagation { .. } => true,
            Self::CascadingFailure { failure_count, .. } if *failure_count > 5 => true,
            _ => false,
        }
    }

    pub fn description(&self) -> String {
        match self {
            Self::SplitBrain { active_partitions, .. } => {
                format!("Split-brain detected with {} active partitions", active_partitions.len())
            }
            Self::CascadingFailure { failure_count, duration, .. } => {
                format!("{} failures cascaded over {:?}", failure_count, duration)
            }
            Self::DataRace { conflicting_operations, .. } => {
                format!("Data race between {} operations", conflicting_operations.len())
            }
            Self::Livelock { cycle_count, .. } => {
                format!("Livelock detected: {} cycles", cycle_count)
            }
            Self::ThunderingHerd { simultaneous_requests, .. } => {
                format!("Thundering herd: {} simultaneous requests", simultaneous_requests)
            }
            Self::PoisonPillPropagation { affected_nodes, .. } => {
                format!("Poison pill affected {} nodes", affected_nodes.len())
            }
        }
    }
}

impl Tracer {
    pub fn new() -> Self {
        Self {
            log: RwLock::new(Vec::new()),
            enabled: RwLock::new(true),
            start_time: Instant::now(),
            start_system_time: SystemTime::now()
        }
    }

    pub fn record(&self, event: TraceEvent) {
        if !*self.enabled.read() {
            return;
        }

        let thread_id = format!("{:?}", std::thread::current().id());

        self.log.write().push(TraceEntry { timestamp: Instant::now(), system_time: SystemTime::now(), thread_id, event });
    }

    pub fn mark(&self, label: impl Into<String>, data: impl Into<String>) {
        self.record(TraceEvent::UserMarker { label: label.into(), data: data.into() });
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

    pub fn export_chrome_trace(&self, path: &str) -> std::io::Result<()> {
        let log = self.log.read();
        let mut events = Vec::new();

        for entry in log.iter() {
            let ts = entry.timestamp.duration_since(self.start_time).as_micros() as f64;

            let (name, cat, args) = match &entry.event {
                TraceEvent::ChaosInjected { operation, rate, result } => {
                    let args = serde_json::json!({
                        "operation": operation,
                        "rate": rate,
                        "result": result
                    });
                    (format!("chaos:{}", operation), "chaos", Some(args))
                }
                TraceEvent::Partition { from, to, oneway } => {
                    let args = serde_json::json!({
                        "from": from,
                        "to": to,
                        "oneway": oneway
                    });
                    ("partition".to_string(), "network", Some(args))
                }
                TraceEvent::InvariantViolated { name, message } => {
                    let args = serde_json::json!({
                        "invariant": name,
                        "message": message
                    });
                    (format!("violation:{}", name), "invariant", Some(args))
                }
                TraceEvent::ConnectionAttempt { from, to, addr } => {
                    let args = serde_json::json!({
                        "from": from,
                        "to": to,
                        "addr": addr
                    });
                    ("connect".to_string(), "network", Some(args))
                }
                _ => (format!("{:?}", entry.event), "misc", None)
            };

            events.push(ChromeTraceEvent {
                name,
                cat: cat.to_string(),
                ph: "i".to_string(),
                ts,
                pid: std::process::id(),
                tid: entry.thread_id.clone(),
                dur: None,
                args
            });
        }

        let mut file = File::create(path)?;
        writeln!(file, "{}", serde_json::to_string_pretty(&events)?)?;

        Ok(())
    }

    pub fn export_json(&self, path: &str) -> std::io::Result<()> {
        let log = self.get_log();
        let mut file = File::create(path)?;
        writeln!(file, "{}", serde_json::to_string_pretty(&log)?)?;
        Ok(())
    }

    pub fn export_timeline(&self, path: &str) -> std::io::Result<()> {
        let log = self.get_log();
        let mut file = File::create(path)?;

        writeln!(file, "=== Fracture Chaos Timeline ===")?;
        writeln!(file, "Start: {:?}", self.start_system_time);
        writeln!(file)?;

        for entry in log.iter() {
            let elapsed = entry.timestamp.duration_since(self.start_time);
            writeln!(file, "[{:>8.3}s] {}", elapsed.as_secs_f64(), entry.event)?;
        }

        Ok(())
    }

    pub fn find_patterns(&self) -> Vec<BugPattern> {
        let log = self.log.read();
        let mut detector = PatternDetector::new();

        for (i, entry) in log.iter().enumerate() {
            detector.process(i, entry);
        }

        detector.get_patterns();
    }
}

struct PatternDetector {
    patterns: Vec<BugPattern>,
    active_partitions: HashSet<String>,
    partition_times: HashMap<String, Instant>,
    recent_failures: Vec<(Instant, String)>,
    cascade_window: Duration,
    operation_history: Vec<(String, Instant)>,
    livelock_window: Duration,
    concurrent_operations: HashMap<String, Vec<(String, Instant)>>,
    request_bursts: Vec<(Instant, String)>
}

impl PatternDetector {
    fn new() -> Self {
        Self {
            patterns: Vec::new(),
            active_partitions: std::collections::HashSet::new(),
            partition_times: std::collections::HashMap::new(),
            recent_failures: Vec::new(),
            cascade_window: Duration::from_secs(5),
            operation_history: Vec::new(),
            livelock_window: Duration::from_secs(10),
            concurrent_operations: std::collections::HashMap::new(),
            request_bursts: Vec::new(),
        }
    }

    fn process(&mut self, _index: usize, entry: &TraceEntry) {
        use TraceEvent::*;
        
        match &entry.event {
            Partition { from, to, oneway } => {
                self.handle_partition(entry.timestamp, from, to, *oneway);
            }
            
            PartitionHealed { from, to } => {
                self.handle_partition_healed(from, to);
            }
            
            ConnectionFailed { from, to, error } => {
                self.handle_connection_failure(entry.timestamp, from, to, error);
            }
            
            InvariantViolated { name, message } => {
                self.handle_invariant_violation(entry.timestamp, name, message);
            }
            
            ConnectionAttempt { from, to, .. } => {
                self.check_thundering_herd(entry.timestamp, from, to);
            }
            
            DataSent { from, to, bytes } | DataReceived { from, to, bytes } => {
                self.check_data_race(entry.timestamp, from, to, *bytes);
            }
            
            ChaosInjected { operation, result: true, .. } => {
                self.track_operation(entry.timestamp, operation);
            }
            
            _ => {}
        }
        
        self.cleanup_old_state(entry.timestamp);
    }

    fn handle_partition(&mut self, timestamp: Instant, from: &str, to: &str, oneway: bool) {
        let key = if oneway {
            format!("{}→{}", from, to)
        }
        else {
            format!("{}-{}", from.min(to), from.max(to))
        };

        self.active_partitions.insert(key.clone());
        self.partition_times.insert(key, timestamp);
    }

    fn handle_partition_healed(&mut self, from: &str, to: &str) {
        self.active_partitions.remove(&format!("{}-{}", from.min(to), from.max(to)));
        self.active_partitions.remove(&format!("{}→{}", from, to));
        self.active_partitions.remove(&format!("{}→{}", to, from));
    }

    fn handle_connection_failure(&mut self, timestamp: Instant, from: &str, to: &str, error: &str) {
        let failure_desc = format!("{}→{}: {}", from, to, error);
        self.recent_failures.push((timestamp, failure_desc));

        let recent_count = self.recent_failures
            .iter()
            .filter(|(t, _)| timestamp.duration_since(*t) < self.cascade_window)
            .count();

        if recent_count >= 3 {
            let oldest_in_cascade = self.recent_failures
                .iter()
                .filter(|(t, _)| timestamp.duration_since(*t) < self.cascade_window)
                .map(|(t, _)| *t)
                .min()
                .unwrap_or(timestamp);

            self.patterns.push(BugPattern::CascadingFailure { timestamp, failure_count: recent_count, duration: timestamp.duration_since(oldest_in_cascade) });

            let error_groups = self.recent_failures
                .iter()
                .filter(|(t, _)| timestamp.duration_since(*t) < self.cascade_window)
                .fold(HashMap::new(), |mut acc, (_, desc)| {
                    let parts: Vec<_> = desc.split(": ").collect();
                    if parts.len() >= 2 {
                        let error_type = parts[1];
                        *acc.entry(error_type).or_insert(0) += 1;
                    }
                    acc
                });
            
            for (error_type, count) in error_groups {
                if count >= 3 {
                    let affected_nodes: Vec<String> = self.recent_failures
                        .iter()
                        .filter(|(_, desc)| desc.contains(error_type))
                        .map(|(_, desc)| desc.split("→").next().unwrap_or("").to_string())
                        .collect::<HashSet<_>>()
                        .into_iter()
                        .collect();

                    self.patterns.push(BugPattern::PoisonPillPropagation { timestamp, origin: from.to_string(), affected_nodes });
                }
            }
        }
    }

    fn handle_invariant_violation(&mut self, timestamp: Instant, name: &str, message: &str) {
        match name {
            n if n.contains("split_brain") || n.contains("multiple_leaders") => {
                if !self.active_partitions.is_empty() {
                    self.patterns.push(BugPattern::SplitBrain { timestamp, active_partitions: self.active_partitions.clone() });
                }
            }
            n if n.contains("data_race") || n.contains("concurrent") => {
                let recent_ops: Vec<String> = self.concurrent_operations
                    .values()
                    .flat_map(|ops| ops.iter())
                    .filter(|(_, t)| timestamp.duration_since(*t) < Duration::from_secs(1))
                    .map(|(op, _)| op.clone())
                    .collect();

                if recent_ops.len() >= 2 {
                    self.patterns.push(BugPattern::DataRace { timestamp, conflicting_operations: recent_ops });
                }
            }
            _ => {}
        }
    }

    fn check_thundering_herd(&mut self, timestamp: Instant, from: &str, to: &str) {
        self.request_bursts.push((timestamp, format!("{}→{}", from, to)));

        let burst_window = Duration::from_millis(100);
        let simultaneous = self.request_bursts
            .iter()
            .filter(|(t, _)| timestamp.duration_since(*t) < burst_window)
            .count();

        if simultaneous >= 10 {
            self.patterns.push(BugPattern::ThunderingHerd { timestamp, simultaneous_requests: simultaneous });
        }
    }

    fn check_data_race(&mut self, timestamp: Instant, from: &str, to: &str, bytes: usize) {
        let resource = format!("{}:{}", to, bytes);
        let operation = format!("{}→{}", from, to);

        self.concurrent_operations
            .entry(resource)
            .or_insert_with(Vec::new)
            .push((operation, timestamp));
    }

    fn track_operation(&mut self, timestamp: Instant, operation: &str) {
        self.operation_history.push((operation.to_string(), timestamp));

        if self.operation_history.len() >= 10 {
            let recent: Vec<&str> = self.operation_history
                .iter()
                .rev()
                .take(10)
                .map(|(op, _)| op.as_str())
                .collect();

            if recent.len() >= 6 {
                let pattern_len = 3;
                let pattern = &recent[0..pattern_len];
                let repeat = &recent[pattern_len..pattern_len * 2];

                if pattern == repeat {
                    let cycle_count = recent.chunks(pattern_len)
                        .filter(|chunk| chunk == &pattern)
                        .count();

                    if cycle_count >= 2 {
                        self.patterns.push(BugPattern::Livelock {
                            timestamp,
                            repeated_operations: pattern.iter().map(|s| s.to_string()).collect(),
                            cycle_count
                        });
                    }
                }
            }
        }
    }

    fn cleanup_old_state(&mut self, current_time: Instant) {
        self.recent_failures.retain(|(t, _)| current_time.duration_since(*t) < Duration::from_secs(30));

        self.request_bursts.retain(|(t, _)| current_time.duration_since(*t) < Duration::from_secs(1));

        for ops in self.concurrent_operations.values_mut() {
            ops.retain(|(_, t)| current_time.duration_since(*t) < Duration::from_secs(5));
        }

        if self.operation_history.len() > 100 {
            self.operation_history.drain(0..50);
        }
    }

    fn get_patterns(self) -> Vec<BugPattern> {
        self.patterns
    }
}

pub static GLOBAL_TRACER: LazyLock<Tracer> = LazyLock::new(Tracer::new);

#[inline]
pub fn record(event: TraceEvent) {
    GLOBAL_TRACER.record(event);
}

pub fn mark(label: impl Into<String>, data: impl Into<String>) {
    GLOBAL_TRACER.mark(label, data);
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

pub fn export_chrome(path: &str) -> std::io::Result<()> {
    GLOBAL_TRACER.export_chrome_trace(path)
}

pub fn export_json(path: &str) -> std::io::Result<()> {
    GLOBAL_TRACER.export_json(path)
}

pub fn export_timeline(path: &str) -> std::io::Result<()> {
    GLOBAL_TRACER.export_timeline(path)
}

pub fn find_bugs() -> Vec<BugPattern> {
    GLOBAL_TRACER.find_patterns()
}

pub struct TimeTravel {
    trace: Vec<TraceEntry>,
    position: usize
}

impl TimeTravel {
    pub fn new(trace: Vec<TraceEntry>) -> Self {
        Self { trace, position: 0 }
    }

    pub fn from_current() -> Self {
        Self::new(get_trace())
    }

    pub fn step_forward(&mut self) -> Option<&TraceEntry> {
        if self.position < self.trace.len() {
            let entry = &self.trace[self.position];
            self.position += 1;
            Some(entry)
        }
        else {
            None
        }
    }

    pub fn step_backward(&mut self) -> Option<&TraceEntry> {
        if self.position > 0 {
            self.position -= 1;
            Some(&self.trace[self.position])
        }
        else {
            None
        }
    }

    pub fn jump_to(&mut self, position: usize) -> Option<&TraceEntry> {
        if position < self.trace.len() {
            self.position = position;
            Some(&self.trace[position])
        }
        else {
            None
        }
    }

    pub fn find_next(&mut self, predicate: impl Fn(&TraceEvent) -> bool) -> Option<TraceEntry> {
        while let Some(entry) = self.step_forward() {
            if predicate(&entry.event) {
                return Some(entry.clone());
            }
        }
        None
    }

    pub fn current(&self) -> Option<&TraceEntry> {
        if self.position < self.trace.len() {
            Some(&self.trace[self.position])
        }
        else {
            None
        }
    }

    pub fn position(&self) -> usize {
        self.position
    }

    pub fn len(&self) -> usize {
        self.trace.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_trace_recording() {
        let tracer = Tracer::new();

        tracer.record(TraceEvent::UserMarker { label: "test".to_string(), data: "start".to_string() });

        tracer.record(TraceEvent::ChaosInjected { operation: "tcp_connect".to_string(), rate: 0.1, result: true });

        let log = tracer.get_log();
        assert_eq!(log.len(), 2);
    }

    #[test]
    fn test_time_travel() {
        clear_trace();

        record(TraceEvent::UserMarker { label: "1".to_string(), data: "first".to_string() });

        record(TraceEvent::UserMarker { label: "2".to_string(), data: "second".to_string() });

        let mut tt = TimeTravel::from_current();
        assert_eq!(tt.position(), 0);

        let first = tt.step_forward().unwrap();
        if let TraceEvent::UserMarker { label, .. } = &first.event {
            assert_eq!(label, "1");
        }

        let second = tt.step_forward().unwrap();
        if let TraceEvent::UserMarker { label, .. } = &second.event {
            assert_eq!(label, "2");
        }

        let back = tt.step_backward().unwrap();
        if let TraceEvent::UserMarker { label, .. } = &back.event {
            assert_eq!(label, "1")
        }
    }
}