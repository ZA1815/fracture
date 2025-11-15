use std::sync::atomic::{AtomicU64, AtomicBool, Ordering};
use std::sync::LazyLock;
use std::time::Duration;
use dashmap::DashMap;
use rand::{SeedableRng, Rng};
use rand_chacha::ChaCha8Rng;

mod topology;
pub mod scenario;
pub mod invariants;
mod trace;
mod visualization;

use trace::TraceEvent;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FailureType {
    ConnectionRefused,
    ReadFailure,
    WriteFailure,
    Timeout,
    Partitioned,
    PacketLoss,
    Delay,
    Reorder
}

#[derive(Debug, Clone)]
pub struct FailureConfig {
    pub rate: f64,
    pub duration: Option<Duration>
}

#[derive(Debug, Clone)]
pub struct DelayConfig {
    pub min: Duration,
    pub max: Duration
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ChaosOperation {
    TcpConnect,
    TcpRead,
    TcpWrite,
    TcpReadBytes,
    TcpWriteBytes,
    TcpPartialWrite,
    TcpAccept,
    UdpSend,
    UdpRecv,
    TimeSkew,
    TimeoutEarly,
    TimeoutAtEarly, 
    IntervalPeriodSkew,
    IntervalStartShift,
    SleepUntilShift,
    FsRead,
    FsWrite,
    FsOpen,
    FsCreate,
    FsRemoveFile,
    FsReadDir,
    TaskSpawn,
    TaskYield,
    SyncMutexLock,
    SyncRwLockRead,
    SyncRwLockWrite
}

pub struct ChaosState {
    pub enabled: AtomicBool,
    pub seed: AtomicU64,
    pub failure_rates: DashMap<ChaosOperation, FailureConfig>,
    pub partitions: DashMap<(String, String), bool>,
    pub packet_loss: DashMap<String, f64>,
    pub delays: DashMap<String, DelayConfig>,
    pub reordering: DashMap<String, f64>,
    pub topology: parking_lot::RwLock<topology::NetworkTopology>
}

static CHAOS: LazyLock<ChaosState> = LazyLock::new(|| ChaosState {
    enabled: AtomicBool::new(false),
    seed: AtomicU64::new(rand::random()),
    failure_rates: DashMap::new(),
    partitions: DashMap::new(),
    packet_loss: DashMap::new(),
    delays: DashMap::new(),
    reordering: DashMap::new(),
    topology: parking_lot::RwLock::new(topology::NetworkTopology::new())
});

#[inline]
pub fn should_fail(operation: ChaosOperation) -> bool {
    if !CHAOS.enabled.load(Ordering::Relaxed) {
        return false;
    }

    let op_name = format!("{:?}", operation);

    let rate = CHAOS.failure_rates
        .get(&operation)
        .map(|config| config.rate)
        .unwrap_or(0.0);

    let result = CHAOS.failure_rates.get(&operation).map(|config| {
        let seed = CHAOS.seed.load(Ordering::Relaxed);
        let hash = hash_operation(seed, &op_name);
        let roll = hash as f64 / u64::MAX as f64;
        roll < config.rate
    })
    .unwrap_or(false);

    trace::record(TraceEvent::ChaosInjected { operation: op_name, rate, result });
    result
}

pub fn inject(operation: ChaosOperation, failure_rate: f64) {
    CHAOS.failure_rates.insert(operation, FailureConfig { rate: failure_rate, duration: None });
    CHAOS.enabled.store(true, Ordering::Relaxed);
}

pub fn inject_temporary(operation: ChaosOperation, failure_rate: f64, duration: Duration) {
    CHAOS.failure_rates.insert(operation, FailureConfig { rate: failure_rate, duration: Some(duration) });
    CHAOS.enabled.store(true, Ordering::Relaxed);
}

pub fn partition(from: &str, to: &str) {
    CHAOS.partitions.insert((from.to_string(), to.to_string()), true);
    CHAOS.partitions.insert((to.to_string(), from.to_string()), true);
    CHAOS.enabled.store(true, Ordering::Relaxed);

    trace::record(TraceEvent::Partition { from: from.to_string(), to: to.to_string(), oneway: false });
}

pub fn partition_oneway(from: &str, to: &str) {
    CHAOS.partitions.insert((from.to_string(), to.to_string()), true);
    CHAOS.enabled.store(true, Ordering::Relaxed);

    trace::record(TraceEvent::Partition { from: from.to_string(), to: to.to_string(), oneway: true });
}

pub fn heal_partition(from: &str, to: &str) {
    CHAOS.partitions.remove(&(from.to_string(), to.to_string()));
    CHAOS.partitions.remove(&(to.to_string(), from.to_string()));

    trace::record(TraceEvent::PartitionHealed { from: from.to_string(), to: to.to_string() });
}

#[inline]
pub fn is_partitioned(from: &str, to: &str) -> bool {
    CHAOS.partitions.get(&(from.to_string(), to.to_string())).map(|v| *v).unwrap_or(false)
}

pub fn set_packet_loss(node: &str, rate: f64) {
    CHAOS.packet_loss.insert(node.to_string(), rate.clamp(0.0, 1.0));
    CHAOS.enabled.store(true, Ordering::Relaxed);

    trace::record(TraceEvent::PacketLossSet { node: node.to_string(), rate });
}

#[inline]
pub fn should_drop_packet(node: &str) -> bool {
    CHAOS.packet_loss.get(node).map(|rate| {
        let seed = CHAOS.seed.load(Ordering::Relaxed);
        let hash = hash_operation(seed, node);
        (hash as f64 / u64::MAX as f64) < *rate
    })
    .unwrap_or(false)
}

pub fn set_delay(node: &str, min: Duration, max: Duration) {
    CHAOS.delays.insert(node.to_string(), DelayConfig { min, max });
    CHAOS.enabled.store(true, Ordering::Relaxed);

    trace::record(TraceEvent::DelaySet { node: node.to_string(), min, max });
}

pub fn get_delay(node: &str) -> Option<Duration> {
    CHAOS.delays.get(node).map(|config| {
        let seed = CHAOS.seed.load(Ordering::Relaxed);
        let mut rng = ChaCha8Rng::seed_from_u64(seed);
        let min_ms = config.min.as_millis() as u64;
        let max_ms = config.max.as_millis() as u64;
        let delay_ms = rng.gen_range(min_ms..=max_ms);
        Duration::from_millis(delay_ms)
    })
}

pub fn set_reordering(node: &str, rate: f64) {
    CHAOS.reordering.insert(node.to_string(), rate.clamp(0.0, 1.0));
    CHAOS.enabled.store(true, Ordering::Relaxed);

    trace::record(TraceEvent::ReorderingSet { node: node.to_string(), rate });
}

#[inline]
pub fn should_reorder(node: &str) -> bool {
    CHAOS.reordering.get(node).map(|rate| {
        let seed = CHAOS.seed.load(Ordering::Relaxed);
        let hash = hash_operation(seed, node);
        (hash as f64 / u64::MAX as f64) < *rate
    })
    .unwrap_or(false)
}

pub fn clear() {
    CHAOS.failure_rates.clear();
    CHAOS.partitions.clear();
    CHAOS.packet_loss.clear();
    CHAOS.delays.clear();
    CHAOS.reordering.clear();
    CHAOS.enabled.store(false, Ordering::Relaxed);
}

pub fn get_seed() -> u64 {
    CHAOS.seed.load(Ordering::Relaxed)
}

pub fn set_seed(seed: u64) {
    CHAOS.seed.store(seed, Ordering::Relaxed);
}

fn hash_operation(seed: u64, s: &str) -> u64 {
    let mut hash = seed;
    for byte in s.bytes() {
        hash = hash.wrapping_mul(31).wrapping_add(byte as u64);
    }
    hash
}

pub fn init_from_env() {
    if let Ok(seed_str) = std::env::var("FRACTURE_SEED") {
        if let Ok(seed_val) = seed_str.parse::<u64>() {
            CHAOS.seed.store(seed_val, Ordering::Relaxed);
            eprintln!("🎲 Fracture seed: {}", seed_val);
        }
        else {
            let seed = rand::random::<u64>();
            CHAOS.seed.store(seed, Ordering::Relaxed);
            eprintln!("🎲 Fracture seed: {} (random)", seed);
        }
        eprintln!("   Reproduce with: FRACTURE_SEED={}", CHAOS.seed.load(Ordering::Relaxed));
    }

    if let Ok(val) = std::env::var("FRACTURE_AUTO") {
        if val == "true" {
            inject(ChaosOperation::TcpConnect, 0.01);
            inject(ChaosOperation::TcpRead, 0.001);
            inject(ChaosOperation::TcpWrite, 0.001);
            eprintln!("⚡ Auto-chaos enabled");
            eprintln!("   Configure: FRACTURE_AUTO=false to disable");
        }
    }

    if let Ok(val) = std::env::var("FRACTURE_PARTITION") {
        let nodes: Vec<&str> = val.split(',').collect();
        if nodes.len() == 2 {
            partition(nodes[0], nodes[1]);
            eprintln!("🔌 Partition: {} ⟷ {}", nodes[0], nodes[1]);
        }
    }

    if let Ok(val) = std::env::var("FRACTURE_PACKET_LOSS") {
        if let Ok(rate) = val.parse::<f64>() {
            set_packet_loss("*", rate);
            eprintln!("📉 Packet loss: {}%", rate * 100.0);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_deterministic_failures() {
        set_seed(12345);
        inject(ChaosOperation::TcpAccept, 0.5);
        let results: Vec<bool> = (0..10).map(|_| should_fail(ChaosOperation::TcpAccept)).collect();

        set_seed(12345);
        let results2: Vec<bool> = (0..10).map(|_| should_fail(ChaosOperation::TcpAccept)).collect();

        assert_eq!(results, results2, "Should be deterministic with the same seed");
    }
}