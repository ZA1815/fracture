use std::sync::atomic::{AtomicU64, AtomicBool, Ordering};
use std::sync::LazyLock;
use std::time::Duration;
use dashmap::DashMap;
use rand::{SeedableRng, Rng};
use rand_chacha::ChaCha8Rng;

pub mod topology;
pub mod scenario;
pub mod invariants;

pub use topology::{Topology, NetworkTopology};
pub use scenario::{Scenario, ScenarioBuilder};
pub use invariants::{Invariant, InvariantChecker};

#[cfg(feature = "simulation")]
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

#[cfg(feature = "simulation")]
#[derive(Debug, Clone)]
pub struct FailureConfig {
    pub rate: f64,
    pub duration: Option<Duration>
}

#[cfg(feature = "simulation")]
#[derive(Debug, Clone)]
pub struct DelayConfig {
    pub min: Duration,
    pub max: Duration
}

#[cfg(feature = "simulation")]
pub struct ChaosState {
    pub enabled: AtomicBool,
    pub seed: AtomicU64,
    pub failure_rates: DashMap<String, FailureConfig>,
    pub partitions: DashMap<(String, String), bool>,
    pub packet_loss: DashMap<String, f64>,
    pub delays: DashMap<String, DelayConfig>,
    pub reordering: DashMap<String, f64>,
    pub topology: parking_lot::RwLock<topology::NetworkTopology>
}

#[cfg(feature = "simulation")]
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
pub fn should_fail(operation: &str) -> bool {
    #[cfg(not(feature = "simulation"))]
    {
        false
    }

    #[cfg(feature = "simulation")]
    {
        if !CHAOS.enabled.load(Ordering::Relaxed) {
            return false;
        }

        CHAOS.failure_rates.get(operation).map(|config| {
            let seed = CHAOS.seed.load(Ordering::Relaxed);
            let hash = hash_operation(seed, operation);
            let roll = (hash as f64 / u64::MAX as f64);
            roll < config.rate
        })
        .unwrap_or(false)
    }
}

pub fn inject(operation: &str, failure_rate: f64) {
    #[cfg(feature = "simulation")]
    {
        CHAOS.failure_rates.insert(operation.to_string(), FailureConfig { rate: failure_rate, duration: None });
        CHAOS.enabled.store(true, Ordering::Relaxed);
    }
}

pub fn inject_temporary(operation: &str, failure_rate: f64, duration: Duration) {
    #[cfg(feature = "simulation")]
    {
        CHAOS.failure_rates.insert(operation.to_string(), FailureConfig { rate: failure_rate, duration: Some(duration) });
        CHAOS.enabled.store(true, Ordering::Relaxed);
    }
}

#[cfg(feature = "simulation")]
pub fn partition(from: &str, to: &str) {
    CHAOS.partitions.insert((from.to_string(), to.to_string()), true);
    CHAOS.partitions.insert((to.to_string(), from.to_string()), true);
    CHAOS.enabled.store(true, Ordering::Relaxed);
}

#[cfg(feature = "simulation")]
pub fn partition_oneway(from: &str, to: &str) {
    CHAOS.partitions.insert((from.to_string(), to.to_string()), true);
    CHAOS.enabled.store(true, Ordering::Relaxed);
}

#[cfg(feature = "simulation")]
pub fn heal_partition(from: &str, to: &str) {
    CHAOS.partitions.remove(&(from.to_string(), to.to_string()));
    CHAOS.partitions.remove(&(to.to_string(), from.to_string()));
}

#[inline]
pub fn is_partitioned(from: &str, to: &str) -> bool {
    #[cfg(not(feature = "simulation"))]
    {
        false
    }

    #[cfg(feature = "simulation")]
    {
        CHAOS.partitions.get(&(from.to_string(), to.to_string())).map(|v| *v).unwrap_or(false)
    }
}

#[cfg(feature = "simulation")]
pub fn set_packet_loss(node: &str, rate: f64) {
    CHAOS.packet_loss.insert(node.to_string(), rate.clamp(0.0, 1.0));
    CHAOS.enabled.store(true, Ordering::Relaxed);
}

#[inline]
pub fn should_drop_packet(node: &str) -> bool {
    #[cfg(not(feature = "simulation"))]
    {
        false
    }

    #[cfg(feature = "simulation")]
    {
        CHAOS.packet_loss.get(node).map(|rate| {
            let seed = CHAOS.seed.load(Ordering::Relaxed);
            let hash = hash_operation(seed, node);
            (hash as f64 / u64::MAX as f64) < *rate
        })
        .unwrap_or(false)
    }
}

#[cfg(feature = "simulation")]
pub fn set_delay(node: &str, min: Duration, max: Duration) {
    CHAOS.delays.insert(node.to_string(), DelayConfig { min, max });
    CHAOS.enabled.store(true, Ordering::Relaxed);
}

pub fn get_delay(node: &str) -> Option<Duration> {
    #[cfg(not(feature = "simulation"))]
    {
        None
    }

    #[cfg(feature = "simulation")]
    {
        CHAOS.delays.get(node).map(|config| {
            let seed = CHAOS.seed.load(Ordering::Relaxed);
            let mut rng = ChaCha8Rng::seed_from_u64(seed);
            let min_ms = config.min.as_millis() as u64;
            let max_ms = config.max.as_millis() as u64;
            let delay_ms = rng.gen_range(min_ms..=max_ms);
            Duration::from_millis(delay_ms)
        })
    }
}

#[cfg(feature = "simulation")]
pub fn set_reordering(node: &str, rate: f64) {
    CHAOS.reordering.insert(node.to_string(), rate.clamp(0.0, 1.0));
    CHAOS.enabled.store(true, Ordering::Relaxed);
}

#[inline]
pub fn should_reorder(node: &str) -> bool {
    #[cfg(not(feature = "simulation"))]
    {
        false
    }

    #[cfg(feature = "simulation")]
    {
        CHAOS.reordering.get(node).map(|rate| {
            let seed = CHAOS.seed.load(Ordering::Relaxed);
            let hash = hash_operation(seed, node);
            (hash as f64 / u64::MAX as f64) < *rate
        })
        .unwrap_or(false)
    }
}

#[cfg(feature = "simulation")]
pub fn clear() {
    CHAOS.failure_rates.clear();
    CHAOS.partitions.clear();
    CHAOS.packet_loss.clear();
    CHAOS.delays.clear();
    CHAOS.reordering.clear();
    CHAOS.enabled.store(false, Ordering::Relaxed);
}

#[cfg(feature = "simulation")]
pub fn get_seed() -> u64 {
    CHAOS.seed.load(Ordering::Relaxed)
}

#[cfg(feature = "simulation")]
pub fn set_seed(seed: u64) {
    CHAOS.seed.store(seed, Ordering::Relaxed);
}

#[cfg(feature = "simulation")]
fn hash_operation(seed: u64, s: &str) -> u64 {
    let mut hash = seed;
    for byte in s.bytes() {
        hash = hash.wrapping_mul(31).wrapping_add(byte as u64);
    }
    hash
}

#[cfg(feature = "simulation")]
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
            inject("tcp_connect", 0.01);
            inject("tcp_read", 0.001);
            inject("tcp_write", 0.001);
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
        inject("test_op", 0.5);
        let results: Vec<bool> = (0..10).map(|_| should_fail("test_op")).collect();

        set_seed(12345);
        let results2: Vec<bool> = (0..10).map(|_| should_fail("test_op")).collect();

        assert_eq!(results, results2, "Should be deterministic with the same seed");
    }
}