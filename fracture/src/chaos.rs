use std::sync::atomic::{AtomicU64, AtomicBool, Ordering};
use std::sync::LazyLock;
use dashmap::DashMap;

pub struct ChaosState {
    pub enabled: AtomicBool,
    pub seed: AtomicU64,
    pub failure_rates: DashMap<String, f64>,
    pub partitions: DashMap<(String, String), bool>
}

static CHAOS: LazyLock<ChaosState> = LazyLock::new(|| ChaosState {
    enabled: AtomicBool::new(false),
    seed: AtomicU64::new(0),
    failure_rates: DashMap::new(),
    partitions: DashMap::new()
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

        CHAOS.failure_rates.get(operation).map(|rate| {
            let seed = CHAOS.seed.load(Ordering::Relaxed);
            let hash = hash_combine(seed, operation);
            (hash as f64 / u64::MAX as f64) < *rate
        })
        .unwrap_or(false)
    }
}

pub fn inject(operation: &str, failure_rate: f64) {
    #[cfg(feature = "simulation")]
    {
        CHAOS.failure_rates.insert(operation.to_string(), failure_rate);
        CHAOS.enabled.store(true, Ordering::Relaxed);
    }
}

pub fn partition(from: &str, to: &str) {
    #[cfg(feature = "simulation")]
    {
        CHAOS.partitions.insert((from.to_string(), to.to_string()), true);
    }
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

fn hash_combine(seed: u64, s: &str) -> u64 {
    let mut hash = seed;
    for byte in s.bytes() {
        hash = hash.wrapping_mul(31).wrapping_add(byte as u64);
    }
    hash
}

pub fn init_from_env() {
    #[cfg(feature = "simulation")]
    {
        if let Ok(seed) = std::env::var("FRACTURE_SEED") {
            if let Ok(seed_val) = seed.parse::<u64>() {
                CHAOS.seed.store(seed_val, Ordering::Relaxed);
                eprintln!("🎲 Fracture seed: {}", seed_val);
                eprintln!("   Reproduce with: FRACTURE_SEED={}", seed_val);
            }
            else {
                let seed = rand::random::<u64>();
                CHAOS.seed.store(seed, Ordering::Relaxed);
                eprintln!("🎲 Fracture seed: {}", seed);
                eprintln!("   Reproduce with: FRACTURE_SEED={}", seed);
            }

            if let Ok(val) = std::env::var("FRACTURE_AUTO") {
                if val == "true" {
                    inject("tcp_connect", 0.01);
                    inject("tcp_read", 0.001);
                    inject("tcp_write", 0.001);
                    eprintln!("⚡ Auto-chaos enabled (disable with FRACTURE_AUTO=false in your .env)");
                }
            }
        }
    }
}