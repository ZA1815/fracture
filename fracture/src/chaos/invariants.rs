use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use dashmap::DashMap;
use std::fmt;

use crate::chaos::trace::{TraceEvent, record};

pub struct Invariant {
    name: String,
    check: Arc<dyn Fn() -> bool + Send + Sync>,
    violated: AtomicBool,
    violation_count: AtomicU64
}

impl Invariant {
    pub fn new(name: impl Into<String>, check: impl Fn() -> bool + Send + Sync + 'static) -> Self {
        Self {
            name: name.into(),
            check: Arc::new(check),
            violated: AtomicBool::new(false),
            violation_count: AtomicU64::new(0)
        }
    }

    pub fn check(&self) -> bool {
        let holds = (self.check)();
        if !holds {
            self.violated.store(true, Ordering::Relaxed);
            self.violation_count.fetch_add(1, Ordering::Relaxed);
        }

        record(TraceEvent::InvariantChecked { name: self.name.clone(), holds });

        holds
    }

    pub fn is_violated(&self) -> bool {
        self.violated.load(Ordering::Relaxed)
    }

    pub fn violation_count(&self) -> u64 {
        self.violation_count.load(Ordering::Relaxed)
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

impl fmt::Debug for Invariant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Invariant")
            .field("name", &self.name)
            .field("violated", &self.violated)
            .field("violation_count", &self.violation_count)
            .finish()
    }
}

pub struct InvariantChecker {
    invariants: DashMap<String, Arc<Invariant>>,
    enabled: AtomicBool
}

impl InvariantChecker {
    pub fn new() -> Self {
        Self {
            invariants: DashMap::new(),
            enabled: AtomicBool::new(true)
        }
    }

    pub fn register(&self, invariant: Invariant) {
        let name = invariant.name.clone();
        self.invariants.insert(name, Arc::new(invariant));
    }

    pub fn register_fn(&self, name: impl Into<String>, check: impl Fn() -> bool + Send + Sync + 'static) {
        self.register(Invariant::new(name, check));
    }

    pub fn check_all(&self) -> bool {
        if !self.enabled.load(Ordering::Relaxed) {
            return true;
        }

        let mut all_hold = true;
        for entry in self.invariants.iter() {
            if !entry.value().check() {
                all_hold = false;
                eprintln!("❌ Invariant violated: {}", entry.key());
            }
        }

        all_hold
    }

    pub fn check_one(&self, name: &str) -> Option<bool> {
        self.invariants.get(name).map(|inv| inv.check())
    }

    pub fn get_violations(&self) -> Vec<String> {
        self.invariants
            .iter()
            .filter(|entry| entry.value().is_violated())
            .map(|entry| entry.key().clone())
            .collect()
    }

    pub fn has_violations(&self) -> bool {
        self.invariants.iter().any(|entry| entry.value().is_violated())
    }

    pub fn clear(&self) {
        self.invariants.clear();
    }

    pub fn reset(&self) {
        for entry in self.invariants.iter() {
            entry.value().violated.store(false, Ordering::Relaxed);
            entry.value().violation_count.store(0, Ordering::Relaxed);
        }
    }

    pub fn set_enabled(&self, enabled: bool) {
        self.enabled.store(enabled, Ordering::Relaxed);
    }

    pub fn summary(&self) -> Vec<(String, bool, u64)> {
        self.invariants.iter().map(|entry| {
            let inv = entry.value();
            (entry.key().clone(), inv.is_violated(), inv.violation_count())
        })
        .collect()
    }
}

impl Default for InvariantChecker {
    fn default() -> Self {
        Self::new()
    }
}

pub struct CommonInvariants;

impl CommonInvariants {
    pub fn no_data_loss<F>(check_fn: F) -> Invariant
    where F: Fn() -> bool + Send + Sync + 'static {
        Invariant::new("no_data_loss", check_fn)
    }

    pub fn monotonic_counter(get_counter: Arc<dyn Fn() -> u64 + Send + Sync>) -> Invariant {
        let last_value = Arc::new(AtomicU64::new(0));
        Invariant::new("monotonic_counter", move || {
            let current = get_counter();
            let last = last_value.load(Ordering::Relaxed);
            if current >= last {
                last_value.store(current, Ordering::Relaxed);
                true
            }
            else {
                eprintln!("Counter went backwards: {} -> {}", last, current);
                false
            }
        })
    }

    pub fn eventual_consistency<F>(check_fn: F) -> Invariant
    where F: Fn() -> bool + Send + Sync + 'static {
        Invariant::new("eventual_consistency", check_fn)
    }

    pub fn mutual_exclusion<F>(get_lock_holders: F) -> Invariant 
    where F: Fn() -> Vec<String> + Send + Sync + 'static {
        Invariant::new("mutual_exclusion", move || {
            let holders = get_lock_holders();
            if holders.len() > 1 {
                eprintln!("Multiple lock holders: {:?}", holders);
                false
            }
            else {
                true
            }
        })
    }

    pub fn no_split_brain<F>(get_leaders: F) -> Invariant
    where F: Fn() -> Vec<String> + Send + Sync + 'static {
        Invariant::new("no_split_brain", move || {
            let leaders = get_leaders();
            if leaders.len() > 1 {
                eprintln!("Split brain detected. Multiple leaders: {:?}", leaders);
                false
            }
            else {
                true
            }
        })
    }

    pub fn bounded_divergence<F>(get_states: F, max_diff: u64) -> Invariant
    where F: Fn() -> Vec<u64> + Send + Sync + 'static {
        Invariant::new("bounded_divergence", move || {
            let states = get_states();
            if states.is_empty() {
                return true;
            }

            let min = *states.iter().min().unwrap();
            let max = *states.iter().max().unwrap();
            let diff = max - min;

            if diff > max_diff {
                eprintln!("States diverged too much: min={}, max={}, diff={}", min, max, diff);
                false
            }
            else {
                true
            }
        })
    }

    pub fn idempotent<F>(operation: F) -> Invariant
    where F: Fn() -> String + Send + Sync + 'static {
        let last_result = Arc::new(parking_lot::RwLock::new(None::<String>));
        Invariant::new("idempotent", move || {
            let result = operation();
            let mut last = last_result.write();

            if let Some(prev) = &*last {
                if prev != &result {
                    eprintln!("Operation not idempotent: {} != {}", prev, result);
                    return false;
                }
            }

            *last = Some(result);
            true
        })
    }

    pub fn total_order<F>(get_sequence: F) -> Invariant
    where F: Fn() -> Vec<u64> + Send + Sync + 'static {
        Invariant::new("total_order", move || {
            let sequence = get_sequence();
            for i in 1..sequence.len() {
                if sequence[i] <= sequence[i - 1] {
                    eprintln!("Order violation: {} -> {}", sequence[i - 1], sequence[i]);
                    return false;
                }
            }
            true
        })
    }
}

static INVARIANTS: std::sync::LazyLock<InvariantChecker> = std::sync::LazyLock::new(InvariantChecker::new);

pub fn register(invariant: Invariant) {
    INVARIANTS.register(invariant);
}

pub fn register_fn(name: impl Into<String>, check: impl Fn() -> bool + Send + Sync + 'static) {
    INVARIANTS.register_fn(name, check);
}

pub fn check_all() -> bool {
    INVARIANTS.check_all()
}

pub fn get_violations() -> Vec<String> {
    INVARIANTS.get_violations()
}

pub fn has_violations() -> bool {
    INVARIANTS.has_violations()
}

pub fn clear() {
    INVARIANTS.clear();
}

pub fn reset() {
    INVARIANTS.reset();
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicU64;

    #[test]
    fn test_invariant_basic() {
        let counter = AtomicU64::new(0);
        let inv = Invariant::new("always_true", move || {
            counter.fetch_add(1, Ordering::Relaxed);
            true
        });

        assert!(inv.check());
        assert!(!inv.is_violated());
    }

    #[test]
    fn test_invariant_violation() {
        let inv = Invariant::new("always_false", || false);
        assert!(!inv.check());
        assert!(inv.is_violated());
        assert_eq!(inv.violation_count(), 1);
    }

    #[test]
    fn test_monotonic_counter() {
        let counter = Arc::new(AtomicU64::new(0));
        let counter_clone = counter.clone();
        let inv = CommonInvariants::monotonic_counter(Arc::new(move || {
            counter_clone.load(Ordering::Relaxed)
        }));

        counter.store(5, Ordering::Relaxed);
        assert!(inv.check());

        counter.store(10, Ordering::Relaxed);
        assert!(inv.check());

        counter.store(3, Ordering::Relaxed);
        assert!(!inv.check());
    }

    #[test]
    fn test_invariant_checker() {
        let checker = InvariantChecker::new();

        checker.register_fn("always_true", || true);
        checker.register_fn("always_false", || false);

        assert!(!checker.check_all());
        assert!(checker.has_violations());

        let violations = checker.get_violations();
        assert_eq!(violations, vec!["always_false"]);
    }
}