use std::time::Duration;
use std::future::Future;
use crate::chaos::{ChaosOperation, heal_partition, inject, partition, partition_oneway, set_delay, set_packet_loss, set_reordering};
use crate::chaos::trace::{record, TraceEvent};

#[derive(Debug, Clone)]
pub struct Scenario {
    seed: Option<u64>,
    operations: Vec<ScenarioOperation>
}

#[derive(Debug, Clone)]
enum ScenarioOperation {
    Partition { from: String, to: String, oneway: bool },
    HealPartition { from: String, to: String },
    FailureRate { operation: ChaosOperation, rate: f64 },
    PacketLoss { node: String, rate: f64 },
    Delay { node: String, min: Duration, max: Duration },
    Reordering { node: String, rate: f64 },
    Wait { duration: Duration }
}

impl Scenario {
    pub fn new() -> Self {
        Self {
            seed: None,
            operations: Vec::new()
        }
    }

    pub fn seed(mut self, seed: u64) -> Self {
        self.seed = Some(seed);
        self
    }

    pub fn partition(mut self, from: impl Into<String>, to: impl Into<String>) -> Self {
        self.operations.push(ScenarioOperation::Partition { from: from.into(), to: to.into(), oneway: false });
        self
    }

    pub fn partition_oneway(mut self, from: impl Into<String>, to: impl Into<String>) -> Self {
        self.operations.push(ScenarioOperation::Partition { from: from.into(), to: to.into(), oneway: true });
        self
    }

    pub fn heal_partition(mut self, from: impl Into<String>, to: impl Into<String>) -> Self {
        self.operations.push(ScenarioOperation::HealPartition { from: from.into(), to: to.into() });
        self
    }

    pub fn fail(mut self, operation: ChaosOperation, rate: f64) -> Self {
        self.operations.push(ScenarioOperation::FailureRate { operation, rate });
        self
    }

    pub fn fail_connections(self, rate: f64) -> Self {
        self.fail(ChaosOperation::TcpConnect, rate)
    }

    pub fn fail_reads(self, rate: f64) -> Self {
        self.fail(ChaosOperation::TcpRead, rate)
    }

    pub fn fail_writes(self, rate: f64) -> Self {
        self.fail(ChaosOperation::TcpWrite, rate)
    }

    pub fn packet_loss(mut self, node: impl Into<String>, rate: f64) -> Self {
        self.operations.push(ScenarioOperation::PacketLoss { node: node.into(), rate });
        self
    }

    pub fn delay(mut self, node: impl Into<String>, min: Duration, max: Duration) -> Self {
        self.operations.push(ScenarioOperation::Delay { node: node.into(), min, max });
        self
    }

    pub fn delay_fixed(self, node: impl Into<String>, delay: Duration) -> Self {
        self.delay(node.into(), delay, delay)
    }

    pub fn reordering(mut self, node: impl Into<String>, rate: f64) -> Self {
        self.operations.push(ScenarioOperation::Reordering { node: node.into(), rate });
        self
    }

    pub fn wait(mut self, duration: Duration) -> Self {
        self.operations.push(ScenarioOperation::Wait { duration });
        self
    }

    pub fn build(self) {
        if let Some(seed) = self.seed {
            crate::chaos::set_seed(seed);
        }

        for operation in &self.operations {
            record(TraceEvent::ScenarioOp(format!("{:?}", operation)));
        }

        for operation in self.operations {
            match operation {
                ScenarioOperation::Partition { from, to, oneway } => {
                    if oneway {
                        partition_oneway(&from, &to);
                    }
                    else {
                        partition(&from, &to);
                    }
                }
                ScenarioOperation::HealPartition { from, to } => {
                    heal_partition(&from, &to);
                }
                ScenarioOperation::FailureRate { operation, rate } => {
                    inject(operation, rate);
                }
                ScenarioOperation::PacketLoss { node, rate } => {
                    set_packet_loss(&node, rate);
                }
                ScenarioOperation::Delay { node, min, max } => {
                    set_delay(&node, min, max);
                }
                ScenarioOperation::Reordering { node, rate } => {
                    set_reordering(&node, rate);
                }
                ScenarioOperation::Wait { .. } => {
                    // Placeholder
                }
            }
        }
    }

    pub async fn run<F, Fut>(self, test: F)
    where F: FnOnce() -> Fut, Fut: Future<Output = ()> {
        self.build();
        test().await;
    }
}

impl Default for Scenario {
    fn default() -> Self {
        Self::new()
    }
}

pub struct ScenarioBuilder;

impl ScenarioBuilder {
    pub fn split_brain(group1: Vec<impl Into<String>>, group2: Vec<impl Into<String>>) -> Scenario {
        let mut scenario = Scenario::new();

        let g1: Vec<String> = group1.into_iter().map(Into::into).collect();
        let g2: Vec<String> = group2.into_iter().map(Into::into).collect();

        for node1 in &g1 {
            for node2 in &g2 {
                scenario = scenario.partition(node1.clone(), node2.clone());
            }
        }

        scenario
    }

    pub fn flaky_network(packet_loss: f64, delay: Duration) -> Scenario {
        Scenario::new().packet_loss("*", packet_loss).delay_fixed("*", delay).fail_connections(0.05)
    }

    pub fn slow_network(min_delay: Duration, max_delay: Duration) -> Scenario {
        Scenario::new().delay("*", min_delay, max_delay)
    }

    pub fn lossy_network(loss_rate: f64) -> Scenario {
        Scenario::new().packet_loss("*", loss_rate).fail_reads(loss_rate * 0.5).fail_writes(loss_rate * 0.5)
    }

    pub fn cascading_failure(nodes: Vec<impl Into<String>>) -> Scenario {
        let mut scenario = Scenario::new();

        for (i, _) in nodes.into_iter().enumerate() {
            let delay = Duration::from_millis(100 * (i as u64 + 1));
            scenario = scenario.wait(delay).fail(ChaosOperation::TcpConnect, 1.0); // Add node specific variant
        }

        scenario
    }

    pub fn byzantine(malicious_nodes: Vec<impl Into<String>>) -> Scenario {
        let mut scenario = Scenario::new();

        for node in malicious_nodes {
            let node = node.into();
            scenario = scenario
                .packet_loss(node.clone(), 0.3)
                .reordering(node.clone(), 0.5)
                .delay(node, Duration::from_millis(10), Duration::from_millis(500));
        }

        scenario
    }

    pub fn temporary_partition(from: impl Into<String>, to: impl Into<String>, duration: Duration) -> Scenario {
        let from = from.into();
        let to = to.into();

        Scenario::new().partition(from.clone(), to.clone()).wait(duration).heal_partition(from, to)
    }

    pub fn rolling_restart(nodes: Vec<impl Into<String>>, restart_duration: Duration) -> Scenario {
        let mut scenario = Scenario::new();

        for _ in nodes {
            scenario = scenario.fail(ChaosOperation::TcpConnect, 1.0).wait(restart_duration).fail(ChaosOperation::TcpConnect, 0.0);
        }

        scenario
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scenario_builder() {
        let scenario = Scenario::new()
            .seed(12345)
            .partition("node1", "node2")
            .packet_loss("node3", 0.1)
            .delay("node4", Duration::from_millis(50), Duration::from_millis(50));

        assert_eq!(scenario.operations.len(), 3);
        assert!(scenario.seed.is_some());
    }

    #[test]
    fn test_split_brain() {
        let scenario = ScenarioBuilder::split_brain(vec!["node1", "node2"], vec!["node3", "node4"]);

        assert_eq!(scenario.operations.len(), 4);
    }

    #[test]
    fn test_flaky_network() {
        let scenario = ScenarioBuilder::flaky_network(0.05, Duration::from_millis(100));
        assert_eq!(scenario.operations.len(), 3);
    }
}