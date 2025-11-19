use std::collections::{HashMap, HashSet};
use std::time::Duration;

#[derive(Debug, Clone)]
pub struct NetworkTopology {
    nodes: HashSet<String>,
    reachability: HashMap<String, HashSet<String>>,
    latencies: HashMap<(String, String), Duration>,
    reliability: HashMap<(String, String), f64>
}

impl NetworkTopology {
    pub fn new() -> Self {
        Self {
            nodes: HashSet::new(),
            reachability: HashMap::new(),
            latencies: HashMap::new(),
            reliability: HashMap::new()
        }
    }

    pub fn add_node(&mut self, node: impl Into<String>) {
        let node = node.into();
        self.nodes.insert(node.clone());
        self.reachability.entry(node).or_insert_with(HashSet::new);
    }

    pub fn add_nodes(&mut self, nodes: impl IntoIterator<Item = impl Into<String>>) {
        for node in nodes {
            self.add_node(node);
        }
    }

    pub fn remove_node(&mut self, node: &str) {
        self.nodes.remove(node);
        self.reachability.remove(node);

        for reachable in self.reachability.values_mut() {
            reachable.remove(node);
        }

        self.latencies.retain(|(from, to), _| from != node && to != node);
        self.reliability.retain(|(from, to), _| from!= node && to != node);
    }

    pub fn connect(&mut self, node1: &str, node2: &str) {
        self.add_node(node1);
        self.add_node(node2);

        self.reachability.entry(node1.to_string()).or_insert_with(HashSet::new).insert(node2.to_string());
        self.reachability.entry(node2.to_string()).or_insert_with(HashSet::new).insert(node1.to_string());
    }

    pub fn connect_oneway(&mut self, from: &str, to: &str) {
        self.add_node(from);
        self.add_node(to);

        self.reachability.entry(from.to_string()).or_insert_with(HashSet::new).insert(to.to_string());
    }

    pub fn disconnect(&mut self, node1: &str, node2: &str) {
        if let Some(reachable) = self.reachability.get_mut(node1) {
            reachable.remove(node2);
        }
        if let Some(reachable) = self.reachability.get_mut(node2) {
            reachable.remove(node1);
        }

        self.latencies.remove(&(node1.to_string(), node2.to_string()));
        self.latencies.remove(&(node2.to_string(), node1.to_string()));

        self.reliability.remove(&(node1.to_string(), node2.to_string()));
        self.reliability.remove(&(node2.to_string(), node1.to_string()));
    }

    pub fn can_reach(&self, from: &str, to: &str) -> bool {
        self.reachability.get(from).map(|reachable| reachable.contains(to)).unwrap_or(false)
    }

    pub fn set_latency(&mut self, from: &str, to: &str, latency: Duration) {
        self.latencies.insert((from.to_string(), to.to_string()), latency);
    }

    pub fn get_latency(&self, from: &str, to: &str) -> Option<Duration> {
        self.latencies.get(&(from.to_string(), to.to_string())).copied()
    }

    pub fn set_reliability(&mut self, from: &str, to: &str, reliability: f64) {
        self.reliability.insert((from.to_string(), to.to_string()), reliability.clamp(0.0, 1.0));
    }

    pub fn get_reliability(&self, from: &str, to: &str) -> f64 {
        self.reliability.get(&(from.to_string(), to.to_string())).copied().unwrap_or(1.0)
    }

    pub fn nodes(&self) -> impl Iterator<Item = &String> {
        self.nodes.iter()
    }

    pub fn reachable_from(&self, node: &str) -> impl Iterator<Item = &String> {
        self.reachability.get(node).into_iter().flat_map(|set| set.iter())
    }

    pub fn mesh(nodes: impl IntoIterator<Item = impl Into<String>>) -> Self {
        let mut topology = Self::new();
        let node_list: Vec<String> = nodes.into_iter().map(Into::into).collect();

        if node_list.is_empty() {
            return topology;
        }

        for node in &node_list {
            topology.add_node(node.clone());
        }

        for i in 0..node_list.len() {
            for j in (i + 1)..node_list.len() {
                topology.connect(&node_list[i], &node_list[j]);
            }
        }

        topology
    }

    pub fn ring(nodes: impl IntoIterator<Item = impl Into<String>>) -> Self {
        let mut topology = Self::new();
        let node_list: Vec<String> = nodes.into_iter().map(Into::into).collect();

        if node_list.is_empty() {
            return topology;
        }

        for node in &node_list {
            topology.add_node(node.clone());
        }

        for i in 0..node_list.len() {
            let next = (i + 1) % node_list.len();
            topology.connect(&node_list[i], &node_list[next]);
        }

        topology
    }

    pub fn star(hub: impl Into<String>, nodes: impl IntoIterator<Item = impl Into<String>>) -> Self {
        let mut topology = Self::new();
        let hub = hub.into();
        topology.add_node(hub.clone());

        for node in nodes {
            let node = node.into();
            topology.add_node(node.clone());
            topology.connect(&hub, &node);
        }

        topology
    }

    pub fn partition_groups(&mut self, groups: &[Vec<String>]) {
        for reachable in self.reachability.values_mut() {
            reachable.clear();
        }

        for group in groups {
            for i in 0..group.len() {
                for j in 0..group.len() {
                    if i != j {
                        self.reachability.entry(group[i].clone()).or_insert_with(HashSet::new).insert(group[j].clone());
                    }
                }
            }
        }
    }

    pub fn split_brain(&mut self, group1: &[String], group2: &[String]) {
        self.partition_groups(&[group1.to_vec(), group2.to_vec()]);
    }

    pub fn heal(&mut self) {
        let nodes: Vec<String> = self.nodes.iter().cloned().collect();

        for i in 0..nodes.len() {
            for j in 0..nodes.len() {
                if i != j {
                    self.reachability.entry(nodes[i].clone()).or_insert_with(HashSet::new).insert(nodes[j].clone());
                }
            }
        }
    }

    pub fn is_partitioned(&self) -> bool {
        if self.nodes.len() < 2 {
            return false;
        }

        let start = match self.nodes.iter().next() {
            Some(node) => node,
            None => return false
        };

        let mut visited = HashSet::new();
        let mut queue = vec![start.clone()];
        visited.insert(start.clone());

        while let Some(node) = queue.pop() {
            if let Some(reachable) = self.reachability.get(&node) {
                for neighbor in reachable {
                    if visited.insert(neighbor.clone()) {
                        queue.push(neighbor.clone());
                    }
                }
            }
        }

        visited.len() < self.nodes.len()
    }
}

pub trait Topology {
    fn add_node(&mut self, node: impl Into<String>);
    fn connect(&mut self, node1: &str, node2: &str);
    fn disconnect(&mut self, node1: &str, node2: &str);
    fn can_reach(&self, from: &str, to: &str) -> bool;
}

impl Topology for NetworkTopology {
    fn add_node(&mut self, node: impl Into<String>) {
        NetworkTopology::add_node(self, node);
    }

    fn connect(&mut self, node1: &str, node2: &str) {
        NetworkTopology::connect(self, node1, node2);
    }

    fn disconnect(&mut self, node1: &str, node2: &str) {
        NetworkTopology::disconnect(self, node1, node2);
    }

    fn can_reach(&self, from: &str, to: &str) -> bool {
        NetworkTopology::can_reach(self, from, to)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mesh_topology() {
        let topology = NetworkTopology::mesh(vec!["node1", "node2", "node3"]);

        assert!(topology.can_reach("node1", "node2"));
        assert!(topology.can_reach("node2", "node3"));
        assert!(topology.can_reach("node3", "node1"));
    }

    #[test]
    fn test_ring_topology() {
        let topology = NetworkTopology::ring(vec!["node1", "node2", "node3"]);
        
        assert!(topology.can_reach("node1", "node2"));
        assert!(topology.can_reach("node2", "node3"));
        assert!(topology.can_reach("node3", "node1"));
    }

    #[test]
    fn test_partition_detection() {
        let mut topology = NetworkTopology::mesh(vec!["node1", "node2", "node3", "node4"]);
        assert!(!topology.is_partitioned());
        
        topology.split_brain(&["node1".to_string(), "node2".to_string()], &["node3".to_string(), "node4".to_string()]);
        
        assert!(topology.is_partitioned());
        assert!(topology.can_reach("node1", "node2"));
        assert!(topology.can_reach("node3", "node4"));
        assert!(!topology.can_reach("node1", "node3"));
    }
}