#[cfg(feature = "simulation")]
use turmoil::{Builder, Sim};
use std::future::Future;
use std::time::Duration;

#[cfg(feature = "simulation")]
pub struct FractureTest {
    seed: Option<u64>,
    duration: Duration,
    auto_chaos: bool,
    nodes: Vec<String>
}

#[cfg(feature = "simulation")]
impl FractureTest {
    pub fn new() -> Self {
        crate::chaos::init_from_env();

        let auto_chaos;
        if let Ok(val) = std::env::var("FRACTURE_AUTO") {
            if val == "true" {
                auto_chaos = true
            }
            else {
                auto_chaos = false
            }
        }
        else {
            auto_chaos = false
        };

        Self {
            seed: None,
            duration: Duration::from_secs(60),
            auto_chaos,
            nodes: vec![]
        }
    }

    pub fn quick<F, Fut>(test_fn: F)
    where F: FnOnce() -> Fut + 'static, Fut: Future<Output = ()> + 'static
    {
        Self::new().run(test_fn)
    }

    pub fn seed(mut self, seed: u64) -> Self {
        self.seed = Some(seed);
        self
    }

    pub fn duration(mut self, duration: Duration) -> Self {
        self.duration = duration;
        self
    }

    pub fn nodes(mut self, nodes: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.nodes = nodes.into_iter().map(Into::into).collect();
        self
    }

    pub fn run<F, Fut>(self, test_fn: F)
    where F: FnOnce() -> Fut + 'static, Fut: Future<Output = ()> + 'static
    {
        let mut sim = Builder::new().simulation_duration(self.duration).build();

        if !self.nodes.is_empty() {
            for node in &self.nodes {
                sim.host(**&node, || async {
                    // Node will run test function
                });
            }
        }

        sim.run().unwrap();
    }
}