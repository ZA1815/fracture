use fracture::prelude::*;
use fracture::chaos;
use rand::{SeedableRng, Rng};

async fn run_scenario() -> Vec<u32> {
    let (tx, mut rx) = mpsc::unbounded();

    // Create a deterministic RNG from the chaos seed
    let seed = chaos::get_seed();
    let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);

    // Spawn tasks that race against time and chaos
    for i in 0..10 {
        let tx = tx.clone();
        let delay = rng.r#gen::<u64>() % 100;
        spawn(async move {
            sleep(std::time::Duration::from_millis(delay)).await;
            tx.send(i).unwrap();
        });
    }
    
    // Inject some chaos
    chaos::inject(chaos::ChaosOperation::TaskScheduleDelay, 0.5);

    let mut results = Vec::new();
    // Collect 10 results
    for _ in 0..10 {
        if let Some(val) = rx.recv().await {
            results.push(val);
        }
    }
    results
}

#[test]
fn test_determinism() {
    // Run 1: Seed 12345
    let runtime1 = fracture::runtime::Runtime::new();
    let res1 = runtime1.block_on(async {
        chaos::set_seed(12345);
        run_scenario().await
    });

    // Run 2: Seed 12345 (Should match exactly)
    let runtime2 = fracture::runtime::Runtime::new();
    let res2 = runtime2.block_on(async {
        chaos::set_seed(12345);
        run_scenario().await
    });

    // Run 3: Seed 67890 (Should likely differ)
    let runtime3 = fracture::runtime::Runtime::new();
    let res3 = runtime3.block_on(async {
        chaos::set_seed(67890);
        run_scenario().await
    });

    assert_eq!(res1, res2, "Execution with same seed was not deterministic!");
    assert_ne!(res1, res3, "Execution with different seed should produce different ordering");
}