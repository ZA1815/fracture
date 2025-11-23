# Fracture

> ⚠️ **PROJECT IS IN ALPHA** - Fracture is in early development (v0.1.1). The core concepts work, but there are likely edge cases and bugs we haven't found yet. **Please report any issues you encounter!** The irony is not lost on us that a chaos testing tool needs help finding its own bugs. 🙃

**Deterministic chaos testing for async Rust. Drop-in for Tokio.**

Fracture is a testing framework that helps you find bugs in async code by simulating failures, network issues, and race conditions—all deterministically and reproducibly. Note that Fracture is only a drop-in replacement for Tokio and does not work with any other async runtime.

## The Problem

Most async Rust code looks fine in tests but breaks in production:

```rust
async fn handle_request(db: &Database, api: &ExternalApi) -> Result<Response> {
    let user = db.get_user(user_id).await?;  // What if the DB times out?
    let data = api.fetch_data().await?;       // What if the API returns 500?
    Ok(process(user, data))
}
```

**Your tests pass because they assume the happy path.** Production doesn't.

## The Solution

Fracture runs your async code in a **simulated environment** with **deterministic chaos injection**:

```rust
#[fracture::test]
async fn test_with_chaos() {
    // Inject 30% network failure rate
    chaos::inject(ChaosOperation::TcpWrite, 0.3);
    
    // Your code runs with failures injected
    let result = handle_request(&db, &api).await;
    
    // Did your retry logic work? Did you handle timeouts?
    assert!(result.is_ok());
}
```

**Same seed = same failures = reproducible bugs.**

## Features

- ✅ **Deterministic** - Control randomness with seeds, reproduce bugs every time
- ✅ **Fast** - Pure in-memory simulation, no real network/filesystem
- ✅ **Chaos Injection** - Network failures, delays, partitions, timeouts, task aborts
- ✅ **Drop-in Testing** - Works like `#[tokio::test]` but with superpowers
- ✅ **Async Primitives** - Tasks, channels, timers, TCP, select!, timeouts
- ✅ **Scenario Builder** - Script complex failure sequences (partitions, delays, healing)

## Installation

⚠️ **Alpha warning:** API may change between minor versions until 1.0.0

Add to your `Cargo.toml`:

```toml
[dev-dependencies]
fracture = "0.1"
```

## Quick Start

### Basic Test

```rust
use fracture::prelude::*;

#[fracture::test]
async fn my_async_test() {
    let (tx, mut rx) = mpsc::unbounded();
    
    spawn(async move {
        tx.send("hello").unwrap();
    });
    
    assert_eq!(rx.recv().await, Some("hello"));
}
```


### Running Applications with `#[fracture::main]`

Use `#[fracture::main]` as a drop-in replacement for `#[tokio::main]`:

```rust
use fracture::prelude::*;

#[fracture::main]
async fn main() {
    println!("Starting server...");

    let listener = TcpListener::bind("127.0.0.1:8080").await.unwrap();

    loop {
        let (socket, _) = listener.accept().await.unwrap();
        spawn(async move {
            handle_connection(socket).await;
        });
    }
}
```

**How it works:**
- **With `simulation` feature enabled**: Runs using Fracture's deterministic runtime (for development/testing with chaos injection)
- **Without `simulation` feature**: Falls back to the real Tokio runtime (for production)

This lets you develop and test with Fracture's chaos testing capabilities, then deploy to production with zero code changes by simply disabling the `simulation` feature.

**Example Cargo.toml setup:**

```toml
[dependencies]
# Production: Uses real Tokio runtime
fracture = "0.1"

[dev-dependencies]
# Testing: Uses simulation runtime with chaos
fracture = { version = "0.1", features = ["simulation"] }
```

With this setup, `cargo run` uses the real Tokio runtime, while `cargo test` uses Fracture's simulation.

### With Chaos Injection

```rust
use fracture::prelude::*;
use fracture::chaos;

#[fracture::test]
async fn test_resilient_sender() {
    // 50% chance messages fail to send
    chaos::inject(ChaosOperation::MpscSend, 0.5);
    
    let (tx, mut rx) = mpsc::unbounded();
    
    // Your code needs retry logic to pass this test!
    let sender = spawn(async move {
        for i in 0..10 {
            let mut attempts = 0;
            loop {
                match tx.send(i) {
                    Ok(_) => break,
                    Err(_) => {
                        attempts += 1;
                        if attempts > 5 {
                            panic!("Failed after 5 retries");
                        }
                        sleep(Duration::from_millis(10)).await;
                    }
                }
            }
        }
    });
    
    sender.await.unwrap();
}
```

### Network Partition Scenarios

```rust
use fracture::chaos::{ChaosOperation, Scenario};

#[fracture::test(duration = "5s")]
async fn test_network_partition() {
    let scenario = Scenario::new()
        .seed(42)  // Deterministic chaos
        .partition("192.168.1.1:8000", "192.168.1.2:8000")
        .wait(Duration::from_secs(2))
        .heal_partition("192.168.1.1:8000", "192.168.1.2:8000");
    
    spawn(scenario.execute_chaos());
    
    // Your distributed system code runs here
    // Does it handle partitions correctly?
}
```

### Reproducing Bugs

When a test fails, Fracture shows you the seed:

```
Run with FRACTURE_SEED=17135321411058301739 to reproduce.
```

Set the environment variable to get the exact same failure:

```bash
FRACTURE_SEED=17135321411058301739 cargo test
```

## Testing External Libraries (reqwest, sqlx, aws-sdk)
By default, Fracture simulates your logic. External libraries that depend on the real tokio runtime (like database drivers or HTTP clients) will continue to use the real network and OS threads, ignoring your chaos settings.

To simulate chaos in external libraries, you must "patch" Tokio.

We provide a Shim Crate strategy that tricks the entire dependency tree into using Fracture instead of Tokio.

1. The Setup

In your Cargo.toml, add a patch directive to redirect tokio to the shim included in this repository:
```
[patch.crates-io]

⚠️ This forces every library in your tree to use Fracture as its runtime
tokio = { git = "https://github.com/ZA1815/fracture", path = "tokio-shim" }
```

Alternatively, you can create a .cargo/config.toml file with the same content, this will apply the patch globally to your project without modifying Cargo.toml.

**Make sure that for both of these, you delete the patch section before releasing to production.**
```

2. The Rules

When patching is active:

Do NOT enable the tokio feature in fracture. Your Cargo.toml dependencies should look like this:

```
[dev-dependencies]
# Only enable simulation features, do not depend on the real tokio
fracture = { version = "0.1", features = ["simulation"] } 
Run tests normally: cargo test
```

Revert for production: Remove the [patch] section when building your actual application release.

Why do this?
Time Travel: fracture::time::sleep(Duration::from_secs(3600)) will instantly advance time for reqwest timeouts.

Network Chaos: You can inject packet loss into sqlx database connections.

Determinism: The entire stack becomes deterministic, including 3rd party driver behavior.

## Use Cases

### Web Backends & APIs

Test your HTTP handlers under real-world conditions:

```rust
#[fracture::test]
async fn test_api_with_database_timeouts() {
    chaos::inject(ChaosOperation::TcpRead, 0.2);  // 20% DB read failures
    
    let response = handle_get_user(user_id).await;
    
    // Does your code return a proper error? Retry? Use a fallback?
    assert!(response.is_ok());
}
```

### Distributed Systems

Test consensus algorithms, replication, leader election:

```rust
#[fracture::test]
async fn test_raft_with_network_partition() {
    let scenario = Scenario::new()
        .partition("node1", "node2")
        .wait(Duration::from_secs(1))
        .heal_partition("node1", "node2");
    
    spawn(scenario.execute_chaos());
    
    // Run your Raft nodes
    // Does leader election still work?
}
```

### Background Job Processors

Test task queues with failures:

```rust
#[fracture::test]
async fn test_job_retry_logic() {
    chaos::inject(ChaosOperation::TaskPanic, 0.1);  // 10% tasks panic
    
    let results = process_jobs(job_queue).await;
    
    // Did failed jobs retry? Did you lose any work?
    assert_eq!(results.len(), expected_count);
}
```

### Real-Time Systems

Test WebSocket servers, event streams, subscriptions:

```rust
#[fracture::test]
async fn test_websocket_reconnection() {
    chaos::inject(ChaosOperation::TcpWrite, 0.3);
    
    let client = WebSocketClient::connect().await?;
    
    // Does your client reconnect on failures?
    // Do you maintain message order?
}
```

## Available Chaos Operations

Fracture can inject failures into:

- **Tasks**: `TaskSpawn`, `TaskAbort`, `TaskPanic`, `TaskDeadlock`, `TaskStarvation`
- **Channels**: `MpscSend`, `MpscRecv`, `OneshotSend`, `OneshotRecv`
- **Network**: `TcpConnect`, `TcpAccept`, `TcpRead`, `TcpWrite`
- **Time**: `SleepShort`, `SleepLong`, `TimeoutEarly`, `TimeoutLate`
- **Threading**: `SpawnBlocking`, `ThreadPoolExhaustion`

Set chaos rates from 0.0 (never) to 1.0 (always):

```rust
chaos::inject(ChaosOperation::TcpWrite, 0.5);  // 50% failure rate
```

## How It Works

1. **Simulation Runtime** - Fracture provides a complete async runtime that runs entirely in-memory
2. **Deterministic Scheduling** - Task execution order is controlled by a seeded RNG
3. **Chaos Injection** - At key points (sends, receives, I/O), Fracture can inject failures
4. **Time Control** - Virtual time advances deterministically, no real sleeps
5. **Reproducibility** - Same seed → same task order → same failures → same bugs

This is inspired by FoundationDB's approach to testing: run thousands of simulated scenarios to find rare edge cases.

## Testing Philosophy

**Traditional approach:**
```rust
#[tokio::test]
async fn test_happy_path() {
    let result = my_function().await;
    assert!(result.is_ok());  // ✅ Passes (but only in ideal conditions)
}
```

**Fracture approach:**
```rust
#[fracture::test]
async fn test_with_real_conditions() {
    chaos::inject(ChaosOperation::TcpWrite, 0.3);  // Real networks fail
    
    let result = my_function_with_retries().await;
    assert!(result.is_ok());  // ⚠️ Fails unless you handle errors properly
}
```

**Fracture forces you to write resilient code from the start.**

## API Overview


### Main Entry Point

```rust
#[fracture::main]                    // Basic async main (switches between Fracture/Tokio)
#[fracture::main(duration = "10s")]  // Optional duration parameter (when using simulation)
async fn main() { }
```

### Testing

```rust
#[fracture::test]                    // Basic test
#[fracture::test(duration = "10s")]  // Run for 10 simulated seconds
async fn my_test() { }
```

### Async Primitives

```rust
// Tasks
let handle = spawn(async { ... });
handle.await?;
handle.abort();

// Channels
let (tx, rx) = mpsc::unbounded();
let (tx, rx) = oneshot::channel();

// Time
sleep(Duration::from_millis(100)).await;
timeout(Duration::from_secs(1), operation).await?;

// Network (simulated)
let listener = TcpListener::bind("127.0.0.1:8000").await?;
let (stream, _) = listener.accept().await?;

// Select
select! {
    val = rx.recv() => { ... }
    _ = sleep(timeout) => { ... }
}
```

### Chaos Control

```rust
// Inject chaos
chaos::inject(ChaosOperation::TcpWrite, 0.5);

// Clear chaos
chaos::clear(ChaosOperation::TcpWrite);

// Scenarios
let scenario = Scenario::new()
    .seed(42)
    .delay("addr1", min, max)
    .partition("addr1", "addr2")
    .wait(duration)
    .heal_partition("addr1", "addr2");

spawn(scenario.execute_chaos());
```

## Examples

Check out the [tests/](fracture/tests/) directory for complete examples:

- **[demo_success.rs](fracture/tests/demo_success.rs)** - Resilient message passing with retries
- **[demo_failure.rs](fracture/tests/demo_failure.rs)** - Non-resilient code that fails under chaos
- **[simple_test.rs](fracture/tests/simple_test.rs)** - Basic async primitives
- **[modules_test.rs](fracture/tests/modules_test.rs)** - Testing different async modules

## Comparison

| Feature | Tokio | Loom | Jepsen | **Fracture** |
|---------|-------|------|--------|------------|
| Async runtime | ✅ | ❌ | ❌ | ✅ |
| Deterministic | ❌ | ✅ | ❌ | ✅ |
| Chaos injection | ❌ | ❌ | ✅ | ✅ |
| Fast (in-memory) | ✅ | ✅ | ❌ | ✅ |
| Network simulation | ❌ | ❌ | ✅ | ✅ |
| Reproducible bugs | ❌ | ✅ | ✅ | ✅ |

- **Tokio**: Production runtime, no chaos testing
- **Loom**: Concurrency testing (different problem space)
- **Jepsen**: Live cluster testing (slow, expensive)
- **Fracture**: Deterministic chaos testing for async code

## Limitations & Alpha Status

⚠️ **This is alpha software (v0.1.1).** We've tested it extensively, but async runtimes are complex and there are undoubtedly edge cases we haven't hit yet.

**Known limitations:**

- Only works with Fracture's async primitives (not Tokio/async-std directly)
- Limited chaos operations compared to real-world scenarios
- No file I/O simulation yet
- Performance not optimized for massive scale (1000+ tasks may be slow)
- Some edge cases in the runtime may cause panics or incorrect behavior

**We NEED your bug reports!** 

If you find issues (crashes, incorrect behavior, missing features), please:
1. **[Open an issue](https://github.com/yourusername/fracture/issues)** with details
2. Include the seed (`FRACTURE_SEED`) if the bug is reproducible
3. Minimal reproduction code is incredibly helpful

The more people use this in real scenarios, the more robust it becomes. Help us make async Rust more reliable by breaking Fracture first! 🔨

## Contributing

Found a bug? Want a feature? Open an issue or PR!

Areas we'd love help with:
- More chaos operations
- Performance improvements
- Integration with existing async ecosystems
- Better error messages
- More examples

## Roadmap

- [ ] File I/O simulation and chaos
- [ ] Clock skew injection
- [ ] Memory pressure simulation
- [ ] Better integration with Tokio/async-std
- [ ] Visualization of test execution traces
- [ ] Property-based testing integration
- [ ] More comprehensive network simulation

## Why "Fracture"?

Because we intentionally **break** your code to make it stronger. Chaos testing finds the **fracture points** in your system before production does.

## License

MIT License. See [LICENSE](../LICENSE) for details.

## Acknowledgments

Inspired by:
- FoundationDB's deterministic simulation testing
- [Jepsen](https://jepsen.io/) for pioneering chaos testing
- The Rust async ecosystem (Tokio, async-std, futures)