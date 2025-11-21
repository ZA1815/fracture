use fracture::prelude::*;
use fracture::chaos;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

/// AGGRESSIVE chaos test - pushes the system to its limits.
/// Resilient code should PASS even with heavy chaos injection.
#[fracture::test(duration = "10s")]
async fn demo_replication_success() {
    println!("\n🔥 AGGRESSIVE CHAOS TEST - Testing System Resilience\n");
    
    let messages_to_send = 50;
    let delivered = Arc::new(AtomicUsize::new(0));
    let retries = Arc::new(AtomicUsize::new(0));
    
    // Channels
    let (msg_tx, mut msg_rx) = mpsc::unbounded();
    let (ack_tx, mut ack_rx) = mpsc::unbounded();
    
    // INJECT AGGRESSIVE CHAOS
    println!("💥 Injecting HEAVY chaos:");
    println!("   - 50% TCP write failure rate");
    println!("   - 30% read failure rate");
    println!("   - Random delays up to 100ms");
    println!("   - Network partitions\n");
    
    chaos::inject(ChaosOperation::TcpWrite, 0.5);
    chaos::inject(ChaosOperation::TcpRead, 0.3);
    
    let scenario = Scenario::new()
        .seed(999)
        .delay("192.168.1.1:8000", std::time::Duration::from_millis(10), std::time::Duration::from_millis(100))
        .wait(std::time::Duration::from_millis(500))
        .partition("192.168.1.1:8000", "192.168.1.2:8000")
        .wait(std::time::Duration::from_millis(200))
        .heal_partition("192.168.1.1:8000", "192.168.1.2:8000");
    
    spawn(scenario.execute_chaos());
    
    // RESILIENT SENDER
    let delivered_clone = delivered.clone();
    let retries_clone = retries.clone();
    let sender = spawn(async move {
        for i in 0..messages_to_send {
            let mut attempts = 0;
            const MAX_ATTEMPTS: usize = 50;
            
            loop {
                attempts += 1;
                
                match msg_tx.send((i, format!("msg-{}", i))) {
                    Ok(_) => {
                        match timeout(std::time::Duration::from_millis(100), ack_rx.recv()).await {
                            Ok(Some(ack_id)) if ack_id == i => {
                                delivered_clone.fetch_add(1, Ordering::SeqCst);
                                if attempts > 1 {
                                    retries_clone.fetch_add(attempts - 1, Ordering::SeqCst);
                                    println!("  ✓ msg-{} delivered after {} attempts", i, attempts);
                                } else {
                                    println!("  ✓ msg-{} delivered", i);
                                }
                                break;
                            }
                            _ => {
                                if attempts >= MAX_ATTEMPTS {
                                    println!("  ✗ msg-{} FAILED after {} attempts", i, MAX_ATTEMPTS);
                                    break;
                                }
                                let backoff = std::cmp::min(attempts * 5, 50);
                                sleep(std::time::Duration::from_millis(backoff as u64)).await;
                            }
                        }
                    }
                    Err(_) => {
                        println!("  ✗ Channel closed for msg-{}", i);
                        break;
                    }
                }
            }
        }
        println!("\n📤 Sender finished");
    });
    
    // RECEIVER
    let receiver = spawn(async move {
        let mut count = 0;
        while let Some((id, _data)) = msg_rx.recv().await {
            count += 1;
            let _ = ack_tx.send(id);
            
            if count >= messages_to_send {
                break;
            }
        }
        println!("📥 Receiver finished ({} messages)", count);
        count
    });
    
    sender.await.unwrap();
    let received = receiver.await.unwrap();
    
    sleep(std::time::Duration::from_millis(500)).await;
    
    let delivered_count = delivered.load(Ordering::SeqCst);
    let retry_count = retries.load(Ordering::SeqCst);
    
    println!("\n╔════════════════════ RESULTS ════════════════════╗");
    println!("║ Expected:  {:>3}                                ║", messages_to_send);
    println!("║ Delivered: {:>3}                                ║", delivered_count);
    println!("║ Received:  {:>3}                                ║", received);
    println!("║ Retries:   {:>3}                                ║", retry_count);
    println!("║ Success:   {}                             ║", 
        if delivered_count == messages_to_send { "✅ YES" } else { "❌ NO " });
    println!("╚═════════════════════════════════════════════════╝\n");
    
    // Check AFTER all work is done
    assert_eq!(delivered_count, messages_to_send, 
        "System FAILED under chaos! Only {}/{} messages delivered.", 
        delivered_count, messages_to_send
    );
    
    println!("✅ SYSTEM IS RESILIENT! Survived aggressive chaos with {} retries.\n", retry_count);
}