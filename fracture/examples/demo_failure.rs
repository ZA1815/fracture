use fracture::prelude::*;
use fracture::chaos;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

/// This test demonstrates NON-RESILIENT code that fails under chaos.
/// Expected to FAIL to show what happens without proper error handling.
#[fracture::test(duration = "3s")]
async fn demo_replication_failure() {
    println!("\n🎯 Demonstrating NON-RESILIENT System (Will Fail)\n");
    
    let sent_count = Arc::new(AtomicUsize::new(0));
    let received_count = Arc::new(AtomicUsize::new(0));
    
    let (tx, mut rx) = mpsc::unbounded();
    
    let expected_messages = 15;
    
    // Inject chaos EARLY to cause failures
    println!("💥 Chaos injected: 40% send failure rate\n");
    chaos::inject(ChaosOperation::MpscSend, 0.4);
    
    // NON-RESILIENT SENDER: No retries, no error handling!
    let sent_clone = sent_count.clone();
    let sender = spawn(async move {
        for i in 0..expected_messages {
            sleep(std::time::Duration::from_millis(20)).await;
            
            // Just try once, no retries!
            match tx.send(format!("msg-{}", i)) {
                Ok(_) => {
                    sent_clone.fetch_add(1, Ordering::SeqCst);
                    println!("  → Sent message #{}", i);
                }
                Err(e) => {
                    println!("  ✗ Failed to send message #{}: {:?}", i, e);
                    // No retry! Just give up!
                }
            }
        }
        println!("\nSender finished");
    });
    
    // Receiver
    let recv_clone = received_count.clone();
    let receiver = spawn(async move {
        while let Some(msg) = rx.recv().await {
            recv_clone.fetch_add(1, Ordering::SeqCst);
            println!("  ← Received: {}", msg);
        }
    });
    
    let _ = sender.await;
    
    sleep(std::time::Duration::from_millis(300)).await;
    
    receiver.abort();
    
    let sent = sent_count.load(Ordering::SeqCst);
    let received = received_count.load(Ordering::SeqCst);
    let lost = sent.saturating_sub(received);
    
    println!("\n╔══════════════════ Test Results ══════════════════╗");
    println!("║ Expected messages: {:>6}                      ║", expected_messages);
    println!("║ Messages sent:     {:>6}                      ║", sent);
    println!("║ Messages received: {:>6}                      ║", received);
    println!("║ Messages lost:     {:>6}  {}                  ║", 
        lost, 
        if lost > 0 { "❌" } else { "✅" }
    );
    println!("╚═══════════════════════════════════════════════════╝\n");
    
    // This SHOULD fail because code isn't resilient
    if lost > 0 {
        panic!(
            "\n🚨 DATA LOSS DETECTED!\n\n\
            Expected {} messages, sent {}, but only received {}.\n\
            {} messages were lost due to chaos injection.\n\n\
            This demonstrates how systems WITHOUT retry logic fail under chaos.",
            expected_messages, sent, received, lost
        );
    }
}