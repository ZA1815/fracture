use fracture::prelude::*;
use fracture::io::{AsyncReadExt, AsyncWriteExt};

#[fracture::test]
async fn test_filesystem_ops() {
    // Test basic file creation and reading
    let path = "test_file.txt";
    let content = b"Hello, Fracture!";

    let mut file = File::create(path).await.unwrap();
    file.write_all(content).await.unwrap();
    file.sync_all().await.unwrap();

    let mut file = File::open(path).await.unwrap();
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).await.unwrap();

    assert_eq!(buffer, content);
    
    // Cleanup
    fracture::fs::remove_file(path).await.unwrap();
}

#[fracture::test]
async fn test_networking_tcp() {
    // Simple TCP Echo Server/Client
    let addr = "127.0.0.1:8080";
    
    let server = spawn(async move {
        let mut listener = TcpListener::bind(addr).await.unwrap();
        let (mut socket, _) = listener.accept().await.unwrap();
        let (mut rd, mut wr) = socket.split();
        fracture::io::copy(&mut rd, &mut wr).await.unwrap();
    });

    // Give server time to bind (deterministic sleep)
    sleep(std::time::Duration::from_millis(10)).await;

    let mut stream = TcpStream::connect(addr).await.unwrap();
    let msg = b"echo test";
    stream.write_all(msg).await.unwrap();

    let mut buf = vec![0u8; msg.len()];
    stream.read_exact(&mut buf).await.unwrap();
    
    assert_eq!(&buf, msg);
    
    // Clean up server
    drop(server); 
}

#[fracture::test]
async fn test_sync_primitives() {
    // Test Mutex and Channels
    let counter = std::sync::Arc::new(Mutex::new(0));
    let (tx, mut rx) = mpsc::channel(10);

    for i in 0..5 {
        let c = counter.clone();
        let t = tx.clone();
        spawn(async move {
            let mut lock = c.lock().await;
            *lock += 1;
            t.send(i).await.unwrap();
        });
    }
    drop(tx);

    let mut results = Vec::new();
    while let Some(i) = rx.recv().await {
        results.push(i);
    }

    assert_eq!(*counter.lock().await, 5);
    assert_eq!(results.len(), 5);
}

#[fracture::test]
async fn test_time_ordering() {
    let start = Instant::now();
    sleep(std::time::Duration::from_millis(100)).await;
    let elapsed = start.elapsed();
    
    // In simulation, this should be exact or very close
    assert!(elapsed >= std::time::Duration::from_millis(100));
}

#[fracture::test]
async fn test_process_mock() {
    // Test the mock process registry
    let mut cmd = Command::new("echo");
    cmd.arg("hello world");
    
    let output = cmd.output().await.unwrap();
    
    // Fracture's default mock echoes a placeholder message unless configured otherwise
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("Output from echo"));
    assert!(output.status.success());
}