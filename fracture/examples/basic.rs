use fracture::prelude::*;

async fn echo_server() -> anyhow::Result<()> {
    let listener = TcpListener::bind("0.0.0.0:3000").await?;
    println!("Server listening on :3000");
    
    loop {
        let (mut socket, addr) = listener.accept().await?;
        println!("Connection from {:?}", addr);
        
        spawn(async move {
            let mut buf = vec![0; 1024];
            loop {
                match socket.read(&mut buf).await {
                    Ok(0) => break,
                    Ok(n) => {
                        socket.write_all(&buf[..n]).await.unwrap();
                    }
                    Err(e) => {
                        eprintln!("Error: {}", e);
                        break;
                    }
                }
            }
        });
    }
}

async fn client() -> anyhow::Result<()> {
    sleep(Duration::from_millis(100)).await;
    
    let mut stream = TcpStream::connect("127.0.0.1:3000").await?;
    stream.write_all(b"Hello Fracture!").await?;
    
    let mut buf = vec![0; 15];
    stream.read_exact(&mut buf).await?;
    
    println!("Got: {}", String::from_utf8_lossy(&buf));
    Ok(())
}

#[fracture::main]
async fn main() {
    spawn(echo_server());
    client().await.unwrap();
}