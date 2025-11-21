use fracture::prelude::*;

#[test]
fn test_simple_spawn() {
    let runtime = fracture::runtime::Runtime::new();
    let result = runtime.block_on(async {
        let handle = spawn(async {
            42
        });
        handle.await.unwrap()
    });
    assert_eq!(result, 42);
}

#[test]
fn test_simple_sleep() {
    let runtime = fracture::runtime::Runtime::new();
    runtime.block_on(async {
        println!("Before sleep");
        sleep(std::time::Duration::from_millis(10)).await;
        println!("After sleep");
    });
}

#[test]
fn test_multiple_tasks() {
    let runtime = fracture::runtime::Runtime::new();
    runtime.block_on(async {
        let h1 = spawn(async { 1 });
        let h2 = spawn(async { 2 });
        let r1 = h1.await.unwrap();
        let r2 = h2.await.unwrap();
        assert_eq!(r1 + r2, 3);
    });
}
