use fracture_ir::{SyntaxConfig, SyntaxProjector, Compiler, printer};

fn main() {
    let test_params = r#"
fn add(x: i32, y: i32) -> i32 {
    return x + y;
}

fn main() -> i32 {
    let a = 10;
    let b = 32;
    let c = add(a, b);
    return c;
}
"#;

    println!("=== Test 1: Function parameters and calls ===");
    println!("{}", test_params);
    
    let config = SyntaxConfig::rust();
    let mut projector = SyntaxProjector::new(test_params, config);
    
    match projector.project_to_hsir() {
        Ok(program) => {
            println!("\nGenerated HSIR:");
            for (name, func) in &program.functions {
                println!("Function {} (params: {:?}):", name, func.params);
                for inst in &func.body {
                    println!("  {}", printer::print_inst(inst));
                }
            }
            
            let compiler = Compiler::new(program);
            match compiler.compile_to_file("test_params") {
                Ok(_) => println!("\n✓ Compiled to ./test_params"),
                Err(e) => println!("Compilation error: {}", e),
            }
        }
        Err(e) => println!("Projection error: {}", e),
    }

    let test_control = r#"
fn factorial(n: i32) -> i32 {
    let result = 1;
    let i = 1;
    while i < n {
        result = result * i;
        i = i + 1;
    }
    return result;
}

fn main() -> i32 {
    let x = factorial(5);
    if x == 120 {
        return 1;
    }
    return 0;
}
"#;

    println!("\n\n=== Test 2: Control flow (if/while) ===");
    println!("{}", test_control);
    
    let config = SyntaxConfig::rust();
    let mut projector = SyntaxProjector::new(test_control, config);
    
    match projector.project_to_hsir() {
        Ok(program) => {
            println!("\nGenerated HSIR:");
            for (name, func) in &program.functions {
                println!("Function {}:", name);
                for inst in &func.body {
                    println!("  {}", printer::print_inst(inst));
                }
            }
            
            let compiler = Compiler::new(program);
            match compiler.compile_to_file("test_control") {
                Ok(_) => println!("\n✓ Compiled to ./test_control"),
                Err(e) => println!("Compilation error: {}", e),
            }
        }
        Err(e) => println!("Projection error: {}", e),
    }

    let test_python = r#"
def factorial(n: int) -> int:
    result = 1
    i = 1
    while i < n:
        result = result * i
        i = i + 1
    return result

def main() -> int:
    x = factorial(5)
    if x == 120:
        return 1
    return 0
"#;

    println!("\n\n=== Test 3: Same logic in Python syntax ===");
    println!("{}", test_python);
    
    let config = SyntaxConfig::python();
    let mut projector = SyntaxProjector::new(test_python, config);
    
    match projector.project_to_hsir() {
        Ok(program) => {
            println!("\nGenerated HSIR (should be nearly identical to Test 2):");
            for (name, func) in &program.functions {
                println!("Function {}:", name);
                for inst in &func.body {
                    println!("  {}", printer::print_inst(inst));
                }
            }
            
            let compiler = Compiler::new(program);
            match compiler.compile_to_file("test_python") {
                Ok(_) => println!("\n✓ Compiled to ./test_python"),
                Err(e) => println!("Compilation error: {}", e),
            }
        }
        Err(e) => println!("Projection error: {}", e),
    }
}