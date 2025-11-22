use fracture_ir::{SyntaxConfig, SyntaxProjector, Compiler, printer};

fn main() {
    let python_code = r#"
def add(x, y):
    result = x + y
    return result

def main():
    a = 10
    b = 32  
    c = add(a, b)
    return c
"#;

    println!("Python-like code:");
    println!("{}", python_code);
    
    let config = SyntaxConfig::python();
    let mut projector = SyntaxProjector::new(python_code, config);
    
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
            match compiler.compile_to_file("rust_style_program") {
                Ok(_) => {
                    println!("\nCompiled! Run with:");
                    println!("  ./rust_style_program");
                    println!("  echo $?");
                }
                Err(e) => println!("Compilation error: {}", e),
            }
        }
        Err(e) => println!("Projection error: {}", e),
    }
    
    let rust_code = r#"
fn main() -> i32 {
    let x = 25;
    let y = 17;
    return x + y;
}
"#;

    println!("\n\nRust-like code:");
    println!("{}", rust_code);
    
    let config = SyntaxConfig::rust();
    let mut projector = SyntaxProjector::new(rust_code, config);
    
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
            match compiler.compile_to_file("python_style_program") {
                Ok(_) => {
                    println!("\nCompiled! Run with:");
                    println!("  ./python_style_program");
                    println!("  echo $?");
                }
                Err(e) => println!("Compilation error: {}", e),
            }
        }
        Err(e) => println!("Projection error: {}", e),
    }
}