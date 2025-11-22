use fracture_ir::{Program, Function, Compiler, hsir::*};
use std::collections::HashMap;

fn main() {
    let mut program = Program {
        functions: HashMap::new(),
        entry: "main".to_string()
    };

    let main_func = Function {
        name: "main".to_string(),
        params: vec![],
        return_type: Type::I32,
        body: vec![
            Inst::Move { dst: Reg(0), src: Value::Const(Const::I32(35)), ty: Type::I32 },

            Inst::Move { dst: Reg(1), src: Value::Const(Const::I32(7)), ty: Type::I32 },

            Inst::Add { dst: Reg(2), lhs: Value::Reg(Reg(0)), rhs: Value::Reg(Reg(1)), ty: Type::I32 },

            Inst::Return { val: Some(Value::Reg(Reg(2))) }
        ],
        locals: HashMap::new()
    };

    program.functions.insert("main".to_string(), main_func);

    let compiler = Compiler::new(program);
    match compiler.compile_to_file("test_program") {
        Ok(_) => {
            println!("\nSuccess. Run with:");
            println!("  ./test_program");
            println!("  echo $?");
        }
        Err(e) => println!("Compilation failed: {}", e)
    }
}