use fracture_ir::{Parser, printer};

fn main() {
    let code = r#"
    move r0, 42, i32
    add r1, r0, 10, i32
    jump loop_start
    "#;

    let mut parser = Parser::new(code);

    println!("Parsing HSIR:");
    while let Some(inst) = parser.parse_inst() {
        println!("  Parsed: {:?}", inst);
        println!("  Printed: {}", printer::print_inst(&inst));
    }
}