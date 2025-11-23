pub mod hsir;
pub mod parser;
pub mod printer;
pub mod compiler;
pub mod syntax_config;

pub use hsir::{Inst, Type, Value, Reg, Program, Function, Const};
pub use parser::Parser;
pub use compiler::Compiler;
pub use syntax_config::SyntaxConfig;