pub mod hsir;
pub mod parser;
pub mod printer;
pub mod codegen_x86;
pub mod compiler;
pub mod syntax_config;
pub mod lexer;
pub mod projector;

pub use hsir::{Inst, Type, Value, Reg, Program, Function, Const};
pub use parser::Parser;
pub use compiler::Compiler;
pub use syntax_config::SyntaxConfig;
pub use projector::SyntaxProjector;