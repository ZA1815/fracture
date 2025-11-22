pub mod hsir;
pub mod parser;
pub mod printer;

pub use hsir::{Inst, Type, Value, Reg, Program};
pub use parser::Parser;