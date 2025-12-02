pub mod hsir;
pub mod printer;
pub mod syntax_config;

pub use hsir::{
    Inst,
    Type,
    Value,
    Reg,
    Program,
    Function,
    Const,
    Module,
    ModulePath,
    PathSegment,
    UseStatement,
    UseTree,
    Visibility,
    StructDef,
};
pub use syntax_config::SyntaxConfig;