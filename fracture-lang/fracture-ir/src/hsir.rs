use serde::{Serialize, Deserialize};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Type {
    I32, I64, U32, U64, F32, F64,
    Bool,
    String,
    Array(Box<Type>, usize),
    Slice(Box<Type>),
    Ptr(Box<Type>),
    Ref(Box<Type>, bool),
    Struct(String),
    Function(Vec<Type>, Box<Type>),
    Future(Box<Type>),
    Void,
    Unknown
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Reg(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Label(pub String);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Const {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Bool(bool),
    String(String),
    Null
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Reg(Reg),
    Const(Const),
    Label(Label)
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Inst {
    Move { dst: Reg, src: Value, ty: Type },

    Add { dst: Reg, lhs: Value, rhs: Value, ty: Type },
    Sub { dst: Reg, lhs: Value, rhs: Value, ty: Type },
    Mul { dst: Reg, lhs: Value, rhs: Value, ty: Type },
    Div { dst: Reg, lhs: Value, rhs: Value, ty: Type },

    Eq { dst: Reg, lhs: Value, rhs: Value, ty: Type },
    Lt { dst: Reg, lhs: Value, rhs: Value, ty: Type },
    Gt { dst: Reg, lhs: Value, rhs: Value, ty: Type },

    Alloc { dst: Reg, size: Value, ty: Type },
    Load { dst: Reg, ptr: Value, ty: Type },
    Store { ptr: Value, src: Value, ty: Type },
    Free { ptr: Value },

    Jump { target: Label },
    JumpIf { cond: Value, target: Label },
    JumpIfFalse { cond: Value, target: Label },
    Label { target: Label },
    Call { dst: Option<Reg>, func: Value, args: Vec<Value>, ty: Type },
    Return { val: Option<Value> },

    Spawn { dst: Reg, func: Value, args: Vec<Value> },
    Await { dst: Option<Reg>, future: Value },
    Yield,

    SimPoint { id: String, metadata: HashMap<String, String> },

    BeginBorrow { reg: Reg, is_mut: bool },
    EndBorrow { reg: Reg }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Function {
    pub name: String,
    pub params: Vec<(Reg, Type)>,
    pub return_type: Type,
    pub body: Vec<Inst>,
    pub locals: HashMap<Reg, Type>
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Program {
    pub functions: HashMap<String, Function>,
    pub entry: String
}