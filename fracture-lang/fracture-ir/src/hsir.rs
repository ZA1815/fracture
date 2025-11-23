use serde::{Serialize, Deserialize};
use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};

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

impl Program {
    pub fn to_file(&self, path: &str) -> Result<(), String> {
        let encoded = bincode::serialize(self)
            .map_err(|e| format!("Serialization failed: {}", e))?;

        let mut file = File::create(path)
            .map_err(|e| format!("Failed to create file: {}", e))?;

        file.write_all(&encoded)
            .map_err(|e| format!("Failed to write file: {}", e))?;

        Ok(())
    }

    pub fn from_file(path: &str) -> Result<Self, String> {
        let mut file = File::open(path)
            .map_err(|e| format!("Failed to open file: {}", e))?;

        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)
            .map_err(|e| format!("Failed to read file: {}", e))?;

        let program: Program = bincode::deserialize(&buffer)
            .map_err(|e| format!("Deserialization failed: {}", e));

        Ok(program)
    }

    pub fn to_text(&self) -> String {
        use crate::printer;

        let mut output = String::new();
        output.push_str(&format!("; Program entry: {}\n\n", self.entry));

        for (name, func) in &self.functions {
            output.push_str(&format!("; Function: {}\n", name));
            output.push_str(&format!(";   params: {:?}\n", func.params));
            output.push_str(&format!(";   return: {:?}\n", func.return_type));
            output.push_str(&format!("    locals: {:?}\n\n", func.locals));

            for inst in &func.body {
                output.push_str(&format!("  {}\n", printer::print_inst(inst)));
            }

            output.push_str("\n");
        }

        output
    }

    pub fn stats(&self) -> ProgramStats {
        let mut total_insts = 0;
        let mut total_regs = 0;

        for func in self.functions.values() {
            total_insts += func.body.len();
            total_regs += func.locals.len();
        }

        ProgramStats {
            num_functions: self.functions.len(),
            total_instructions: total_insts,
            total_registers: total_regs
        }
    }
}

#[derive(Debug, Clone)]
pub struct ProgramStats {
    pub num_functions: usize,
    pub total_instructions: usize,
    pub total_registers: usize
}

impl std::fmt::Display for ProgramStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Functions: {}, Instructions: {}, Registers: {}", self.num_functions, self.total_instructions, self.total_instructions)
    }
}