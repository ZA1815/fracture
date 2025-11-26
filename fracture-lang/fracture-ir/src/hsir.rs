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
    Tuple(Vec<Type>),
    Ptr(Box<Type>),
    Ref(Box<Type>, bool),
    Struct(String),
    Function(Vec<Type>, Box<Type>),
    Future(Box<Type>),
    Vec(Box<Type>),
    HashMap(Box<Type>, Box<Type>),
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

    HeapAlloc { dst: Reg, size: Value },
    HeapRealloc { dst: Reg, ptr: Value, old_size: Value, new_size: Value },
    HeapFree { ptr: Value },

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
    EndBorrow { reg: Reg },

    StructAlloc { dst: Reg, struct_name: String },
    FieldLoad { dst: Reg, struct_reg: Reg, field_name: String, ty: Type },
    FieldStore { struct_reg: Reg, field_name: String, value: Value, ty: Type },

    // Maybe allow any types in array like JS later
    ArrayAlloc { dst: Reg, element_ty: Type, size: Value},
    IndexLoad { dst: Reg, array: Reg, index: Value, element_ty: Type },
    IndexStore { array: Reg, index: Value, value: Value, element_ty: Type },

    SliceCreate { dst: Reg, array: Reg, start: Value, end: Value, element_ty: Type },
    SliceLen { dst: Reg, slice: Reg },
    SliceIndexLoad { dst: Reg, slice: Reg, index: Value, element_ty: Type },

    TupleAlloc { dst: Reg, element_types: Vec<Type> },
    TupleLoad { dst: Reg, tuple_reg: Reg, index: usize, ty: Type },
    TupleStore { tuple_reg: Reg, index: usize, value: Value, ty: Type },

    StringAlloc { dst: Reg, data: String },
    StringLen { dst: Reg, string: Reg },
    StringConcat { dst: Reg, left: Reg, right: Reg },
    StringPush { string: Reg, value: Value },
    StringIndex { dst: Reg, string: Reg, index: Value },

    VecAlloc { dst: Reg, element_ty: Type, initial_cap: Value },
    VecPush { vec: Reg, value: Value, element_ty: Type },
    VecPop { dst: Reg, vec: Reg, element_ty: Type },
    VecGet { dst: Reg, vec: Reg, index: Value, element_ty: Type },
    VecSet { vec: Reg, index: Value, value: Value, element_ty: Type },
    VecLen { dst: Reg, vec: Reg },
    VecCap { dst: Reg, vec: Reg },

    // Optimize hashmap later using SwissTable strategy (final touch optimization)
    HashMapAlloc { dst: Reg, key_ty: Type, value_ty: Type, initial_cap: Value },
    HashMapInsert { map: Reg, key: Value, value: Value, key_ty: Type, value_ty: Type },
    HashMapGet { dst: Reg, found_dst: Reg, map: Reg, key: Value, key_ty: Type, value_ty: Type },
    HashMapRemove { success_dst: Reg, map: Reg, key: Value, key_ty: Type, value_ty: Type },
    HashMapContains { dst: Reg, map: Reg, key: Value, key_ty: Type, value_ty: Type },
    HashMapLen { dst: Reg, map: Reg },
    HashMapCap { dst: Reg, map: Reg },
    HashMapClear { map: Reg } // Optimization: Shrink when overallocated
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<(String, Type)>
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Function {
    pub name: String,
    pub params: Vec<(Reg, Type)>,
    pub return_type: Type,
    pub body: Vec<Inst>,
    pub locals: HashMap<Reg, Type>,
    pub attributes: Vec<String>
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Program {
    pub functions: HashMap<String, Function>,
    pub structs: HashMap<String, StructDef>,
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
            .map_err(|e| format!("Deserialization failed: {}", e))?;

        Ok(program)
    }

    pub fn to_text(&self) -> String {
        use crate::printer;

        let mut output = String::new();
        output.push_str(&format!("; Program entry: {}\n\n", self.entry));

        for (name, struct_def) in &self.structs {
            output.push_str(&format!("; Struct: {}\n", name));
            for (field_name, field_type) in &struct_def.fields {
                output.push_str(&format!(";    {}: {:?}\n", field_name, field_type));
            }
            output.push_str("\n");
        }

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
            num_structs: self.structs.len(),
            total_instructions: total_insts,
            total_registers: total_regs
        }
    }

    pub fn get_struct(&self, name: &str) -> Option<&StructDef> {
        self.structs.get(name)
    }

    pub fn field_offset(&self, struct_name: &str, field_name: &str) -> Option<(usize, &Type)> {
        let struct_def = self.get_struct(struct_name)?;

        for (i, (fname, ftype)) in struct_def.fields.iter().enumerate() {
            if fname == field_name {
                return Some((i, ftype));
            }
        }

        None
    }
}

impl Function {
    pub fn has_attribute(&self, attr: &str) -> bool {
        self.attributes.iter().any(|a| a == attr)
    }

    pub fn is_unsafe(&self) -> bool {
        self.has_attribute("unsafe")
    }
}

#[derive(Debug, Clone)]
pub struct ProgramStats {
    pub num_functions: usize,
    pub num_structs: usize,
    pub total_instructions: usize,
    pub total_registers: usize
}

impl std::fmt::Display for ProgramStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Functions: {}, Instructions: {}, Registers: {}", self.num_functions, self.total_instructions, self.total_instructions)
    }
}