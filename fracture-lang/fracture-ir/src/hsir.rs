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

impl Type {
    pub fn needs_drop(&self) -> bool {
        match self {
            Type::I32 | Type::I64 | Type::U32 | Type::U64 => false,
            Type::F32 | Type::F64 => false,
            Type::Bool => false,
            Type::Void => false,
            Type::String => true,
            Type::Vec(_) => true,
            Type::HashMap(_, _) => true,
            Type::Ptr(_) => false,
            Type::Ref(_, _) => false,
            Type::Array(elem_ty, _) => elem_ty.needs_drop(),
            Type::Slice(_) => false,
            Type::Tuple(types) => types.iter().any(|ty| ty.needs_drop()),
            // Conservative: Assuming all structs need drop, change later for optimization
            Type::Struct(_) => true,
            Type::Function(_, _) => false,
            Type::Future(_) => true,
            Type::Unknown => true
        }
    }
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
pub enum Visibility {
    Private,
    Public,
    Crate

    // Might expand this later with pub(super), pub(in path) etc.
}

impl Default for Visibility {
    fn default() -> Self {
        Visibility::Private
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ModulePath {
    pub segments: Vec<PathSegment>
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PathSegment {
    Ident(String),
    SelfKw,
    Super,
    Crate
}

impl ModulePath {
    pub fn new(name: &str) -> Self {
        Self { segments: vec![PathSegment::Ident(name.to_string())] }
    }

    pub fn from_parts(parts: Vec<&str>) -> Self {
        Self {
            segments: parts.into_iter().map(|s| PathSegment::Ident(s.to_string())).collect()
        }
    }

    pub fn leaf_name(&self) -> Option<&str> {
        self.segments.last().and_then(|seg| {
            match seg {
                PathSegment::Ident(name) => Some(name.as_str()),
                _ => None
            }
        })
    }

    // Hardcoded right now, change later to use user's tokens
    pub fn to_string(&self) -> String {
        self.segments.iter()
            .map(|seg| match seg {
                PathSegment::Ident(name) => name.clone(),
                PathSegment::SelfKw => "self".to_string(),
                PathSegment::Super => "super".to_string(),
                PathSegment::Crate => "crate".to_string()
            })
            .collect::<Vec<_>>()
            .join("::")
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum UseTree {
    Simple {
        path: ModulePath,
        alias: Option<String>
    },
    Nested {
        path: ModulePath,
        items: Vec<UseTree>
    },
    Glob {
        path: ModulePath
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UseStatement {
    pub tree: UseTree,
    pub visibility: Visibility
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    pub name: String,
    pub path: ModulePath,
    pub visibility: Visibility,
    pub uses: Vec<UseStatement>,
    pub functions: HashMap<String, Function>,
    pub structs: HashMap<String, StructDef>,
    pub children: HashMap<String, Module>,
    pub external_mods: Vec<String>
}

impl Module {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            path: ModulePath::new(name),
            visibility: Visibility::Private,
            uses: Vec::new(),
            functions: HashMap::new(),
            structs: HashMap::new(),
            children: HashMap::new(),
            external_mods: Vec::new(),
        }
    }

    pub fn root() -> Self {
        Self {
            name: "crate".to_string(),
            path: ModulePath { segments: vec![PathSegment::Crate] },
            visibility: Visibility::Public,
            uses: Vec::new(),
            functions: HashMap::new(),
            structs: HashMap::new(),
            children: HashMap::new(),
            external_mods: Vec::new()
        }
    }

    pub fn add_function(&mut self, func: Function) {
        self.functions.insert(func.name.clone(), func);
    }

    pub fn add_struct(&mut self, s: StructDef) {
        self.structs.insert(s.name.clone(), s);
    }

    pub fn add_child(&mut self, child: Module) {
        self.children.insert(child.name.clone(), child);
    }

    pub fn get_function(&self, name: &str) -> Option<&Function> {
        self.functions.get(name)
    }

    pub fn get_child(&self, name: &str) -> Option<&Module> {
        self.children.get(name)
    }
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
    HashMapClear { map: Reg }, // Optimization: Shrink when overallocated

    SysWrite { fd: Value, buf: Reg, len: Value, result_dst: Reg },
    SysRead { fd: Value, buf: Reg, len: Value, result_dst: Reg },
    SysOpen { path: Reg, flags: Value, mode: Value, result_dst: Reg },
    SysClose { fd: Value, result_dst: Reg },
    Print { value: Reg },
    Println { value: Reg },
    Eprint { value: Reg },
    Eprintln { value: Reg },
    ReadLine { dst: Reg },
    IntToString { dst: Reg, value: Value },
    SysSeek { fd: Value, offset: Value, whence: Value, result_dst: Reg },
    SysStat { path: Reg, stat_buf: Reg, result_dst: Reg },
    SysFstat { fd: Value, stat_buf: Reg, result_dst: Reg },
    SysMkdir { path: Reg, mode: Value, result_dst: Reg },
    SysRmdir { path: Reg, result_dst: Reg },
    SysUnlink { path: Reg, result_dst: Reg },
    SysRename { old_path: Reg, new_path: Reg, result_dst: Reg },
    SysAccess { path: Reg, mode: Value, result_dst: Reg },
    SysGetcwd { dst: Reg },
    SysChdir { path: Reg, result_dst: Reg },
    FileReadToString { dst: Reg, path: Reg, result_dst: Reg },
    FileWriteString { path: Reg, content: Reg, result_dst: Reg },
    FileAppendString { path: Reg, content: Reg, result_dst: Reg },

    DropScope { id: u32 },
    DropEndScope { id: u32 },
    DropRegister { reg: Reg, ty: Type, drop_fn: Option<String>, needs_drop: bool },
    DropUnregister { reg: Reg },
    DropCall { reg: Reg, ty: Type },
    DropFlag { reg: Reg, flag_reg: Reg },
    DropIfFlag { reg: Reg, flag_reg: Reg, ty: Type }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<(String, Type)>,
    pub visibility: Visibility,
    pub field_visibility: HashMap<String, Visibility>
}

impl StructDef {
    pub fn new(name: &str, fields: Vec<(String, Type)>) -> Self {
        Self {
            name: name.to_string(),
            fields,
            visibility: Visibility::Private,
            field_visibility: HashMap::new()
        }
    }

    pub fn public(name: &str, fields: Vec<(String, Type)>) -> Self {
        Self {
            name: name.to_string(),
            fields,
            visibility: Visibility::Public,
            field_visibility: HashMap::new()
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Function {
    pub name: String,
    pub params: Vec<(Reg, Type)>,
    pub return_type: Type,
    pub body: Vec<Inst>,
    pub locals: HashMap<Reg, Type>,
    pub attributes: Vec<String>,
    pub visibility: Visibility,
    pub module_path: Option<ModulePath>
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Program {
    pub functions: HashMap<String, Function>,
    pub structs: HashMap<String, StructDef>,
    pub entry: String,
    pub root_module: Module,
    pub uses: Vec<UseStatement>
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
            num_modules: count_modules(&self.root_module),
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

    pub fn resolve_function(&self, path: &ModulePath) -> Option<&Function> {
        if let Some(func) = self.functions.get(&path.to_string()) {
            return Some(func);
        }

        self.resolve_in_module(&self.root_module, path)
    }

    pub fn resolve_in_module<'a>(&'a self, module: &'a Module, path: &ModulePath) -> Option<&'a Function> {
        if path.segments.len() == 1 {
            if let PathSegment::Ident(name) = &path.segments[0] {
                return module.functions.get(name);
            }
        }

        if let PathSegment::Ident(first) = &path.segments[0] {
            if let Some(child) = module.children.get(first) {
                let remaining = ModulePath {
                    segments: path.segments[1..].to_vec()
                };

                return self.resolve_in_module(child, &remaining);
            }
        }

        None
    }
}

fn count_modules(module: &Module) -> usize {
    1 + module.children.values()
        .map(|child| count_modules(child))
        .sum::<usize>()
}

impl Function {
    pub fn has_attribute(&self, attr: &str) -> bool {
        self.attributes.iter().any(|a| a == attr)
    }

    pub fn is_unsafe(&self) -> bool {
        self.has_attribute("unsafe")
    }

    // Hardcoded using ::, change later to account for user token
    pub fn qualified_name(&self) -> String {
        if let Some(path) = &self.module_path {
            format!("{}::{}", path.to_string(), self.name)
        }
        else {
            self.name.clone()
        }
    }
}

#[derive(Debug, Clone)]
pub struct ProgramStats {
    pub num_functions: usize,
    pub num_structs: usize,
    pub num_modules: usize,
    pub total_instructions: usize,
    pub total_registers: usize
}

impl std::fmt::Display for ProgramStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Functions: {}, Instructions: {}, Registers: {}", self.num_functions, self.total_instructions, self.total_instructions)
    }
}