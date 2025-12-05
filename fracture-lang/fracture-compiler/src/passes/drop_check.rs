use fracture_ir::{Program, Function, Inst, Value, Type, Reg};
use std::{collections::{HashMap, HashSet}, usize};

#[derive(Debug, Clone)]
struct DropEntry {
    reg: Reg,
    ty: Type,
    scope_id: u32,
    needs_drop: bool,
    is_moved: bool,
    // drop_flag: Option<Reg> later
}

#[derive(Debug)]
struct DropState {
    entries: HashMap<Reg, DropEntry>,
    scope_stack: Vec<u32>,
    next_scope_id: u32,
    scope_values: HashMap<u32, Vec<Reg>>
}

impl DropState {
    fn new() -> Self {
        Self {
            entries: HashMap::new(),
            scope_stack: Vec::new(),
            next_scope_id: 0,
            scope_values: HashMap::new()
        }
    }

    fn begin_scope(&mut self) -> u32 {
        let id = self.next_scope_id;
        self.next_scope_id += 1;
        self.scope_stack.push(id);
        self.scope_values.insert(id, Vec::new());

        id
    }

    fn end_scope(&mut self) -> Vec<DropEntry> {
        let scope_id = self.scope_stack.pop()
            .expect("end_scope called without matching begin_scope");

        let values = self.scope_values.remove(&scope_id)
            .unwrap_or_default();

        let mut to_drop = Vec::new();
        for reg in values.into_iter().rev() {
            if let Some(entry) = self.entries.remove(&reg) {
                if entry.needs_drop && !entry.is_moved {
                    to_drop.push(entry);
                }
            }
        }

        to_drop
    }

    fn current_scope(&self) -> u32 {
        *self.scope_stack.last().expect("No active scope")
    }

    fn register(&mut self, reg: Reg, ty: Type) {
        let scope_id = self.current_scope();
        let needs_drop = ty.needs_drop();

        let entry = DropEntry {
            reg: reg.clone(),
            ty,
            scope_id,
            needs_drop,
            is_moved: false
        };

        self.entries.insert(reg.clone(), entry);

        if let Some(values) = self.scope_values.get_mut(&scope_id) {
            values.push(reg);
        }
    }

    fn mark_moved(&mut self, reg: &Reg) {
        if let Some(entry) = self.entries.get_mut(reg) {
            entry.is_moved = true;
        }
    }

    fn unregister(&mut self, reg: &Reg) {
        self.entries.remove(reg);
    }
}

pub fn analyze(program: &Program) -> Result<ProgramDropInfo, String> {
    let mut info = ProgramDropInfo { function_drops: HashMap::new() };

    for (name, func) in &program.functions {
        if func.is_unsafe() {
            println!("  Skipping drop analysis for #[unsafe] function: {}", name);
            continue;
        }

        let func_info = analyze_function(name, func)?;
        info.function_drops.insert(name.clone(), func_info);
    }

    Ok(info)
}

fn analyze_function(name: &str, func: &Function) -> Result<FunctionDropInfo, String> {
    let mut state = DropState::new();
    let mut drops: HashMap<usize, Vec<DropEntry>> = HashMap::new();

    state.begin_scope();

    for (reg, ty) in &func.params {
        state.register(reg.clone(), ty.clone());
    }

    for (idx, inst) in func.body.iter().enumerate() {
        analyze_instruction(inst, &mut state, &mut drops, idx)?;
    }

    let final_drops = state.end_scope();

    Ok(FunctionDropInfo {
        drops_before_inst: drops,
        drops_at_end: final_drops
    })
}

fn analyze_instruction(inst: &Inst, state: &mut DropState, drops: &mut HashMap<usize, Vec<DropEntry>>, idx: usize) -> Result<(), String> {
    match inst {
        Inst::VecAlloc { dst, element_ty, initial_cap } => {
            let ty = Type::Vec(Box::new(element_ty.clone()));
            state.register(dst.clone(), ty);
        }
        Inst::StringAlloc { dst, data } => {
            state.register(dst.clone(), Type::String);
        }
        Inst::HashMapAlloc { dst, key_ty, value_ty, initial_cap } => {
            let ty = Type::HashMap(Box::new(key_ty.clone()), Box::new(value_ty.clone()));

            state.register(dst.clone(), ty);
        }
        Inst::StructAlloc { dst, struct_name } => {
            state.register(dst.clone(), Type::Struct(struct_name.clone()));
        }
        Inst::ArrayAlloc { dst, element_ty, size } => {
            let count = match size {
                Value::Const(fracture_ir::Const::I32(n)) => *n as usize,
                _ => 0
            };

            state.register(dst.clone(), Type::Array(Box::new(element_ty.clone()), count));
        }
        Inst::TupleAlloc { dst, element_types } => {
            state.register(dst.clone(), Type::Tuple(element_types.clone()));
        }
        Inst::HeapAlloc { dst, size } => {
            state.register(dst.clone(), Type::Ptr(Box::new(Type::Unknown)));
        }
        Inst::Move { dst, src, ty } => {
            if let Value::Reg(src_reg) = src {
                if ty.needs_drop() {
                    state.mark_moved(src_reg);
                }
            }

            if ty.needs_drop() {
                state.register(dst.clone(), ty.clone());
            }
        }
        // Requires knowing func signature, improve later
        Inst::Call { dst, func, args, ty } => {
            for arg in args {
                if let Value::Reg(reg) = arg {
                    state.mark_moved(reg);
                }
            }

            if let Some(dst_reg) = dst {
                if ty.needs_drop() {
                    state.register(dst_reg.clone(), ty.clone());
                }
            }
        }
        Inst::Return { val } => {
            if let Some(Value::Reg(ret_reg)) = val {
                state.mark_moved(ret_reg);
            }

            // Everything in all active scopes needs to drop
        }
        _ => {}
    }

    Ok(())
}

#[derive(Debug)]
pub struct ProgramDropInfo {
    pub function_drops: HashMap<String, FunctionDropInfo>
}

#[derive(Debug)]
pub struct FunctionDropInfo {
    pub drops_before_inst: HashMap<usize, Vec<DropEntry>>,
    pub drops_at_end: Vec<DropEntry>
}

pub fn insert_drops(program: &mut Program, info: &ProgramDropInfo) {
    for (name, func_info) in &info.function_drops {
        if let Some(func) = program.functions.get_mut(name) {
            insert_function_drops(func, func_info);
        }
    }
}

fn insert_function_drops(func: &mut Function, info: &FunctionDropInfo) {
    let mut new_body = Vec::new();

    for (idx, inst) in func.body.iter().enumerate() {
        if let Some(drops) = info.drops_before_inst.get(&idx) {
            for entry in drops {
                new_body.push(Inst::DropCall {
                    reg: entry.reg.clone(),
                    ty: entry.ty.clone()
                });
            }
        }

        new_body.push(inst.clone());
    }

    for entry in &info.drops_at_end {
        new_body.push(Inst::DropCall {
            reg: entry.reg.clone(),
            ty: entry.ty.clone()
        });
    }

    func.body = new_body;
}

pub fn check(program: &mut Program) -> Result<(), String> {
    println!("  Running drop analysis...");

    let info = analyze(program)?;

    let total_drops: usize = info.function_drops.values()
        .map(|f| f.drops_at_end.len() + f.drops_before_inst.values().map(|v| v.len()).sum::<usize>()).sum();

    println!("  Identified {} drop points", total_drops);

    insert_drops(program, &info);

    println!("  Drop insertion complete");

    Ok(())
}