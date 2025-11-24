use fracture_ir::hsir::*;
use std::collections::HashMap;

pub struct X86CodeGen {
    output: Vec<String>,
    reg_offsets: HashMap<Reg, i32>,
    next_stack_offset: i32,
    current_function_stack_size: i32,
    program: Option<Program>,
    struct_layouts: HashMap<String, Vec<(String, usize, Type)>>
}

impl X86CodeGen {
    pub fn new() -> Self {
        Self {
            output: Vec::new(),
            reg_offsets: HashMap::new(),
            next_stack_offset: 8,
            current_function_stack_size: 0,
            program: None,
            struct_layouts: HashMap::new()
        }
    }

    pub fn compile_program(&mut self, program: &Program) -> String {
        self.program = Some(program.clone());
        self.compute_struct_layouts(program);

        self.emit(".intel_syntax noprefix");
        self.emit(".global _start");
        self.emit("");

        for (name, func) in &program.functions {
            self.compile_function(name, func);
        }

        self.emit("_start:");
        self.emit("    call main");
        self.emit("    mov rdi, rax");
        self.emit("    mov rax, 60");
        self.emit("    syscall");

        self.output.join("\n")
    }

    fn compute_struct_layouts(&mut self, program: &Program) {
        for (struct_name, struct_def) in &program.structs {
            let mut layout = Vec::new();
            let mut offset = 0;

            for (field_name, field_type) in &struct_def.fields {
                let size = self.type_size(field_type);
                layout.push((field_name.clone(), offset, field_type.clone()));
                offset += size;
            }

            self.struct_layouts.insert(struct_name.clone(), layout);
        }
    }

    fn type_size(&self, ty: &Type) -> usize {
        match ty {
            Type::I32 | Type::U32 | Type::F32 => 4,
            Type::I64 | Type::U64 | Type::F64 | Type::Ptr(_) | Type::Ref(_, _) => 8,
            Type::Bool => 1,
            Type::String => 16,
            Type::Struct(name) => {
                if let Some(layout) = self.struct_layouts.get(name) {
                    layout.iter().map(|(_, _, ty)| self.type_size(ty)).sum()
                }
                else {
                    8
                }
            }
            _ => 8
        }
    }

    fn get_struct_size(&self, struct_name: &str) -> usize {
        if let Some(layout) = self.struct_layouts.get(struct_name) {
            layout.iter().map(|(_, _, ty)| self.type_size(ty)).sum()
        }
        else {
            0
        }
    }

    pub fn get_field_offset(&self, struct_name: &str, field_name: &str) -> Option<usize> {
        let layout = self.struct_layouts.get(struct_name)?;

        for (fname, offset, _) in layout {
            if fname == field_name {
                return Some(*offset);
            }
        }

        None
    }

    fn compile_function(&mut self, name: &str, func: &Function) {
        self.emit(&format!("{}:", name));

        self.emit("    push rbp");
        self.emit("    mov rbp, rsp");

        let stack_size = (func.locals.len() + func.params.len() + 4) * 16;
        self.current_function_stack_size = stack_size as i32;
        self.emit(&format!("    sub rsp, {}", stack_size));

        self.reg_offsets.clear();
        self.next_stack_offset = 8;

        let param_regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
        for (i, (param_reg, _)) in func.params.iter().enumerate() {
            if i < param_regs.len() {
                let offset = self.get_or_alloc_reg_offset(param_reg);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], {}", offset, param_regs[i]));
            }
        }

        for inst in &func.body {
            self.compile_inst(inst);
        }

        self.emit("    xor rax, rax");
        self.emit("    leave");
        self.emit("    ret");
        self.emit("");
    }

    fn compile_inst(&mut self, inst: &Inst) {
        match inst {
            Inst::Move { dst, src, ty } => {
                self.load_value_to_rax(src, ty);
                let offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
            }
            Inst::Add { dst, lhs, rhs, ty } => {
                self.load_value_to_rax(lhs, ty);
                self.emit("    mov rcx, rax");
                self.load_value_to_rax(rhs, ty);
                self.emit("    add rax, rcx");
                let offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
            }
            Inst::Sub { dst, lhs, rhs, ty } => {
                self.load_value_to_rax(lhs, ty);
                self.emit("    mov rcx, rax");
                self.load_value_to_rax(rhs, ty);
                self.emit("    sub rcx, rax");
                self.emit("    mov rax, rcx");
                let offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
            }
            Inst::Mul { dst, lhs, rhs, ty } => {
                self.load_value_to_rax(lhs, ty);
                self.emit("    mov rcx, rax");
                self.load_value_to_rax(rhs, ty);
                self.emit("    imul rax, rcx");
                let offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
            }
            Inst::Div { dst, lhs, rhs, ty } => {
                self.load_value_to_rax(rhs, ty);
                self.emit("    mov rcx, rax");
                self.load_value_to_rax(lhs, ty);
                self.emit("    xor rdx, rdx");
                self.emit("    idiv rcx");
                let offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
            }
            Inst::Eq { dst, lhs, rhs, ty } => {
                self.load_value_to_rax(lhs, ty);
                self.emit("    mov rcx, rax");
                self.load_value_to_rax(rhs, ty);
                self.emit("    cmp rcx, rax");
                self.emit("    sete al");
                self.emit("    movzx rax, al");
                let offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
            }
            Inst::Lt { dst, lhs, rhs, ty } => {
                self.load_value_to_rax(lhs, ty);
                self.emit("    mov rcx, rax");
                self.load_value_to_rax(rhs, ty);
                self.emit("    cmp rcx, rax");
                self.emit("    setl al");
                self.emit("    movzx rax, al");
                let offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
            }
            Inst::Jump { target } => {
                self.emit(&format!("    jmp {}", target.0));
            }
            Inst::JumpIf { cond, target } => {
                self.load_value_to_rax(cond, &Type::Bool);
                self.emit("    cmp rax, 0");
                self.emit(&format!("    jne {}", target.0));
            }
            Inst::JumpIfFalse { cond, target } => {
                self.load_value_to_rax(cond, &Type::Bool);
                self.emit("    cmp rax, 0");
                self.emit(&format!("    je {}", target.0));
            }
            Inst::Label { target } => {
                self.emit(&format!("{}:", target.0));
            }
            Inst::Call { dst, func, args, ty } => {
                let param_regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

                for (i, arg) in args.iter().enumerate() {
                    if i < param_regs.len() {
                        self.load_value_to_rax(arg, ty);
                        self.emit(&format!("    mov {}, rax", param_regs[i]));
                    }
                    else {
                        self.load_value_to_rax(arg, ty);
                        self.emit("    push rax");
                    }
                }

                match func {
                    Value::Label(label) => {
                        self.emit(&format!("    call {}", label.0));
                    }
                    _ => {
                        self.load_value_to_rax(func, ty);
                        self.emit("    call rax");
                    }
                }

                if args.len() > param_regs.len() {
                    let stack_bytes = (args.len() - param_regs.len()) * 8;
                    self.emit(&format!("    add rsp, {}", stack_bytes));
                }

                if let Some(dst_reg) = dst {
                    let offset = self.get_or_alloc_reg_offset(dst_reg);
                    self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
                }
            }
            Inst::Return { val } => {
                if let Some(v) = val {
                    self.load_value_to_rax(v, &Type::I64);
                }
                else {
                    self.emit("    xor rax, rax");
                }

                self.emit("    leave");
                self.emit("    ret");
            }
            Inst::Alloc { dst, size, ty } => {
                self.load_value_to_rax(size, &Type::I64);

                self.emit("    push rax");
                self.emit("    mov rax, 12");
                self.emit("    xor rdi, rdi");
                self.emit("    syscall");
                self.emit("    mov rcx, rax");

                self.emit("    pop rdi");
                self.emit("    add rdi, rax");
                self.emit("    mov rax, 12");
                self.emit("    syscall");

                self.emit("    mov rax, rcx");
                let offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
            }
            Inst::Store { ptr, src, .. } => {
                self.load_value_to_rax(ptr, &Type::I64);
                self.emit("    mov rcx, rax");

                self.load_value_to_rax(src, &Type::I64);

                self.emit("    mov QWORD PTR [rcx], rax");
            }
            Inst::Load { dst, ptr, .. } => {
                self.load_value_to_rax(ptr, &Type::I64);

                self.emit("    mov rax, QWORD PTR [rax]");

                let offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
            }
            Inst::StructAlloc { dst, struct_name } => {
                let size = self.get_struct_size(struct_name);

                // Allocate to heap later
                let struct_offset = self.next_stack_offset;
                self.next_stack_offset += size as i32;

                self.emit(&format!("    lea rax, [rbp-{}]", struct_offset));
                let dst_offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
            }
            Inst::FieldStore { struct_reg, field_name, value, ty } => {
                let struct_offset = self.get_or_alloc_reg_offset(struct_reg);
                self.emit(&format!("    mov rcx, QWORD PTR [rbp-{}]", struct_offset));

                if let Value::Reg(reg) = value {
                    if let Some(ty) = self.reg_offsets.get(reg) {
                        // Simplification, need type tracking
                        if let Some(field_offset) = self.get_field_offset_without_struct(field_name) {
                            self.load_value_to_rax(value, &Type::I64);
                            self.emit(&format!("    mov QWORD PTR [rcx+{}], rax", field_offset));
                        }
                    }
                }
            }
            Inst::FieldLoad { dst, struct_reg, field_name, ty } => {
                let struct_offset = self.get_or_alloc_reg_offset(struct_reg);
                self.emit(&format!("    mov rcx, QWORD PTR [rbp-{}]", struct_offset));

                if let Some(field_offset) = self.get_field_offset_without_struct(field_name) {
                    self.emit(&format!("    mov rax, QWORD PTR [rcx+{}]", field_offset));
                    let dst_offset = self.get_or_alloc_reg_offset(dst);
                    self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
                }
            }
            // On stack for rn, move to heap later
            Inst::ArrayAlloc { dst, element_ty, size } => {
                let element_size = self.type_size(element_ty);
                let dst_offset = self.get_or_alloc_reg_offset(dst);
                self.load_value_to_rax(size, &Type::I64);
                self.emit(&format!("    imul rax, {}", element_size));
                let array_base = self.next_stack_offset + 256;
                self.emit(&format!("    sub rsp, 256"));
                self.emit(&format!("    lea rax, [rbp-{}]", array_base));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
            }
            Inst::IndexLoad { dst, array, index, element_ty } => {
                let element_size = self.type_size(element_ty);
                let array_offset = self.get_or_alloc_reg_offset(array);
                self.emit(&format!("    mov rcx, QWORD PTR [rbp-{}]", array_offset));
                self.load_value_to_rax(index, &Type::I64);
                self.emit(&format!("    imul rax, {}", element_size));
                self.emit("    add rcx, rax");

                match element_size {
                    1 => self.emit("    movzx rax, BYTE PTR [rcx]"),
                    2 => self.emit("    movzx rax, WORD PTR [rcx]"),
                    4 => self.emit("    mov eax, DWORD PTR [rcx]"),
                    8 => self.emit("    mov rax, QWORD PTR [rcx]"),
                    _ => self.emit("    mov rax, QWORD PTR [rcx]"),
                }

                let dst_offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
            }
            Inst::IndexStore { array, index, value, element_ty } => {
                let element_size = self.type_size(element_ty);
                let array_offset = self.get_or_alloc_reg_offset(array);
                self.emit(&format!("    mov rcx, QWORD PTR [rbp-{}]", array_offset));
                self.load_value_to_rax(index, &Type::I64);
                self.emit(&format!("    imul rax, {}", element_size));
                self.emit("    add rcx, rax");
                self.emit("    mov rdx, rcx");
                self.load_value_to_rax(value, element_ty);

                match element_size {
                    1 => self.emit("    mov BYTE PTR [rdx], al"),
                    2 => self.emit("    mov WORD PTR [rdx], ax"),
                    4 => self.emit("    mov DWORD PTR [rdx], eax"),
                    8 => self.emit("    mov QWORD PTR [rdx], rax"),
                    _ => self.emit("    mov QWORD PTR [rdx], rax")
                }
            }
            Inst::SliceCreate { dst, array, start, end, element_ty } => {
                let element_size = self.type_size(element_ty);
                let array_offset = self.get_or_alloc_reg_offset(array);
                self.emit(&format!("    mov rcx, QWORD PTR [rbp-{}]", array_offset));
                self.load_value_to_rax(start, &Type::I64);
                self.emit(&format!("    imul rax, {}", element_size));
                self.emit("    add rcx, rax");
                self.load_value_to_rax(end, &Type::I64);
                self.emit("    push rax");
                self.load_value_to_rax(start, &Type::I64);
                self.emit("    pop rdx");
                self.emit("    sub rdx, rax");
                let slice_offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rcx", slice_offset));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rdx", slice_offset + 8));
            }
            Inst::SliceLen { dst, slice } => {
                let slice_offset = self.get_or_alloc_reg_offset(slice);
                self.emit(&format!("    mov rax, QWORD PTR [rbp-{}]", slice_offset + 8));
                let dst_offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
            }
            Inst::SliceIndexLoad { dst, slice, index, element_ty } => {
                let element_size = self.type_size(element_ty);
                let slice_offset = self.get_or_alloc_reg_offset(slice);
                self.emit(&format!("    mov rcx, QWORD PTR [rbp-{}]", slice_offset));
                self.load_value_to_rax(index, &Type::I64);
                self.emit(&format!("    imul rax, {}", element_size));
                self.emit("    add rcx, rax");

                match element_size {
                    1 => self.emit("    movzx rax, BYTE PTR [rcx]"),
                    2 => self.emit("    movzx rax, WORD PTR [rcx]"),
                    4 => self.emit("    mov eax, DWORD PTR [rcx]"),
                    8 => self.emit("    mov rax, QWORD PTR [rcx]"),
                    _ => self.emit("    mov rax, QWORD PTR [rcx]"),
                }

                let dst_offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
            }
            Inst::SimPoint { id, metadata } => {
                self.emit(&format!("    # SimPoint: {}", id));
            }
            _ => {
                self.emit(&format!("    # TODO: {:?}", inst));
            }
        }
    }

    fn get_field_offset_without_struct(&self, field_name: &str) -> Option<usize> {
        for layout in self.struct_layouts.values() {
            for (fname, offset, _) in layout {
                if fname == field_name {
                    return Some(*offset);
                }
            }
        }

        None
    }

    fn load_value_to_rax(&mut self, val: &Value, ty: &Type) {
        match val {
            Value::Const(c) => {
                match c {
                    Const::I32(i) => {
                        self.emit(&format!("    mov eax, {}", i));

                        if *i < 0 {
                            self.emit("    cdqe")
                        }
                    }
                    Const::I64(i) => {
                        self.emit(&format!("    mov rax, {}", i));
                    }
                    _ => {
                        self.emit("    xor rax, rax")
                    }
                }
            }
            Value::Reg(r) => {
                if let Some(offset) = self.reg_offsets.get(r) {
                    self.emit(&format!("    mov rax, QWORD PTR [rbp-{}]", offset));
                }
                else {
                    self.emit("    xor rax, rax");
                }
            }
            _ => {
                self.emit("    xor rax, rax");
            }
        }
    }

    fn get_or_alloc_reg_offset(&mut self, reg: &Reg) -> i32 {
        if let Some(offset) = self.reg_offsets.get(reg) {
            *offset
        }
        else {
            let offset = self.next_stack_offset;
            self.next_stack_offset += 8;
            self.reg_offsets.insert(reg.clone(), offset);
            
            offset
        }
    }

    fn emit(&mut self, line: &str) {
        self.output.push(line.to_string());
    }
}