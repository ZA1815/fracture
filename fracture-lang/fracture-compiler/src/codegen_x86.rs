use fracture_ir::hsir::*;
use std::collections::HashMap;

pub struct X86CodeGen {
    output: Vec<String>,
    reg_offsets: HashMap<Reg, i32>,
    next_stack_offset: i32,
    current_function_stack_size: i32,
    program: Option<Program>,
    struct_layouts: HashMap<String, Vec<(String, usize, Type)>>,
    vec_layouts: HashMap<Reg, (i32, i32, i32)>,
    label_counter: usize,
    alloc_sizes: HashMap<Reg, i32>
}

impl X86CodeGen {
    pub fn new() -> Self {
        Self {
            output: Vec::new(),
            reg_offsets: HashMap::new(),
            next_stack_offset: 8,
            current_function_stack_size: 0,
            program: None,
            struct_layouts: HashMap::new(),
            vec_layouts: HashMap::new(),
            label_counter: 0,
            alloc_sizes: HashMap::new()
        }
    }

    fn next_label_id(&mut self) -> usize {
        let id = self.label_counter;
        self.label_counter += 1;

        id
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
            Type::String => 24,
            Type::Vec(_) => 24,
            Type::HashMap(_, _) => 32,
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

    fn bucket_size(&self, key_ty: &Type, value_ty: &Type) -> usize {
        let key_size = self.type_size(key_ty);
        let value_size = self.type_size(value_ty);

        let raw_size = 16 + key_size + value_size;

        (raw_size + 7) & !7
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

    fn emit_mmap_alloc(&mut self, dst: &Reg, size_val: &Value) {
        self.load_value_to_rax(size_val, &Type::I64);
        self.emit("    mov rsi, rax");
        self.emit("    xor rdi, rdi");
        self.emit("    mov rdx, 3");
        self.emit("    mov r10, 0x22");
        self.emit("    mov r8, -1");
        self.emit("    xor r9, r9");
        self.emit("    mov rax, 9");
        self.emit("    syscall");
        let dst_offset = self.get_or_alloc_reg_offset(dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
    }

    fn emit_munmap_free(&mut self, ptr_reg: &Reg, size: usize) {
        let ptr_offset = self.get_or_alloc_reg_offset(ptr_reg);
        self.emit(&format!("    mov rdi, QWORD PTR [rbp-{}]", ptr_offset));
        self.emit(&format!("    mov rsi, {}", size));
        self.emit("    mov rax, 11");
        self.emit("    syscall");
    }

    fn emit_vec_drop(&mut self, reg: &Reg) {
        let vec_offset = self.get_or_alloc_reg_offset(reg);
        self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", vec_offset));
        self.emit("    mov rdi, QWORD PTR [r12]");
        self.emit("    test rdi, rdi");
        let skip_label = format!("skip_vec_free_{}", self.next_label_id());
        self.emit(&format!("    jz {}", skip_label));
        // Assuming 8 byte elements, adapt later
        self.emit("    mov rsi, QWORD PTR [r12+16");
        self.emit("    shl rsi, 3");
        self.emit("    mov rax, 11");
        self.emit("    syscall");
        self.emit("    mov QWORD PTR [r12], 0");
        self.emit("    mov QWORD PTR [r12+8], 0");
        self.emit("    mov QWORD PTR [r12+16], 0");
        self.emit(&format!("{}:", skip_label));
    }

    fn emit_string_drop(&mut self, reg: &Reg) {
        let string_offset = self.get_or_alloc_reg_offset(reg);
        self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", string_offset));
        self.emit("    mov rdi, QWORD PTR [r12]");
        self.emit("    test rdi, rdi");
        let skip_label = format!("skip_str_free_{}", self.next_label_id());
        self.emit(&format!("    jz {}", skip_label));
        self.emit("    mov rsi, QWORD PTR [r12+16]");
        self.emit("    mov rax, 11");
        self.emit("    syscall");
        self.emit("    mov QWORD PTR [r12], 0");
        self.emit("    mov QWORD PTR [r12+8], 0");
        self.emit("    mov QWORD PTR [r12+16], 0");
        self.emit(&format!("{}:", skip_label));
    }

    fn emit_hashmap_drop(&mut self, reg: &Reg) {
        let map_offset = self.get_or_alloc_reg_offset(reg);
        self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", map_offset));
        self.emit("    mov rdi, QWORD PTR [r12]");
        self.emit("    test rdi, rdi");
        let skip_label = format!("skip_map_free_{}", self.next_label_id());
        self.emit(&format!("    jz {}", skip_label));
        self.emit("    mov rsi, QWORD PTR [r12+16]");
        self.emit("    shl rsi, 5");
        self.emit("    mov rax, 11");
        self.emit("    syscall");
        self.emit("    mov QWORD PTR [r12], 0");
        self.emit("    mov QWORD PTR [r12+8], 0");
        self.emit("    mov QWORD PTR [r12+16], 0");
        self.emit("    mov QWORD PTR [r12+24], 0");
        self.emit(&format!("{}:", skip_label));
    }

    fn emit_struct_drop(&mut self, reg: &Reg, struct_name: &str) {
        // Placeholder as structs are currently on stack
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
            Inst::HeapAlloc { dst, size } => {
                self.load_value_to_rax(size, &Type::I64);
                self.emit("    push rax");
                self.emit("    mov rax, 12");
                self.emit("    xor rdi, rdi");
                self.emit("    syscall");
                self.emit("    mov rcx, rax");
                self.emit("    pop rdi");
                self.emit("    add rdi, rcx");
                self.emit("    mov rax, 12");
                self.emit("    syscall");
                self.emit("    mov rax, rcx");
                let offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
            }
            // Simple impl, have to improve later
            Inst::HeapRealloc { dst, ptr, old_size, new_size } => {
                self.load_value_to_rax(old_size, &Type::I64);
                self.emit("    push rax");
                self.load_value_to_rax(ptr, &Type::I64);
                self.emit("    push rax");
                self.emit("    mov rax, 12");
                self.emit("    xor rdi, rdi");
                self.emit("    syscall");
                self.emit("    mov r12, rax");
                self.load_value_to_rax(new_size, &Type::I64);
                self.emit("    add rax, r12");
                self.emit("    mov rdi, rax");
                self.emit("    mov rax, 12");
                self.emit("    syscall");
                self.emit("    pop rsi");
                self.emit("    pop rcx");
                self.emit("    mov rdi, r12");
                self.emit("    rep movsb");
                self.emit("    mov rax, r12");
                let dst_offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
            }
            Inst::HeapFree { ptr } => {
                // Placeholder
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
            Inst::TupleAlloc { dst, element_types } => {
                let total_size: usize = element_types.iter()
                    .map(|ty| self.type_size(ty))
                    .sum();
                let dst_offset = self.get_or_alloc_reg_offset(dst);
                let tuple_base = self.next_stack_offset + total_size as i32;
                self.next_stack_offset = tuple_base + 8;
                self.emit(&format!("    lea rax, [rbp-{}]", tuple_base));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
            }
            Inst::TupleLoad { dst, tuple_reg, index, ty } => {
                let tuple_offset = self.get_or_alloc_reg_offset(tuple_reg);
                self.emit(&format!("    mov rcx, QWORD PTR [rbp-{}]", tuple_offset));
                let element_size = self.type_size(ty);
                let field_offset = index * element_size;
                match element_size {
                    4 => self.emit(&format!("    mov eax, DWORD PTR [rcx+{}]", field_offset)),
                    _ => self.emit(&format!("    mov rax, QWORD PTR [rcx+{}]", field_offset)),
                }
                let dst_offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
            }
            Inst::TupleStore { tuple_reg, index, value, ty } => {
                let tuple_offset = self.get_or_alloc_reg_offset(tuple_reg);
                self.emit(&format!("    mov rcx, QWORD PTR [rbp-{}]", tuple_offset));
                let element_size = self.type_size(ty);
                let field_offset = index * element_size;
                self.load_value_to_rax(value, ty);
                match element_size {
                    4 => self.emit(&format!("   mov DWORD PTR [rcx+{}], eax", field_offset)),
                    _ => self.emit(&format!("   mov QWORD PTR [rcx+{}], rax", field_offset))
                }
            }
            Inst::StringAlloc { dst, data } => {
                let dst_offset = self.get_or_alloc_reg_offset(dst);
                let string_offset = self.next_stack_offset + 24;
                self.next_stack_offset = string_offset + 24;
                let len = data.len();
                let cap = len.max(16);
                self.emit("    xor rdi, rdi");
                self.emit(&format!("    mov rsi, {}", cap));
                self.emit("    mov rdx, 3");
                self.emit("    mov r10, 0x22");
                self.emit("    mov r8, -1");
                self.emit("    xor r9, r9");
                self.emit("    mov rax, 9");
                self.emit("    syscall");
                self.emit("    mov rdi, rax");
                for (i, byte) in data.bytes().enumerate() {
                    self.emit(&format!("    mov BYTE PTR [rdi+{}], {}", i, byte));
                }
                let string_base = string_offset + 16;
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rdi", string_base));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], {}", string_base - 8, len));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], {}", string_base - 16, cap));
                self.emit(&format!("    lea rax, [rbp-{}]", string_base));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
            }
            Inst::StringLen { dst, string } => {
                let string_offset = self.get_or_alloc_reg_offset(string);
                self.emit(&format!("    mov rcx, QWORD PTR [rbp-{}]", string_offset));
                self.emit("    mov rax, QWORD PTR [rcx+8]");
                let dst_offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
            }
            Inst::StringConcat { dst, left, right } => {
                let left_offset = self.get_or_alloc_reg_offset(left);
                self.emit(&format!("    mov r8, QWORD PTR [rbp-{}]", left_offset));
                self.emit("    mov r9, QWORD PTR [r8]");
                self.emit("    mov r10, QWORD PTR [r8+8]");
                let right_offset = self.get_or_alloc_reg_offset(right);
                self.emit(&format!("    mov r11, QWORD PTR [rbp-{}]", right_offset));
                self.emit("    mov r12, QWORD PTR [r11]");
                self.emit("    mov r13, QWORD PTR [r11+8]");
                self.emit("    mov rax, r10");
                self.emit("    add rax, r13");
                self.emit("    mov r14, rax");
                self.emit("    add rax, 16");
                self.emit("    push rax");
                self.emit("    mov rax, 12");
                self.emit("    xor rdi, rdi");
                self.emit("    syscall");
                self.emit("    mov r15, rax");
                self.emit("    pop rdi");
                self.emit("    add rdi, r15");
                self.emit("    mov rax, 12");
                self.emit("    syscall");
                self.emit("    mov rsi, r9");
                self.emit("    mov rdi, r15");
                self.emit("    mov rcx, r10");
                self.emit("    rep movsb");
                self.emit("    mov rsi, r12");
                self.emit("    mov rdi, r15");
                self.emit("    add rdi, r10");
                self.emit("    mov rcx, r13");
                self.emit("    rep movsb");
                let dst_offset = self.get_or_alloc_reg_offset(dst);
                let result_offset = self.next_stack_offset + 24;
                self.next_stack_offset = result_offset + 24;
                self.emit(&format!("    mov QWORD PTR [rbp-{}], r15", result_offset));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], r14", result_offset + 8));
                self.emit("    mov rax, r14");
                self.emit("    add rax, 16");
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", result_offset + 16));
                self.emit(&format!("    lea rax, [rbp-{}]", result_offset));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
            }
            Inst::StringPush { string, value } => {
                // Placeholder
            }
            Inst::StringIndex { dst, string, index } => {
                let string_offset = self.get_or_alloc_reg_offset(string);
                self.emit(&format!("    mov rcx, QWORD PTR [rbp-{}]", string_offset));
                self.emit("    mov rcx, QWORD PTR [rcx]");
                self.load_value_to_rax(index, &Type::I64);
                self.emit("    add rcx, rax");
                self.emit("    movzx rax, BYTE PTR [rcx]");
                let dst_offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
            }
            Inst::VecAlloc { dst, element_ty, initial_cap } => {
                let element_size = self.type_size(element_ty);
                let dst_offset = self.get_or_alloc_reg_offset(dst);
                let vec_meta_offset = self.next_stack_offset + 24;
                self.next_stack_offset = vec_meta_offset + 8;
                self.load_value_to_rax(initial_cap, &Type::I64);
                self.emit("    push rax");
                self.emit(&format!("    imul rax, {}", element_size));
                self.emit("    push rax");
                self.emit("    mov rax, 12");
                self.emit("    xor rdi, rdi");
                self.emit("    syscall");
                self.emit("    mov r12, rax");
                self.emit("    pop rdi");
                self.emit("    add rdi, r12");
                self.emit("    mov rax, 12");
                self.emit("    syscall");
                self.emit(&format!("    mov QWORD PTR [rbp-{}], r12", vec_meta_offset));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], 0", vec_meta_offset - 8));
                self.emit("    pop rax");
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", vec_meta_offset - 16));
                self.emit(&format!("    lea rax, [rbp-{}]", vec_meta_offset));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
                self.vec_layouts.insert(dst.clone(), (vec_meta_offset, vec_meta_offset - 8, vec_meta_offset - 16)); 
            }
            Inst::VecPush { vec, value, element_ty } => {
                let element_size = self.type_size(element_ty);
                let vec_offset = self.get_or_alloc_reg_offset(vec);
                let id = self.next_label_id();
                self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", vec_offset));
                self.emit(&format!("    mov r13, QWORD PTR [r12+{}]", 8));
                self.emit(&format!("    mov r14, QWORD PTR [r12+{}]", 16));
                self.emit("    cmp r13, r14");
                let no_grow_label = format!("vec_no_grow_{}", id);
                let after_grow_label = format!("vec_after_grow_{}", id);
                let double_cap_label = format!("vec_double_cap_{}", id);
                let do_realloc_label = format!("vec_do_realloc_{}", id);
                self.emit(&format!("    jl {}", no_grow_label));
                self.emit("    mov rax, r14");
                self.emit("    test rax, rax");
                self.emit(&format!("    jnz {}", double_cap_label));
                self.emit("    mov rax, 8");
                self.emit(&format!("    jmp {}", do_realloc_label));
                self.emit(&format!("{}:", double_cap_label));
                self.emit("    shl rax, 1");
                self.emit(&format!("{}:", do_realloc_label));
                self.emit("    push rax");
                self.emit("    mov rdi, r13");
                self.emit(&format!("    imul rdi, {}", element_size));
                self.emit("    mov rsi, rax");
                self.emit(&format!("    imul rsi, {}", element_size));
                self.emit("    push rdi");
                self.emit("    mov rax, 12");
                self.emit("    xor rdi, rdi");
                self.emit("    syscall");
                self.emit("    mov r15, rax");
                self.emit("    mov rdi, r15");
                self.emit("    add rdi, rsi");
                self.emit("    mov rax, 12");
                self.emit("    syscall");
                self.emit("    mov rdi, r15");
                self.emit("    mov rsi, QWORD PTR [r12]");
                self.emit("    pop rcx");
                self.emit("    rep movsb");
                self.emit("    mov QWORD PTR [r12], r15");
                self.emit("    pop rax");
                self.emit(&format!("    mov QWORD PTR [r12+{}], rax", 16));
                self.emit(&format!("    jmp {}", after_grow_label));
                self.emit(&format!("{}:", no_grow_label));
                self.emit(&format!("{}:", after_grow_label));
                self.emit("    mov r15, QWORD PTR [r12]");
                self.emit(&format!("    mov r13, QWORD PTR [r12+{}]", 8));
                self.emit("    mov rax, r13");
                self.emit(&format!("    imul rax, {}", element_size));
                self.emit("    add r15, rax");
                self.load_value_to_rax(value, element_ty);
                match element_size {
                    1 => self.emit("    mov BYTE PTR [r15], al"),
                    2 => self.emit("    mov WORD PTR [r15], ax"),
                    4 => self.emit("    mov DWORD PTR [r15], eax"),
                    _ => self.emit("    mov QWORD PTR [r15], rax"),
                }
                self.emit("    inc r13");
                self.emit(&format!("    mov QWORD PTR [r12+{}], r13", 8));
            }
            Inst::VecPop { dst, vec, element_ty } => {
                let element_size = self.type_size(element_ty);
                let vec_offset = self.get_or_alloc_reg_offset(vec);
                let dst_offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", vec_offset));
                self.emit(&format!("    mov rax, QWORD PTR [r12+{}]", 8));
                self.emit("    dec rax");
                self.emit(&format!("    mov QWORD PTR [r12+{}], rax", 8));
                self.emit("    mov rcx, QWORD PTR [r12]");
                self.emit(&format!("    imul rax, {}", element_size));
                self.emit("    add rcx, rax");
                match element_size {
                    1 => self.emit("    movzx rax, BYTE PTR [rcx]"),
                    2 => self.emit("    movzx rax, WORD PTR [rcx]"),
                    4 => self.emit("    mov eax, DWORD PTR [rcx]"),
                    _ => self.emit("    mov rax, QWORD PTR [rcx]"),
                }
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
            }
            Inst::VecGet { dst, vec, index, element_ty } => {
                let element_size = self.type_size(element_ty);
                let vec_offset = self.get_or_alloc_reg_offset(vec);
                let dst_offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", vec_offset));
                self.emit("    mov rcx, QWORD PTR [r12]");
                self.load_value_to_rax(index, &Type::I64);
                self.emit(&format!("    imul rax, {}", element_size));
                self.emit("    add rcx, rax");
                match element_size {
                    1 => self.emit("    movzx rax, BYTE PTR [rcx]"),
                    2 => self.emit("    movzx rax, WORD PTR [rcx]"),
                    4 => self.emit("    mov eax, DWORD PTR [rcx]"),
                    _ => self.emit("    mov rax, QWORD PTR [rcx]"),
                }
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
            }
            Inst::VecSet { vec, index, value, element_ty } => {
                let element_size = self.type_size(element_ty);
                let vec_offset = self.get_or_alloc_reg_offset(vec);
                self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", vec_offset));
                self.emit("    mov rcx, QWORD PTR [r12]");
                self.load_value_to_rax(index, &Type::I64);
                self.emit(&format!("    imul rax, {}", element_size));
                self.emit("    add rcx, rax");
                self.emit("    mov rdx, rcx");
                self.load_value_to_rax(value, element_ty);
                match element_size {
                    1 => self.emit("    mov BYTE PTR [rdx], al"),
                    2 => self.emit("    mov WORD PTR [rdx], ax"),
                    4 => self.emit("    mov DWORD PTR [rdx], eax"),
                    _ => self.emit("    mov QWORD PTR [rdx], rax")
                }
            }
            Inst::VecLen { dst, vec } => {
                let vec_offset = self.get_or_alloc_reg_offset(vec);
                let dst_offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", vec_offset));
                self.emit(&format!("    mov rax, QWORD PTR [r12+{}]", 8));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
            }
            Inst::VecCap { dst, vec } => {
                let vec_offset = self.get_or_alloc_reg_offset(vec);
                let dst_offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", vec_offset));
                self.emit(&format!("    mov rax, QWORD PTR [r12+{}]", 16));
                self.emit(&format!("    mov QWORD PTR [rbp-{}]", dst_offset));
            }
            Inst::HashMapAlloc { dst, key_ty, value_ty, initial_cap } => {
                let bucket_size = self.bucket_size(key_ty, value_ty);
                let dst_offset = self.get_or_alloc_reg_offset(dst);
                let meta_offset = self.next_stack_offset + 32;
                self.next_stack_offset = meta_offset + 8;
                self.load_value_to_rax(initial_cap, &Type::I64);
                self.emit("    mov r14, rax");
                self.emit(&format!("    imul rax, {}", bucket_size));
                self.emit("    mov r13, rax");
                self.emit("    mov rax, 12");
                self.emit("    xor rdi, rdi");
                self.emit("    syscall");
                self.emit("    mov r12, rax");
                self.emit("    mov rdi, r12");
                self.emit("    add rdi, r13");
                self.emit("    mov rax, 12");
                self.emit("    syscall");
                self.emit("    mov rdi, r12");
                self.emit("    xor rax, rax");
                self.emit("    mov rcx, r13");
                self.emit("    rep stosb");
                self.emit(&format!("    mov QWORD PTR [rbp-{}], r12", meta_offset));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], 0", meta_offset - 8));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], r14", meta_offset - 16));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], 0", meta_offset - 24));
                self.emit(&format!("    lea rax, [rbp-{}]", meta_offset));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
            }
            Inst::HashMapInsert { map, key, value, key_ty, value_ty } => {
                let bucket_size = self.bucket_size(key_ty, value_ty);
                let key_size = self.type_size(key_ty);
                let value_size = self.type_size(value_ty);
                let map_offset = self.get_or_alloc_reg_offset(map);
                let id = self.next_label_id();
                self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", map_offset));
                self.emit("    mov r13, QWORD PTR [r12]");
                self.emit(&format!("    mov r14, QWORD PTR [r12+{}]", 16));
                // Add proper hashing using FNV-1a or SipHash
                self.load_value_to_rax(key, key_ty);
                self.emit("    mov r15, rax");
                self.emit("    mov rax, r15");
                self.emit("    mov rcx, 0x9E3779B97F4A7C15");
                self.emit("    imul rax, rcx");
                self.emit("    shr rax, 32");
                self.emit("    mov r15, rax");
                self.emit("    xor rdx, rdx");
                self.emit("    div r14");
                self.emit("    mov r8, rdx");
                let probe_loop = format!("hashmap_insert_probe_{}", id);
                let found_empty = format!("hashmap_insert_empty_{}", id);
                let found_match = format!("hashmap_insert_match_{}", id);
                let insert_done = format!("hashmap_insert_done_{}", id);
                let check_resize = format!("hashmap_insert_check_resize_{}", id);
                self.emit(&format!("{}:", probe_loop));
                self.emit("    mov rax, r8");
                self.emit(&format!("    imul rax, {}", bucket_size));
                self.emit("    lea rcx, [r13+rax]");
                self.emit(&format!("    movzx rax, BYTE PTR [rcx+{}]", 8));
                self.emit(&format!("    cmp rax, {}", 1));
                self.emit(&format!("    jne {}", found_empty));
                self.emit(&format!("    mov rax, QWORD PTR [rcx]"));
                self.emit("    cmp rax, r15");
                self.emit(&format!("    jne {}_next_bucket", probe_loop));
                self.load_value_to_rax(key, key_ty);
                match key_size {
                    1 => self.emit(&format!("    cmp al, BYTE PTR [rcx+{}]", 16)),
                    2 => self.emit(&format!("    cmp ax, WORD PTR [rcx+{}]", 16)),
                    4 => self.emit(&format!("    cmp eax, DWORD PTR [rcx+{}]", 16)),
                    _ => self.emit(&format!("    cmp rax, QWORD PTR [rcx+{}]", 16)),
                }
                self.emit(&format!("    je {}", found_match));
                self.emit(&format!("{}_next_bucket:", probe_loop));
                self.emit("    inc r8");
                self.emit("    cmp r8, r14");
                self.emit(&format!("    jl {}", probe_loop));
                self.emit("    xor r8, r8");
                self.emit(&format!("    jmp {}", probe_loop));
                self.emit(&format!("{}:", found_empty));
                self.emit(&format!("    movzx rax, BYTE PTR [rcx+{}]", 8));
                self.emit(&format!("    cmp rax, {}", 2));
                self.emit(&format!("    jne {}_not_tombstone", found_empty));
                self.emit(&format!("    dec QWORD PTR [r12+{}]", 24));
                self.emit(&format!("{}_not_tombstone:", found_empty));
                self.emit("    mov QWORD PTR [rcx], r15");
                self.load_value_to_rax(key, key_ty);
                match key_size {
                    1 => self.emit(&format!("    mov BYTE PTR [rcx+{}], al", 16)),
                    2 => self.emit(&format!("    mov WORD PTR [rcx+{}], ax", 16)),
                    4 => self.emit(&format!("    mov DWORD PTR [rcx+{}], eax", 16)),
                    _ => self.emit(&format!("    mov QWORD PTR [rcx+{}], rax", 16)),
                }
                self.load_value_to_rax(value, value_ty);
                match value_size {
                    1 => self.emit(&format!("    mov BYTE PTR [rcx+{}], al", 16 + key_size)),
                    2 => self.emit(&format!("    mov WORD PTR [rcx+{}], ax", 16 + key_size)),
                    4 => self.emit(&format!("    mov DWORD PTR [rcx+{}], eax", 16 + key_size)),
                    _ => self.emit(&format!("    mov QWORD PTR [rcx+{}], rax", 16 + key_size)),
                }
                self.emit(&format!("    mov BYTE PTR [rcx+{}], {}", 8, 1));
                self.emit(&format!("    inc QWORD PTR [r12+{}]", 8));
                self.emit(&format!("    jmp {}", check_resize));
                self.emit(&format!("{}:", found_match));
                self.load_value_to_rax(value, value_ty);
                match value_size {
                    1 => self.emit(&format!("    mov BYTE PTR [rcx+{}], al", 16 + key_size)),
                    2 => self.emit(&format!("    mov WORD PTR [rcx+{}], ax", 16 + key_size)),
                    4 => self.emit(&format!("    mov DWORD PTR [rcx+{}], eax", 16 + key_size)),
                    _ => self.emit(&format!("    mov QWORD PTR [rcx+{}], rax", 16 + key_size)),
                }
                self.emit(&format!("    jmp {}", insert_done));
                self.emit(&format!("{}:", check_resize));
                self.emit(&format!("    mov rax, QWORD PTR [r12+{}]", 8));
                self.emit(&format!("    add rax, QWORD PTR [r12+{}]", 24));
                self.emit("    shl rax, 2");
                self.emit("    mov rcx, r14");
                self.emit("    imul rcx, 3");
                self.emit("    cmp rax, rcx");
                self.emit(&format!("    jle {}", insert_done));
                // Need to implement resizing
                self.emit(&format!("{}:", insert_done));
            }
            Inst::HashMapGet { dst, found_dst, map, key, key_ty, value_ty } => {
                let bucket_size = self.bucket_size(key_ty, value_ty);
                let key_size = self.type_size(key_ty);
                let value_size = self.type_size(value_ty);
                let map_offset = self.get_or_alloc_reg_offset(map);
                let dst_offset = self.get_or_alloc_reg_offset(dst);
                let found_offset = self.get_or_alloc_reg_offset(found_dst);
                let id = self.next_label_id();
                self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", map_offset));
                self.emit(&format!("    mov r13, QWORD PTR [r12]"));
                self.emit(&format!("    mov r14, QWORD PTR [r12+{}]", 16));
                self.load_value_to_rax(key, key_ty);
                self.emit("    mov r15, rax");
                self.emit("    mov rcx, 0x9E3779B97F4A7C15");
                self.emit("    imul rax, rcx");
                self.emit("    shr rax, 32");
                self.emit("    mov r15, rax");
                self.emit("    xor rdx, rdx");
                self.emit("    div r14");
                self.emit("    mov r8, rdx");
                self.emit("    mov r9, r14");
                let probe_loop = format!("hashmap_get_probe_{}", id);
                let not_found = format!("hashmap_get_not_found_{}", id);
                let found = format!("hashmap_get_found_{}", id);
                let done = format!("hashmap_get_done_{}", id);
                self.emit(&format!("{}:", probe_loop));
                self.emit("    dec r9");
                self.emit(&format!("    jl {}", not_found));
                self.emit("    mov rax, r8");
                self.emit(&format!("    imul rax, {}", bucket_size));
                self.emit("    lea rcx, [r13+rax]");
                self.emit(&format!("    movzx rax, BYTE PTR [rcx+{}]", 8));
                self.emit(&format!("    cmp rax, {}", 0));
                self.emit(&format!("    je {}", not_found));
                self.emit(&format!("    cmp rax, {}", 2));
                self.emit(&format!("    je {}_next", probe_loop));
                self.emit(&format!("    mov rax, QWORD PTR [rcx]"));
                self.emit("    cmp rax, r15");
                self.emit(&format!("    jne {}_next", probe_loop));
                self.load_value_to_rax(key, key_ty);
                match key_size {
                    1 => self.emit(&format!("    cmp al, BYTE PTR [rcx+{}]", 16)),
                    2 => self.emit(&format!("    cmp ax, WORD PTR [rcx+{}]", 16)),
                    4 => self.emit(&format!("    cmp eax, DWORD PTR [rcx+{}]", 16)),
                    _ => self.emit(&format!("    cmp rax, QWORD PTR [rcx+{}]", 16)),
                }
                self.emit(&format!("    je {}", found));
                self.emit(&format!("{}_next:", probe_loop));
                self.emit("    inc r8");
                self.emit("    cmp r8, r14");
                self.emit(&format!("    jl {}", probe_loop));
                self.emit("    xor r8, r8");
                self.emit(&format!("    jmp {}", probe_loop));
                self.emit(&format!("{}:", not_found));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], 0", found_offset));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], 0", dst_offset));
                self.emit(&format!("    jmp {}", done));
                self.emit(&format!("{}:", found));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], 1", found_offset));
                match value_size {
                    1 => self.emit(&format!("    movzx rax, BYTE PTR [rcx+{}]", 16 + key_size)),
                    2 => self.emit(&format!("    movzx rax, WORD PTR [rcx+{}]", 16 + key_size)),
                    4 => self.emit(&format!("    mov eax, DWORD PTR [rcx+{}]", 16 + key_size)),
                    _ => self.emit(&format!("    mov rax, QWORD PTR [rcx+{}]", 16 + key_size)),
                }
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
                self.emit(&format!("{}:", done));
            }
            Inst::HashMapRemove { success_dst, map, key, key_ty, value_ty } => {
                let bucket_size = self.bucket_size(key_ty, value_ty);
                let key_size = self.type_size(key_ty);
                let map_offset = self.get_or_alloc_reg_offset(map);
                let success_offset = self.get_or_alloc_reg_offset(success_dst);
                let id = self.next_label_id();
                self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", map_offset));
                self.emit("    mov r13, QWORD PTR [r12]");
                self.emit(&format!("    mov r14, QWORD PTR [r12+{}]", 16));
                self.load_value_to_rax(key, key_ty);
                self.emit("    mov r15, rax");
                self.emit("    mov rcx, 0x9E3779B97F4A7C15");
                self.emit("    imul rax, rcx");
                self.emit("    shr rax, 32");
                self.emit("    mov r15, rax");
                self.emit("    xor rdx, rdx");
                self.emit("    div r14");
                self.emit("    mov r8, rdx");
                self.emit("    mov r9, r14");
                let probe_loop = format!("hashmap_remove_probe_{}", id);
                let not_found = format!("hashmap_remove_not_found_{}", id);
                let found = format!("hashmap_remove_found_{}", id);
                let done = format!("hashmap_remove_done_{}", id);
                self.emit(&format!("{}:", probe_loop));
                self.emit("    dec r9");
                self.emit(&format!("    jl {}", not_found));
                self.emit("    mov rax, r8");
                self.emit(&format!("    imul rax, {}", bucket_size));
                self.emit("    lea rcx, [r13+rax]");
                self.emit(&format!("    movzx rax, BYTE PTR [rcx+{}]", 8));
                self.emit(&format!("    cmp rax, {}", 0));
                self.emit(&format!("    je {}", not_found));
                self.emit(&format!("    cmp rax, {}", 2));
                self.emit(&format!("    je {}_next", probe_loop));
                self.emit(&format!("    mov rax, QWORD PTR [rcx]"));
                self.emit("    cmp rax, r15");
                self.emit(&format!("    jne {}_next", probe_loop));
                self.load_value_to_rax(key, key_ty);
                match key_size {
                    1 => self.emit(&format!("    cmp al, BYTE PTR [rcx+{}]", 16)),
                    2 => self.emit(&format!("    cmp ax, WORD PTR [rcx+{}]", 16)),
                    4 => self.emit(&format!("    cmp eax, DWORD PTR [rcx+{}]", 16)),
                    _ => self.emit(&format!("    cmp rax, QWORD PTR [rcx+{}]", 16)),
                }
                self.emit(&format!("    je {}", found));
                self.emit(&format!("{}_next:", probe_loop));
                self.emit("    inc r8");
                self.emit("    cmp r8, r14");
                self.emit(&format!("    jl {}", probe_loop));
                self.emit("    xor r8, r8");
                self.emit(&format!("    jmp {}", probe_loop));
                self.emit(&format!("{}:", not_found));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], 0", success_offset));
                self.emit(&format!("    jmp {}", done));
                self.emit(&format!("{}:", found));
                self.emit(&format!("    mov BYTE PTR [rcx+{}], {}", 8, 2));
                self.emit(&format!("    dec QWORD PTR [r12+{}]", 8));
                self.emit(&format!("    inc QWORD PTR [r12+{}]", 24));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], 1", success_offset));
                self.emit(&format!("{}:", done));
            }
            Inst::HashMapContains { dst, map, key, key_ty, value_ty } => {
                let bucket_size = self.bucket_size(key_ty, value_ty);
                let key_size = self.type_size(key_ty);
                let map_offset = self.get_or_alloc_reg_offset(map);
                let dst_offset = self.get_or_alloc_reg_offset(dst);
                let id = self.next_label_id();
                self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", map_offset));
                self.emit("    mov r13, QWORD PTR [r12]");
                self.emit(&format!("    mov r14, QWORD PTR [r12+{}]", 16));
                self.load_value_to_rax(key, key_ty);
                self.emit("    mov r15, rax");
                self.emit("    mov rcx, 0x9E3779B97F4A7C15");
                self.emit("    imul rax, rcx");
                self.emit("    shr rax, 32");
                self.emit("    mov r15, rax");
                self.emit("    xor rdx, rdx");
                self.emit("    div r14");
                self.emit("    mov r8, rdx");
                self.emit("    mov r9, r14");
                let probe_loop = format!("hashmap_contains_probe_{}", id);
                let not_found = format!("hashmap_contains_not_found_{}", id);
                let found = format!("hashmap_contains_found_{}", id);
                let done = format!("hashmap_contains_done_{}", id);
                self.emit(&format!("{}:", probe_loop));
                self.emit("    dec r9");
                self.emit(&format!("    jl {}", not_found));
                self.emit("    mov rax, r8");
                self.emit(&format!("    imul rax, {}", bucket_size));
                self.emit("    lea rcx, [r13+rax]");
                self.emit(&format!("    movzx rax, BYTE PTR [rcx+{}]", 8));
                self.emit(&format!("    cmp rax, {}", 0));
                self.emit(&format!("    je {}", not_found));
                self.emit(&format!("    cmp rax, {}", 2));
                self.emit(&format!("    je {}_next", probe_loop));
                self.emit(&format!("    mov rax, QWORD PTR [rcx]"));
                self.emit("    cmp rax, r15");
                self.emit(&format!("    jne {}_next", probe_loop));
                self.load_value_to_rax(key, key_ty);
                match key_size {
                    1 => self.emit(&format!("    cmp al, BYTE PTR [rcx+{}]", 16)),
                    2 => self.emit(&format!("    cmp ax, WORD PTR [rcx+{}]", 16)),
                    4 => self.emit(&format!("    cmp eax, DWORD PTR [rcx+{}]", 16)),
                    _ => self.emit(&format!("    cmp rax, QWORD PTR [rcx+{}]", 16)),
                }
                self.emit(&format!("    je {}", found));
                self.emit(&format!("{}_next:", probe_loop));
                self.emit("    inc r8");
                self.emit("    cmp r8, r14");
                self.emit(&format!("    jl {}", probe_loop));
                self.emit("    xor r8, r8");
                self.emit(&format!("    jmp {}", probe_loop));
                self.emit(&format!("{}:", not_found));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], 0", dst_offset));
                self.emit(&format!("    jmp {}", done));
                self.emit(&format!("{}:", found));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], 1", dst_offset));
                self.emit(&format!("{}:", done));
            }
            Inst::HashMapLen { dst, map } => {
                let map_offset = self.get_or_alloc_reg_offset(map);
                let dst_offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", map_offset));
                self.emit(&format!("    mov rax, QWORD PTR [r12+{}]", 8));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
            }
            Inst::HashMapCap { dst, map } => {
                let map_offset = self.get_or_alloc_reg_offset(map);
                let dst_offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", map_offset));
                self.emit(&format!("    mov rax, QWORD PTR [r12+{}]", 16));
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
            }
            Inst::HashMapClear { map } => {
                let map_offset = self.get_or_alloc_reg_offset(map);
                self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", map_offset));
                self.emit(&format!("    mov rdi, QWORD PTR [r12]"));
                self.emit(&format!("    mov rcx, QWORD PTR [r12+{}]", 16));
                // Have to fix this, very simple implementation, need to track bucket size or recalculate again
                self.emit("    shl rcx, 5");
                self.emit("    xor rax, rax");
                // Could use rep stosq later (research it)
                self.emit("    rep stosb");
                self.emit(&format!("    mov QWORD PTR [r12+{}], 0", 8));
                self.emit(&format!("    mov QWORD PTR [r12+{}], 0", 24));
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