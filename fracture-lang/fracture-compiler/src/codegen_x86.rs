use fracture_ir::hsir::*;
use std::{collections::HashMap, io::empty};

pub struct X86CodeGen {
    output: Vec<String>,
    reg_offsets: HashMap<Reg, i32>,
    next_stack_offset: i32,
    current_function_stack_size: i32,
    program: Option<Program>,
    struct_layouts: HashMap<String, Vec<(String, usize, Type)>>,
    vec_layouts: HashMap<Reg, (i32, i32, i32)>,
    label_counter: usize,
    alloc_sizes: HashMap<Reg, i32>,
    data_section: Vec<String>,
    data_label_counter: usize
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
            alloc_sizes: HashMap::new(),
            data_section: Vec::new(),
            data_label_counter: 0
        }
    }

    fn next_label_id(&mut self) -> usize {
        let id = self.label_counter;
        self.label_counter += 1;

        id
    }

    fn alloc_data_label(&mut self, content: &str) -> String {
        let label = format!("_str_data_{}", self.data_label_counter);
        self.data_label_counter += 1;

        let bytes: Vec<String> = content.bytes().map(|b| b.to_string()).collect();

        self.data_section.push(format!("{}:", label));
        if bytes.is_empty() {
            self.data_section.push("    .byte 0".to_string());
        }
        else {
            self.data_section.push(format!("    .byte {}", bytes.join(", ")));
        }

        self.data_section.push(format!("{}_len = {} - {}", label, content.len(), 0));

        label
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

        if !self.data_section.is_empty() {
            self.emit("");
            self.emit(".section .data");
            self.emit("_newline:");
            self.emit("    .byte 10");
            self.emit("_newline_len = 1");

            for line in &self.data_section.clone() {
                self.emit(line);
            }
        }
        else {
            self.emit("");
            self.emit(".section .data");
            self.emit("_newline:");
            self.emit("    .byte 10");
            self.emit("_newline_len = 1");
        }

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
                self.compile_move(dst, src, ty);
            }
            Inst::Add { dst, lhs, rhs, ty } => {
                self.compile_add(dst, lhs, rhs, ty);
            }
            Inst::Sub { dst, lhs, rhs, ty } => {
                self.compile_sub(dst, lhs, rhs, ty);
            }
            Inst::Mul { dst, lhs, rhs, ty } => {
                self.compile_mul(dst, lhs, rhs, ty);
            }
            Inst::Div { dst, lhs, rhs, ty } => {
                self.compile_div(dst, lhs, rhs, ty);
            }
            Inst::Eq { dst, lhs, rhs, ty } => {
                self.compile_eq(dst, lhs, rhs, ty);
            }
            Inst::Lt { dst, lhs, rhs, ty } => {
                self.compile_lt(dst, lhs, rhs, ty);
            }
            Inst::Jump { target } => {
                self.compile_jump(target);
            }
            Inst::JumpIf { cond, target } => {
                self.compile_jump_if(cond, target);
            }
            Inst::JumpIfFalse { cond, target } => {
                self.compile_jump_if_false(cond, target);
            }
            Inst::Label { target } => {
                self.compile_label(target);
            }
            Inst::Call { dst, func, args, ty } => {
                self.compile_call(dst, func, args, ty);
            }
            Inst::Return { val } => {
                self.compile_return(val);
            }
            Inst::Alloc { dst, size, ty } => {
                self.compile_alloc(dst, size);
            }
            Inst::HeapAlloc { dst, size } => {
                self.compile_heap_alloc(dst, size);
            }
            // Simple impl, have to improve later
            Inst::HeapRealloc { dst, ptr, old_size, new_size } => {
                self.compile_heap_realloc(dst, ptr, old_size, new_size);
            }
            Inst::HeapFree { ptr } => {
                // Placeholder
            }
            Inst::Store { ptr, src, .. } => {
                self.compile_store(ptr, src);
            }
            Inst::Load { dst, ptr, .. } => {
                self.compile_load(dst, ptr);
            }
            Inst::StructAlloc { dst, struct_name } => {
                self.compile_struct_alloc(dst, struct_name);
            }
            Inst::FieldStore { struct_reg, field_name, value, ty } => {
                self.compile_field_store(struct_reg, field_name, value, ty);
            }
            Inst::FieldLoad { dst, struct_reg, field_name, ty } => {
                self.compile_field_load(dst, struct_reg, field_name, ty);
            }
            // On stack for rn, move to heap later
            Inst::ArrayAlloc { dst, element_ty, size } => {
                self.compile_array_alloc(dst, element_ty, size);
            }
            Inst::IndexLoad { dst, array, index, element_ty } => {
                self.compile_index_load(dst, array, index, element_ty);
            }
            Inst::IndexStore { array, index, value, element_ty } => {
                self.compile_index_store(array, index, value, element_ty);
            }
            Inst::SliceCreate { dst, array, start, end, element_ty } => {
                self.compile_slice_create(dst, array, start, end, element_ty);
            }
            Inst::SliceLen { dst, slice } => {
                self.compile_slice_len(dst, slice);
            }
            Inst::SliceIndexLoad { dst, slice, index, element_ty } => {
                self.compile_slice_index_load(dst, slice, index, element_ty);
            }
            Inst::TupleAlloc { dst, element_types } => {
                self.compile_tuple_alloc(dst, element_types);
            }
            Inst::TupleLoad { dst, tuple_reg, index, ty } => {
                self.compile_tuple_load(dst, tuple_reg, index, ty);
            }
            Inst::TupleStore { tuple_reg, index, value, ty } => {
                self.compile_tuple_store(tuple_reg, index, value, ty);
            }
            Inst::StringAlloc { dst, data } => {
                self.compile_string_alloc(dst, data);
            }
            Inst::StringLen { dst, string } => {
                self.compile_string_len(dst, string);
            }
            Inst::StringConcat { dst, left, right } => {
                self.compile_string_concat(dst, left, right);
            }
            Inst::StringPush { string, value } => {
                // Placeholder
            }
            Inst::StringIndex { dst, string, index } => {
                self.compile_string_index(dst, string, index);
            }
            Inst::VecAlloc { dst, element_ty, initial_cap } => {
                self.compile_vec_alloc(dst, element_ty, initial_cap);
            }
            Inst::VecPush { vec, value, element_ty } => {
                self.compile_vec_push(vec, value, element_ty);
            }
            Inst::VecPop { dst, vec, element_ty } => {
                self.compile_vec_pop(dst, vec, element_ty);
            }
            Inst::VecGet { dst, vec, index, element_ty } => {
                self.compile_vec_get(dst, vec, index, element_ty);
            }
            Inst::VecSet { vec, index, value, element_ty } => {
                self.compile_vec_set(vec, index, value, element_ty);
            }
            Inst::VecLen { dst, vec } => {
                self.compile_vec_len(dst, vec);
            }
            Inst::VecCap { dst, vec } => {
                self.compile_vec_cap(dst, vec);
            }
            Inst::HashMapAlloc { dst, key_ty, value_ty, initial_cap } => {
                self.compile_hashmap_alloc(dst, key_ty, value_ty, initial_cap);
            }
            Inst::HashMapInsert { map, key, value, key_ty, value_ty } => {
                self.compile_hashmap_insert(map, key, value, key_ty, value_ty);
            }
            Inst::HashMapGet { dst, found_dst, map, key, key_ty, value_ty } => {
                self.compile_hashmap_get(dst, found_dst, map, key, key_ty, value_ty);
            }
            Inst::HashMapRemove { success_dst, map, key, key_ty, value_ty } => {
                self.compile_hashmap_remove(success_dst, map, key, key_ty, value_ty);
            }
            Inst::HashMapContains { dst, map, key, key_ty, value_ty } => {
                self.compile_hashmap_contains(dst, map, key, key_ty, value_ty);
            }
            Inst::HashMapLen { dst, map } => {
                self.compile_hashmap_len(dst, map);
            }
            Inst::HashMapCap { dst, map } => {
                self.compile_hashmap_cap(dst, map);
            }
            Inst::HashMapClear { map } => {
                self.compile_hashmap_clear(map);
            }
            Inst::SysWrite { fd, buf, len, result_dst } => {
                self.compile_sys_write(fd, buf, len, result_dst);
            }
            Inst::SysRead { fd, buf, len, result_dst } => {
                self.compile_sys_read(fd, buf, len, result_dst);
            }
            Inst::SysOpen { path, flags, mode, result_dst } => {
                self.compile_sys_open(path, flags, mode, result_dst);
            }
            Inst::SysClose { fd, result_dst } => {
                self.compile_sys_close(fd, result_dst);
            }
            Inst::Print { value } => {
                self.compile_print(value);
            }
            Inst::Println { value } => {
                self.compile_println(value);
            }
            Inst::Eprint { value } => {
                self.compile_eprint(value);
            }
            Inst::Eprintln { value } => {
                self.compile_eprintln(value);
            }
            // Allocating fixed size rn (4096), but we should buffer and parse lines later
            Inst::ReadLine { dst } => {
                self.compile_read_line(dst);
            }
            Inst::IntToString { dst, value } => {
                self.compile_int_to_string(dst, value);
            }
            Inst::SysSeek { fd, offset, whence, result_dst } => {
                self.compile_sys_seek(fd, offset, whence, result_dst);
            }
            Inst::SysStat { path, stat_buf, result_dst } => {
                self.compile_sys_stat(path, stat_buf, result_dst);
            }
            Inst::SysFstat { fd, stat_buf, result_dst } => {
                self.compile_sys_fstat(fd, stat_buf, result_dst);
            }
            Inst::SysMkdir { path, mode, result_dst } => {
                self.compile_sys_mkdir(path, mode, result_dst);
            }
            Inst::SysRmdir { path, result_dst } => {
                self.compile_sys_rmdir(path, result_dst);
            }
            Inst::SysUnlink { path, result_dst } => {
                self.compile_sys_unlink(path, result_dst);
            }
            Inst::SysRename { old_path, new_path, result_dst } => {
                self.compile_sys_rename(old_path, new_path, result_dst);
            }
            Inst::SysAccess { path, mode, result_dst } => {
                self.compile_sys_access(path, mode, result_dst);
            }
            Inst::SysGetcwd { dst } => {
                self.compile_sys_getcwd(dst);
            }
            Inst::SysChdir { path, result_dst } => {
                self.compile_sys_chdir(path, result_dst);
            }
            Inst::FileReadToString { dst, path, result_dst } => {
                self.compile_file_read_to_string(dst, path, result_dst);
            }
            Inst::FileWriteString { path, content, result_dst } => {
                self.compile_file_write_string(path, content, result_dst);
            }
            Inst::FileAppendString { path, content, result_dst } => {
                self.compile_file_append_string(path, content, result_dst);
            }
            Inst::SimPoint { id, metadata } => {
                self.emit(&format!("    # SimPoint: {}", id));
            }
            _ => {
                self.emit(&format!("    # TODO: {:?}", inst));
            }
        }
    }

    fn compile_move(&mut self, dst: &Reg, src: &Value, ty: &Type) {
        self.load_value_to_rax(src, ty);
        let offset = self.get_or_alloc_reg_offset(dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
    }

    fn compile_add(&mut self, dst: &Reg, lhs: &Value, rhs: &Value, ty: &Type) {
        self.load_value_to_rax(lhs, ty);
        self.emit("    mov rcx, rax");
        self.load_value_to_rax(rhs, ty);
        self.emit("    add rax, rcx");
        let offset = self.get_or_alloc_reg_offset(dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
    }

    fn compile_sub(&mut self, dst: &Reg, lhs: &Value, rhs: &Value, ty: &Type) {
        self.load_value_to_rax(lhs, ty);
        self.emit("    mov rcx, rax");
        self.load_value_to_rax(rhs, ty);
        self.emit("    sub rcx, rax");
        self.emit("    mov rax, rcx");
        let offset = self.get_or_alloc_reg_offset(dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
    }

    fn compile_mul(&mut self, dst: &Reg, lhs: &Value, rhs: &Value, ty: &Type) {
        self.load_value_to_rax(lhs, ty);
        self.emit("    mov rcx, rax");
        self.load_value_to_rax(rhs, ty);
        self.emit("    imul rax, rcx");
        let offset = self.get_or_alloc_reg_offset(dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
    }

    fn compile_div(&mut self, dst: &Reg, lhs: &Value, rhs: &Value, ty: &Type) {
        self.load_value_to_rax(rhs, ty);
        self.emit("    mov rcx, rax");
        self.load_value_to_rax(lhs, ty);
        self.emit("    xor rdx, rdx");
        self.emit("    idiv rcx");
        let offset = self.get_or_alloc_reg_offset(dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
    }

    fn compile_eq(&mut self, dst: &Reg, lhs: &Value, rhs: &Value, ty: &Type) {
        self.load_value_to_rax(lhs, ty);
        self.emit("    mov rcx, rax");
        self.load_value_to_rax(rhs, ty);
        self.emit("    cmp rcx, rax");
        self.emit("    sete al");
        self.emit("    movzx rax, al");
        let offset = self.get_or_alloc_reg_offset(dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
    }

    fn compile_lt(&mut self, dst: &Reg, lhs: &Value, rhs: &Value, ty: &Type) {
        self.load_value_to_rax(lhs, ty);
        self.emit("    mov rcx, rax");
        self.load_value_to_rax(rhs, ty);
        self.emit("    cmp rcx, rax");
        self.emit("    setl al");
        self.emit("    movzx rax, al");
        let offset = self.get_or_alloc_reg_offset(dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
    }

    fn compile_jump(&mut self, target: &Label) {
        self.emit(&format!("    jmp {}", target.0));
    }

    fn compile_jump_if(&mut self, cond: &Value, target: &Label) {
        self.load_value_to_rax(cond, &Type::Bool);
        self.emit("    cmp rax, 0");
        self.emit(&format!("    jne {}", target.0));
    }

    fn compile_jump_if_false(&mut self, cond: &Value, target: &Label) {
        self.load_value_to_rax(cond, &Type::Bool);
        self.emit("    cmp rax, 0");
        self.emit(&format!("    je {}", target.0));
    }

    fn compile_label(&mut self, target: &Label) {
        self.emit(&format!("{}:", target.0));
    }

    fn compile_call(&mut self, dst: &Option<Reg>, func: &Value, args: &Vec<Value>, ty: &Type) {
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

    fn compile_return(&mut self, val: &Option<Value>) {
        if let Some(v) = val {
            self.load_value_to_rax(v, &Type::I64);
        }
        else {
            self.emit("    xor rax, rax");
        }

        self.emit("    leave");
        self.emit("    ret");
    }

    fn compile_alloc(&mut self, dst: &Reg, size: &Value) {
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

    fn compile_heap_alloc(&mut self, dst: &Reg, size: &Value) {
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

    fn compile_heap_realloc(&mut self, dst: &Reg, ptr: &Value, old_size: &Value, new_size: &Value) {
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

    fn compile_store(&mut self, ptr: &Value, src: &Value) {
        self.load_value_to_rax(ptr, &Type::I64);
        self.emit("    mov rcx, rax");

        self.load_value_to_rax(src, &Type::I64);

        self.emit("    mov QWORD PTR [rcx], rax");
    }

    fn compile_load(&mut self, dst: &Reg, ptr: &Value) {
        self.load_value_to_rax(ptr, &Type::I64);

        self.emit("    mov rax, QWORD PTR [rax]");

        let offset = self.get_or_alloc_reg_offset(dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
    }

    fn compile_struct_alloc(&mut self, dst: &Reg, struct_name: &str) {
        let size = self.get_struct_size(struct_name);
        // Allocate to heap later
        let struct_offset = self.next_stack_offset;
        self.next_stack_offset += size as i32;
        self.emit(&format!("    lea rax, [rbp-{}]", struct_offset));
        let dst_offset = self.get_or_alloc_reg_offset(dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
    }

    fn compile_field_store(&mut self, struct_reg: &Reg, field_name: &str, value: &Value, ty: &Type) {
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

    fn compile_field_load(&mut self, dst: &Reg, struct_reg: &Reg, field_name: &str, ty: &Type) {
        let struct_offset = self.get_or_alloc_reg_offset(struct_reg);
        self.emit(&format!("    mov rcx, QWORD PTR [rbp-{}]", struct_offset));

        if let Some(field_offset) = self.get_field_offset_without_struct(field_name) {
            self.emit(&format!("    mov rax, QWORD PTR [rcx+{}]", field_offset));
            let dst_offset = self.get_or_alloc_reg_offset(dst);
            self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
        }
    }

    fn compile_array_alloc(&mut self, dst: &Reg, element_ty: &Type, size: &Value) {
        let element_size = self.type_size(element_ty);
        let dst_offset = self.get_or_alloc_reg_offset(dst);
        self.load_value_to_rax(size, &Type::I64);
        self.emit(&format!("    imul rax, {}", element_size));
        let array_base = self.next_stack_offset + 256;
        self.emit(&format!("    sub rsp, 256"));
        self.emit(&format!("    lea rax, [rbp-{}]", array_base));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
    }

    fn compile_index_load(&mut self, dst: &Reg, array: &Reg, index: &Value, element_ty: &Type) {
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

    fn compile_index_store(&mut self, array: &Reg, value: &Value, index: &Value, element_ty: &Type) {
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

    fn compile_slice_create(&mut self, dst: &Reg, array: &Reg, start: &Value, end: &Value, element_ty: &Type) {
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

    fn compile_slice_len(&mut self, dst: &Reg, slice: &Reg) {
        let slice_offset = self.get_or_alloc_reg_offset(slice);
        self.emit(&format!("    mov rax, QWORD PTR [rbp-{}]", slice_offset + 8));
        let dst_offset = self.get_or_alloc_reg_offset(dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
    }

    fn compile_slice_index_load(&mut self, dst: &Reg, slice: &Reg, index: &Value, element_ty: &Type) {
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

    fn compile_tuple_alloc(&mut self, dst: &Reg, element_types: &Vec<Type>) {
        let total_size: usize = element_types.iter()
            .map(|ty| self.type_size(ty))
            .sum();
        let dst_offset = self.get_or_alloc_reg_offset(dst);
        let tuple_base = self.next_stack_offset + total_size as i32;
        self.next_stack_offset = tuple_base + 8;
        self.emit(&format!("    lea rax, [rbp-{}]", tuple_base));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
    }

    fn compile_tuple_load(&mut self, dst: &Reg, tuple_reg: &Reg, index: &usize, ty: &Type) {
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

    fn compile_tuple_store(&mut self, tuple_reg: &Reg, index: &usize, value: &Value, ty: &Type) {
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

    fn compile_string_alloc(&mut self, dst: &Reg, data: &String) {
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

    fn compile_string_len(&mut self, dst: &Reg, string: &Reg) {
        let string_offset = self.get_or_alloc_reg_offset(string);
        self.emit(&format!("    mov rcx, QWORD PTR [rbp-{}]", string_offset));
        self.emit("    mov rax, QWORD PTR [rcx+8]");
        let dst_offset = self.get_or_alloc_reg_offset(dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
    }

    fn compile_string_concat(&mut self, dst: &Reg, left: &Reg, right: &Reg) {
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
        self.emit(&format!("    mov QWORD PTR [rbp-{}], r14", result_offset - 8));
        self.emit("    mov rax, r14");
        self.emit("    add rax, 16");
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", result_offset - 16));
        self.emit(&format!("    lea rax, [rbp-{}]", result_offset));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
    }

    fn compile_string_index(&mut self, dst: &Reg, string: &Reg, index: &Value) {
        let string_offset = self.get_or_alloc_reg_offset(string);
        self.emit(&format!("    mov rcx, QWORD PTR [rbp-{}]", string_offset));
        self.emit("    mov rcx, QWORD PTR [rcx]");
        self.load_value_to_rax(index, &Type::I64);
        self.emit("    add rcx, rax");
        self.emit("    movzx rax, BYTE PTR [rcx]");
        let dst_offset = self.get_or_alloc_reg_offset(dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
    }

    fn compile_vec_alloc(&mut self, dst: &Reg, element_ty: &Type, initial_cap: &Value) {
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

    fn compile_vec_push(&mut self, vec: &Reg, value: &Value, element_ty: &Type) {
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

    fn compile_vec_pop(&mut self, dst: &Reg, vec: &Reg, element_ty: &Type) {
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

    fn compile_vec_get(&mut self, dst: &Reg, vec: &Reg, index: &Value, element_ty: &Type) {
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

    fn compile_vec_set(&mut self, vec: &Reg, index: &Value, value: &Value, element_ty: &Type) {
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

    fn compile_vec_len(&mut self, dst: &Reg, vec: &Reg) {
        let vec_offset = self.get_or_alloc_reg_offset(vec);
        let dst_offset = self.get_or_alloc_reg_offset(dst);
        self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", vec_offset));
        self.emit(&format!("    mov rax, QWORD PTR [r12+{}]", 8));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
    }

    fn compile_vec_cap(&mut self, dst: &Reg, vec: &Reg) {
        let vec_offset = self.get_or_alloc_reg_offset(vec);
        let dst_offset = self.get_or_alloc_reg_offset(dst);
        self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", vec_offset));
        self.emit(&format!("    mov rax, QWORD PTR [r12+{}]", 16));
        self.emit(&format!("    mov QWORD PTR [rbp-{}]", dst_offset));
    }

    fn compile_hashmap_alloc(&mut self, dst: &Reg, key_ty: &Type, value_ty: &Type, initial_cap: &Value) {
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

    fn compile_hashmap_insert(&mut self, map: &Reg, key: &Value, value: &Value, key_ty: &Type, value_ty: &Type) {
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

    fn compile_hashmap_get(&mut self, dst: &Reg, found_dst: &Reg, map: &Reg, key: &Value, key_ty: &Type, value_ty: &Type) {
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

    fn compile_hashmap_remove(&mut self, success_dst: &Reg, map: &Reg, key: &Value, key_ty: &Type, value_ty: &Type) {
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

    fn compile_hashmap_contains(&mut self, dst: &Reg, map: &Reg, key: &Value, key_ty: &Type, value_ty: &Type) {
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

    fn compile_hashmap_len(&mut self, dst: &Reg, map: &Reg) {
        let map_offset = self.get_or_alloc_reg_offset(map);
        let dst_offset = self.get_or_alloc_reg_offset(dst);
        self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", map_offset));
        self.emit(&format!("    mov rax, QWORD PTR [r12+{}]", 8));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
    }

    fn compile_hashmap_cap(&mut self, dst: &Reg, map: &Reg) {
        let map_offset = self.get_or_alloc_reg_offset(map);
        let dst_offset = self.get_or_alloc_reg_offset(dst);
        self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", map_offset));
        self.emit(&format!("    mov rax, QWORD PTR [r12+{}]", 16));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
    }

    fn compile_hashmap_clear(&mut self, map: &Reg) {
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

    fn compile_sys_write(&mut self, fd: &Value, buf: &Reg, len: &Value, result_dst: &Reg) {
        let buf_offset = self.get_or_alloc_reg_offset(buf);
        self.emit(&format!("    mov rsi, QWORD PTR [rbp-{}]", buf_offset));
        self.emit("    mov rsi, QWORD PTR [rsi]");
        self.load_value_to_rax(len, &Type::I64);
        self.emit("    mov rdx, rax");
        self.load_value_to_rax(fd, &Type::I32);
        self.emit("    mov rdi, rax");
        self.emit("    mov rax, 1");
        self.emit("    syscall");
        let result_offset = self.get_or_alloc_reg_offset(result_dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", result_offset));
    }

    fn compile_sys_read(&mut self, fd: &Value, buf: &Reg, len: &Value, result_dst: &Reg) {
        let buf_offset = self.get_or_alloc_reg_offset(buf);
        self.emit(&format!("    mov rsi, QWORD PTR [rbp-{}]", buf_offset));
        self.emit("    mov rsi, QWORD PTR [rsi]");
        self.load_value_to_rax(len, &Type::I64);
        self.emit("    mov rdx, rax");
        self.load_value_to_rax(fd, &Type::I32);
        self.emit("    mov rdi, rax");
        self.emit("    mov rax, 0");
        self.emit("    syscall");
        let result_offset = self.get_or_alloc_reg_offset(result_dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", result_offset));
    }

    fn compile_sys_open(&mut self, path: &Reg, flags: &Value, mode: &Value, result_dst: &Reg) {
        let path_offset = self.get_or_alloc_reg_offset(path);
        self.emit(&format!("    mov rdi, QWORD PTR [rbp-{}]", path_offset));
        self.emit("    mov rdi, QWORD PTR [rdi]");
        self.load_value_to_rax(flags, &Type::I32);
        self.emit("    mov rsi, rax");
        self.load_value_to_rax(mode, &Type::I32);
        self.emit("    mov rdx, rax");
        self.emit("    mov rax, 2");
        self.emit("    syscall");
        let result_offset = self.get_or_alloc_reg_offset(result_dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", result_offset));
    }

    fn compile_sys_close(&mut self, fd: &Value, result_dst: &Reg) {
        self.load_value_to_rax(fd, &Type::I32);
        self.emit("    mov rdi, rax");
        self.emit("    mov rax, 3");
        self.emit("    syscall");
        let result_offset = self.get_or_alloc_reg_offset(result_dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", result_offset));
    }

    fn compile_print(&mut self, value: &Reg) {
        let val_offset = self.get_or_alloc_reg_offset(value);
        self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", val_offset));
        self.emit("    mov rdi, 1");
        self.emit("    mov rsi, QWORD PTR [r12]");
        self.emit("    mov rdx, QWORD PTR [r12+8]");
        self.emit("    mov rax, 1");
        self.emit("    syscall");
        // We ignore errors for now, check later for partial writes and stuff like that
    }

    fn compile_println(&mut self, value: &Reg) {
        let val_offset = self.get_or_alloc_reg_offset(value);
        self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", val_offset));
        self.emit("    mov rdi, 1");
        self.emit("    mov rsi, QWORD PTR [r12]");
        self.emit("    mov rdx, QWORD PTR [r12+8]");
        self.emit("    mov rax, 1");
        self.emit("    syscall");
        // Currently do two writes for new line, we should append newline to end of buffer later
        self.emit("    mov rdi, 1");
        self.emit("    lea rsi, [rip+_newline]");
        self.emit("    mov rdx, 1");
        self.emit("    mov rax, 1");
        self.emit("    syscall");
    }

    fn compile_eprint(&mut self, value: &Reg) {
        let val_offset = self.get_or_alloc_reg_offset(value);
        self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", val_offset));
        self.emit("    mov rdi, 2");
        self.emit("    mov rsi, QWORD PTR [r12]");
        self.emit("    mov rdx, QWORD PTR [r12+8]");
        self.emit("    mov rax, 1");
        self.emit("    syscall");
    }

    fn compile_eprintln(&mut self, value: &Reg) {
        let val_offset = self.get_or_alloc_reg_offset(value);
        self.emit(&format!("    mov r12, QWORD PTR [rbp-{}]", val_offset));
        self.emit("    mov rdi, 2");
        self.emit("    mov rsi, QWORD PTR [r12]");
        self.emit("    mov rdx, QWORD PTR [r12+8]");
        self.emit("    mov rax, 1");
        self.emit("    syscall");
        // Currently do two writes for new line, we should append newline to end of buffer later
        self.emit("    mov rdi, 2");
        self.emit("    lea rsi, [rip+_newline]");
        self.emit("    mov rdx, 1");
        self.emit("    mov rax, 1");
        self.emit("    syscall");
    }

    fn compile_read_line(&mut self, dst: &Reg) {
        let id = self.next_label_id();
        let loop_label = format!("readline_loop_{}", id);
        let found_nl_label = format!("readline_found_nl_{}", id);
        let done_label = format!("readline_done_{}", id);
        self.emit("    mov rax, 12");
        self.emit("    xor rdi, rdi");
        self.emit("    syscall");
        self.emit("    mov r12, rax");
        self.emit("    add rax, 4096");
        self.emit("    mov rdi, rax");
        self.emit("    mov rax, 12");
        self.emit("    syscall");
        self.emit("    mov rdi, 0");
        self.emit("    mov rsi, r12");
        self.emit("    mov rdx, 4095");
        self.emit("    mov rax, 0");
        self.emit("    syscall");
        self.emit("    mov r13, rax");
        self.emit("    test rax, rax");
        self.emit(&format!("    jle {}", done_label));
        self.emit("    xor rcx, rcx");
        self.emit(&format!("{}:", loop_label));
        self.emit("    cmp rcx, r13");
        self.emit(&format!("    jge {}", done_label));
        self.emit("    mov al, BYTE PTR [r12+rcx]");
        self.emit("    cmp al, 10");
        self.emit(&format!("    je {}", found_nl_label));
        self.emit("    inc rcx");
        self.emit(&format!("    jmp {}", loop_label));
        self.emit(&format!("{}:", found_nl_label));
        self.emit("    mov r13, rcx");
        self.emit(&format!("{}:", done_label));
        let dst_offset = self.get_or_alloc_reg_offset(dst);
        let string_struct_offset = self.next_stack_offset + 24;
        self.next_stack_offset = string_struct_offset + 8;
        self.emit(&format!("    mov QWORD PTR [rbp-{}], r12", string_struct_offset));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], r13", string_struct_offset - 8));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], 4096", string_struct_offset - 16));
        self.emit(&format!("    lea rax, [rbp-{}]", string_struct_offset));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
    }

    fn compile_int_to_string(&mut self, dst: &Reg, value: &Value) {
        let id = self.next_label_id();
        let negative_label = format!("itoa_negative_{}", id);
        let convert_loop = format!("itoa_loop_{}", id);
        let build_loop = format!("itoa_build_{}", id);
        let done_label = format!("itoa_done_{}", id);
        self.load_value_to_rax(value, &Type::I64);
        self.emit("    push rax");
        self.emit("    mov rax, 12");
        self.emit("    xor rdi, rdi");
        self.emit("    syscall");
        self.emit("    mov r12, rax");
        self.emit("    lea rdi, [rax + 24]");
        self.emit("    mov rax, 12");
        self.emit("    syscall");
        self.emit("    pop rax");
        self.emit("    xor r13, r13");
        self.emit("    xor r14, r14");
        self.emit("    test rax, rax");
        self.emit(&format!("    jns {}", convert_loop));
        self.emit("    mov r14, 1");
        self.emit("    neg rax");
        self.emit(&format!("{}:", convert_loop));
        self.emit("    xor rdx, rdx");
        self.emit("    mov rcx, 10");
        self.emit("    div rcx");
        self.emit("    add rdx, 48");
        self.emit("    push rdx");
        self.emit("    inc r13");
        self.emit("    test rax, rax");
        self.emit(&format!("    jnz {}", convert_loop));
        self.emit("    test r14, r14");
        self.emit(&format!("    jz {}", build_loop));
        self.emit("    push 45");
        self.emit("    inc r13");
        self.emit(&format!("{}:", build_loop));
        self.emit("    mov rdi, r12");
        self.emit("    mov rcx, r13");
        self.emit("    mov r15, r13");
        self.emit(&format!("{}_pop:", build_loop));
        self.emit("    test rcx, rcx");
        self.emit(&format!("    jz {}", done_label));
        self.emit("    pop rax");
        self.emit("    mov BYTE PTR [rdi], al");
        self.emit("    inc rdi");
        self.emit("    dec rcx");
        self.emit(&format!("    jmp {}_pop", build_loop));
        self.emit(&format!("{}:", done_label));
        self.emit("    mov BYTE PTR [rdi], 0");
        let dst_offset = self.get_or_alloc_reg_offset(dst);
        let string_struct_offset = self.next_stack_offset + 24;
        self.next_stack_offset = string_struct_offset + 8;
        self.emit(&format!("    mov QWORD PTR [rbp-{}], r12", string_struct_offset));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], r15", string_struct_offset - 8));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], 24", string_struct_offset - 16));
        self.emit(&format!("    lea rax, [rbp-{}]", string_struct_offset));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
    }

    fn compile_sys_seek(&mut self, fd: &Value, offset: &Value, whence: &Value, result_dst: &Reg) {
        self.load_value_to_rax(fd, &Type::I32);
        self.emit("    mov rdi, rax");
        self.load_value_to_rax(offset, &Type::I64);
        self.emit("    mov rsi, rax");
        self.load_value_to_rax(whence, &Type::I32);
        self.emit("    mov rdx, rax");
        self.emit("    mov rax, 8");
        self.emit("    syscall");
        let result_offset = self.get_or_alloc_reg_offset(result_dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", result_offset));
    }

    fn compile_sys_stat(&mut self, path: &Reg, stat_buf: &Reg, result_dst: &Reg) {
        let path_offset = self.get_or_alloc_reg_offset(path);
        self.emit("    sub rsp, 144");
        self.emit("    mov r12, rsp");
        self.emit(&format!("    mov rdi, QWORD PTR [rbp-{}]", path_offset));
        self.emit("    mov rdi, QWORD PTR [rdi]");
        self.emit("    mov rsi, r12");
        self.emit("    mov rax, 4");
        self.emit("    syscall");
        self.emit("    mov r13, rax");
        let stat_buf_offset = self.get_or_alloc_reg_offset(stat_buf);
        let stat_offset = self.next_stack_offset + 24;
        self.next_stack_offset = stat_offset + 8;
        self.emit("    mov rax, QWORD PTR [r12+48]");
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", stat_offset));
        self.emit("    mov eax, DWORD PTR [r12+24]");
        self.emit(&format!("    mov DWORD PTR [rbp-{}], eax", stat_offset - 8));
        self.emit("    mov rax, QWORD PTR [r12+88]");
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", stat_offset - 16));
        self.emit(&format!("    lea rax, [rbp-{}]", stat_offset));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", stat_buf_offset));
        self.emit("    add rsp, 144");
        let result_offset = self.get_or_alloc_reg_offset(result_dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], r13", result_offset));
    }

    fn compile_sys_fstat(&mut self, fd: &Value, stat_buf: &Reg, result_dst: &Reg) {
        self.emit("    sub rsp, 144");
        self.emit("    mov r12, rsp");
        self.load_value_to_rax(fd, &Type::I32);
        self.emit("    mov rdi, rax");
        self.emit("    mov rsi, r12");
        self.emit("    mov rax, 5");
        self.emit("    syscall");
        self.emit("    mov r13, rax");
        let stat_buf_offset = self.get_or_alloc_reg_offset(stat_buf);
        let stat_offset = self.next_stack_offset + 24;
        self.next_stack_offset = stat_offset + 8;
        self.emit("    mov rax, QWORD PTR [r12+48]");
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", stat_offset));
        self.emit("    mov eax, DWORD PTR [r12+24]");
        self.emit(&format!("    mov DWORD PTR [rbp-{}], eax", stat_offset - 8));
        self.emit("    mov rax, QWORD PTR [r12+88]");
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", stat_offset - 16));
        self.emit(&format!("    lea rax, [rbp-{}]", stat_offset));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", stat_buf_offset));
        self.emit("    add rsp, 144");
        let result_offset = self.get_or_alloc_reg_offset(result_dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], r13", result_offset));
    }

    fn compile_sys_mkdir(&mut self, path: &Reg, mode: &Value, result_dst: &Reg) {
        let path_offset = self.get_or_alloc_reg_offset(path);
        self.emit(&format!("    mov rdi, QWORD PTR [rbp-{}]", path_offset));
        self.emit("    mov rdi, QWORD PTR [rdi]");
        self.load_value_to_rax(mode, &Type::I32);
        self.emit("    mov rsi, rax");
        self.emit("    mov rax, 83");
        self.emit("    syscall");
        let result_offset = self.get_or_alloc_reg_offset(result_dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", result_offset));
    }

    fn compile_sys_rmdir(&mut self, path: &Reg, result_dst: &Reg) {
        let path_offset = self.get_or_alloc_reg_offset(path);
        self.emit(&format!("    mov rdi, QWORD PTR [rbp-{}]", path_offset));
        self.emit("    mov rdi, QWORD PTR [rdi]");
        self.emit("    mov rax, 84");
        self.emit("    syscall");
        let result_offset = self.get_or_alloc_reg_offset(result_dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", result_offset));
    }

    fn compile_sys_unlink(&mut self, path: &Reg, result_dst: &Reg) {
        let path_offset = self.get_or_alloc_reg_offset(path);
        self.emit(&format!("    mov rdi, QWORD PTR [rbp-{}]", path_offset));
        self.emit("    mov rdi, QWORD PTR [rdi]");
        self.emit("    mov rax, 87");
        self.emit("    syscall");
        let result_offset = self.get_or_alloc_reg_offset(result_dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", result_offset));
    }

    fn compile_sys_rename(&mut self, old_path: &Reg, new_path: &Reg, result_dst: &Reg) {
        let old_path_offset = self.get_or_alloc_reg_offset(old_path);
        let new_path_offset = self.get_or_alloc_reg_offset(new_path);
        self.emit(&format!("    mov rdi, QWORD PTR [rbp-{}]", old_path_offset));
        self.emit("    mov rdi, QWORD PTR [rdi]");
        self.emit(&format!("    mov rsi, QWORD PTR [rbp-{}]", new_path_offset));
        self.emit("    mov rsi, QWORD PTR [rsi]");
        self.emit("    mov rax, 82");
        self.emit("    syscall");
        let result_offset = self.get_or_alloc_reg_offset(result_dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", result_offset));
    }

    fn compile_sys_access(&mut self, path: &Reg, mode: &Value, result_dst: &Reg) {
        let path_offset = self.get_or_alloc_reg_offset(path);
        self.emit(&format!("    mov rdi, QWORD PTR [rbp-{}]", path_offset));
        self.emit("    mov rdi, QWORD PTR [rdi]");
        self.load_value_to_rax(mode, &Type::I32);
        self.emit("    mov rsi, rax");
        self.emit("    mov rax, 21");
        self.emit("    syscall");
        let result_offset = self.get_or_alloc_reg_offset(result_dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", result_offset));
    }

    fn compile_sys_getcwd(&mut self, dst: &Reg) {
        let id = self.next_label_id();
        self.emit("    mov rax, 12");
        self.emit("    xor rdi, rdi");
        self.emit("    syscall");
        self.emit("    mov r12, rax");
        self.emit("    lea rdi, [rax+4096]");
        self.emit("    mov rax, 12");
        self.emit("    syscall");
        self.emit("    mov rdi, r12");
        self.emit("    mov rsi, 4096");
        self.emit("    mov rax, 79");
        self.emit("    syscall");
        self.emit("    mov rdi, r12");
        self.emit("    xor rcx, rcx");
        let loop_label = format!("getcwd_strlen_{}", id);
        let done_label = format!("getcwd_done_{}", id);
        self.emit(&format!("{}:", loop_label));
        self.emit("    mov al, BYTE PTR [rdi+rcx]");
        self.emit("    test al, al");
        self.emit(&format!("    jz {}", done_label));
        self.emit("    inc rcx");
        self.emit(&format!("    jmp {}", loop_label));
        self.emit(&format!("{}:", done_label));
        self.emit("    mov r13, rcx");
        let dst_offset = self.get_or_alloc_reg_offset(dst);
        let string_struct_offset = self.next_stack_offset + 24;
        self.next_stack_offset = string_struct_offset + 8;
        self.emit(&format!("    mov QWORD PTR [rbp-{}], r12", string_struct_offset));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], r13", string_struct_offset - 8));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], 4096", string_struct_offset - 16));
        self.emit(&format!("    lea rax, [rbp-{}]", string_struct_offset));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
    }

    fn compile_sys_chdir(&mut self, path: &Reg, result_dst: &Reg) {
        let path_offset = self.get_or_alloc_reg_offset(path);
        self.emit(&format!("    mov rdi, QWORD PTR [rbp-{}]", path_offset));
        self.emit("    mov rdi, QWORD PTR [rdi]");
        self.emit("    mov rax, 80");
        self.emit("    syscall");
        let result_offset = self.get_or_alloc_reg_offset(result_dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", result_offset));
    }

    fn compile_file_read_to_string(&mut self, dst: &Reg, path: &Reg, result_dst: &Reg) {
        let id = self.next_label_id();
        let path_offset = self.get_or_alloc_reg_offset(path);
        let error_label = format!("file_read_error_{}", id);
        let cleanup_label = format!("file_read_cleanup_{}", id);
        let done_label = format!("file_read_done_{}", id);
        self.emit(&format!("    mov rdi, QWORD PTR [rbp-{}]", path_offset));
        self.emit("    mov rdi, QWORD PTR [rdi]");
        self.emit("    xor rsi, rsi");
        self.emit("    xor rdx, rdx");
        self.emit("    mov rax, 2");
        self.emit("    syscall");
        self.emit("    test rax, rax");
        self.emit(&format!("    js {}", error_label));
        self.emit("    mov r12, rax");
        self.emit("    sub rsp, 144");
        self.emit("    mov rdi, r12");
        self.emit("    mov rsi, rsp");
        self.emit("    mov rax, 5");
        self.emit("    syscall");
        self.emit("    test rax, rax");
        self.emit(&format!("    js {}", cleanup_label));
        self.emit("    mov r13, QWORD PTR [rsp+48]");
        self.emit("    add rsp, 144");
        self.emit("    mov rax, 12");
        self.emit("    xor rdi, rdi");
        self.emit("    syscall");
        self.emit("    mov r14, rax");
        self.emit("    mov rdi, r14");
        self.emit("    add rdi, r13");
        self.emit("    inc rdi");
        self.emit("    mov rax, 12");
        self.emit("    syscall");
        self.emit("    mov rdi, r12");
        self.emit("    mov rsi, r14");
        self.emit("    mov rdx, r13");
        self.emit("    mov rax, 0");
        self.emit("    syscall");
        self.emit("    mov r15, rax");
        self.emit("    test rax, rax");
        self.emit(&format!("    js {}", cleanup_label));
        self.emit("    mov rdi, r12");
        self.emit("    mov rax, 3");
        self.emit("    syscall");
        let dst_offset = self.get_or_alloc_reg_offset(dst);
        let string_struct_offset = self.next_stack_offset + 24;
        self.next_stack_offset = string_struct_offset + 8;
        self.emit(&format!("    mov QWORD PTR [rbp-{}], r14", string_struct_offset));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], r15", string_struct_offset - 8));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], r13", string_struct_offset - 16));
        self.emit(&format!("    lea rax, [rbp-{}]", string_struct_offset));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
        let result_offset = self.get_or_alloc_reg_offset(result_dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], 0", result_offset));
        self.emit(&format!("    jmp {}", done_label));
        self.emit(&format!("{}:", cleanup_label));
        self.emit("    mov rdi, r12");
        self.emit("    mov rax, 3");
        self.emit("    syscall");
        self.emit(&format!("{}:", error_label));
        // Preserve real error code later and use it
        let result_offset = self.get_or_alloc_reg_offset(result_dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], -1", result_offset));
        // Creating empty string instead of error
        let dst_offset = self.get_or_alloc_reg_offset(dst);
        let empty_string_offset = self.next_stack_offset + 24;
        self.next_stack_offset = empty_string_offset + 8;
        self.emit(&format!("    mov QWORD PTR [rbp-{}], 0", empty_string_offset));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], 0", empty_string_offset - 8));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], 0", empty_string_offset - 16));
        self.emit(&format!("    lea rax, [rbp-{}]", empty_string_offset));
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", dst_offset));
        self.emit(&format!("{}:", done_label));
    }

    fn compile_file_write_string(&mut self, path: &Reg, content: &Reg, result_dst: &Reg) {
        let id = self.next_label_id();
        let path_offset = self.get_or_alloc_reg_offset(path);
        let content_offset = self.get_or_alloc_reg_offset(content);
        let error_label = format!("file_write_error_{}", id);
        let done_label = format!("file_write_done_{}", id);
        self.emit(&format!("    mov rdi, QWORD PTR [rbp-{}]", path_offset));
        self.emit("    mov rdi, QWORD PTR [rdi]");
        self.emit("    mov rsi, 577");
        self.emit("    mov rdx, 420");
        self.emit("    mov rax, 2");
        self.emit("    syscall");
        self.emit("    test rax, rax");
        self.emit(&format!("    js {}", error_label));
        self.emit("    mov r12, rax");
        self.emit(&format!("    mov r13, QWORD PTR [rbp-{}]", content_offset));
        self.emit("    mov rsi, QWORD PTR [r13]");
        self.emit("    mov rdx, QWORD PTR [r13+8]");
        self.emit("    mov rdi, r12");
        self.emit("    mov rax, 1");
        self.emit("    syscall");
        self.emit("    mov r14, rax");
        self.emit("    mov rdi, r12");
        self.emit("    mov rax, 3");
        self.emit("    syscall");
        let result_offset = self.get_or_alloc_reg_offset(result_dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], r14", result_offset));
        self.emit(&format!("    jmp {}", done_label));
        self.emit(&format!("{}:", error_label));
        let result_offset = self.get_or_alloc_reg_offset(result_dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", result_offset));
        self.emit(&format!("{}:", done_label));
    }

    fn compile_file_append_string(&mut self, path: &Reg, content: &Reg, result_dst: &Reg) {
        let id = self.next_label_id();
        let path_offset = self.get_or_alloc_reg_offset(path);
        let content_offset = self.get_or_alloc_reg_offset(content);
        let error_label = format!("file_append_error_{}", id);
        let done_label = format!("file_append_done_{}", id);
        self.emit(&format!("    mov rdi, QWORD PTR [rbp-{}]", path_offset));
        self.emit("    mov rdi, QWORD PTR [rdi]");
        self.emit("    mov rsi, 1089");
        self.emit("    mov rdx, 420");
        self.emit("    mov rax, 2");
        self.emit("    syscall");
        self.emit("    test rax, rax");
        self.emit(&format!("    js {}", error_label));
        self.emit("    mov r12, rax");
        self.emit(&format!("    mov r13, QWORD PTR [rbp-{}]", content_offset));
        self.emit("    mov rsi, QWORD PTR [r13]");
        self.emit("    mov rdx, QWORD PTR [r13+8]");
        self.emit("    mov rdi, r12");
        self.emit("    mov rax, 1");
        self.emit("    syscall");
        self.emit("    mov r14, rax");
        self.emit("    mov rdi, r12");
        self.emit("    mov rax, 3");
        self.emit("    syscall");
        let result_offset = self.get_or_alloc_reg_offset(result_dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], r14", result_offset));
        self.emit(&format!("    jmp {}", done_label));
        self.emit(&format!("{}:", error_label));
        let result_offset = self.get_or_alloc_reg_offset(result_dst);
        self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", result_offset));
        self.emit(&format!("{}:", done_label));
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