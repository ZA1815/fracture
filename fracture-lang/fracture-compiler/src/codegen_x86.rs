use fracture_ir::hsir::*;
use std::collections::HashMap;

pub struct X86CodeGen {
    output: Vec<String>,
    reg_offsets: HashMap<Reg, i32>,
    next_stack_offset: i32,
    current_function_stack_size: i32
}

impl X86CodeGen {
    pub fn new() -> Self {
        Self {
            output: Vec::new(),
            reg_offsets: HashMap::new(),
            next_stack_offset: 8,
            current_function_stack_size: 0
        }
    }

    pub fn compile_program(&mut self, program: &Program) -> String {
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
            Inst::Store { ptr, src, ty } => {
                self.load_value_to_rax(ptr, &Type::I64);
                self.emit("    mov rcx, rax");

                self.load_value_to_rax(src, &Type::I64);

                self.emit("    mov QWORD PTR [rcx], rax");
            }
            Inst::Load { dst, ptr, ty } => {
                self.load_value_to_rax(ptr, &Type::I64);

                self.emit("    mov rax, QWORD PTR [rax]");

                let offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
            }
            Inst::SimPoint { id, metadata } => {
                self.emit(&format!("    # SimPoint: {}", id));
            }
            _ => {
                self.emit(&format!("    # TODO: {:?}", inst));
            }
        }
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