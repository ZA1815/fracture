use crate::hsir::*;
use std::io::Write;
use std::collections::HashMap;

pub struct X86CodeGen {
    output: Vec<String>,
    reg_offsets: HashMap<Reg, i32>,
    next_stack_offset: i32,
    label_counter: usize
}

impl X86CodeGen {
    pub fn new() -> Self {
        Self {
            output: Vec::new(),
            reg_offsets: HashMap::new(),
            next_stack_offset: 8,
            label_counter: 0
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

        let stack_size = func.body.len() * 16;
        self.emit(&format!("    sub rsp, {}", stack_size));

        self.reg_offsets.clear();
        self.next_stack_offset = 8;

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

                self.emit("sub rcx, rax");
                self.emit("    mov rax, rcx");

                let offset = self.get_or_alloc_reg_offset(dst);
                self.emit(&format!("    mov QWORD PTR [rbp-{}], rax", offset));
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