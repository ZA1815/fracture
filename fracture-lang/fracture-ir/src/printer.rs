use crate::hsir::*;

pub fn print_inst(inst: &Inst) -> String {
    match inst {
        Inst::Move { dst, src, ty } => {
            format!("move r{}, {}, {}", dst.0, print_value(src), print_type(ty))
        }
        Inst::Add { dst, lhs, rhs, ty } => {
            format!("add r{}, {}, {}, {}", dst.0, print_value(lhs), print_value(rhs), print_type(ty))
        }
        Inst::Jump { target } => {
            format!("jump {}", target.0)
        }
        Inst::Call { dst, func, args, ty } => {
            let dst_str = dst.as_ref()
                .map(|r| format!("r{}", r.0))
                .unwrap_or_else(|| "_".to_string());
            let args_str = args.iter()
                .map(print_value)
                .collect::<Vec<_>>()
                .join(", ");
            format!("call {}, {}, [{}], {}", dst_str, print_value(func), args_str, print_type(ty))
        }
        _ => format!("{:?}", inst)
    }
}

pub fn print_program(program: &crate::hsir::Program) -> String {
    program.to_text()
}

fn print_value(val: &Value) -> String {
    match val {
        Value::Reg(r) => format!("r{}", r.0),
        Value::Const(c) => print_const(c),
        Value::Label(l) => l.0.clone()
    }
}

fn print_const(c: &Const) -> String {
    match c {
        Const::I32(i) => i.to_string(),
        Const::I64(i) => format!("{}i64", i),
        Const::F32(f) => format!("{}f", f),
        Const::F64(d) => format!("{}d", d),
        Const::Bool(b) => b.to_string(),
        Const::String(s) => format!("\"{}\"", s),
        Const::Null => "null".to_string()
    }
}

fn print_type(ty: &Type) -> String {
    match ty {
        Type::I32 => "i32",
        Type::I64 => "i64",
        Type::F32 => "f32",
        Type::F64 => "f64",
        Type::Bool => "bool",
        Type::String => "string",
        Type::Void => "void",
        Type::Unknown => "?",
        _ => "complex"
    }.to_string()
}