use fracture_ir::{Program, Function, Inst, Value, Type, Reg};
use std::collections::HashMap;

pub fn check(program: &Program) -> Result<(), String> {
    for (name, func) in &program.functions {
        if func.is_unsafe() {
            println!("  Skipping type check for #[unsafe] function: {}", name);
            continue;
        }

        check_function(name, func)?;
    }

    Ok(())
}

fn check_function(name: &str, func: &Function) -> Result<(), String> {
    let mut env: HashMap<Reg, Type> = HashMap::new();

    for (reg, ty) in &func.params {
        env.insert(reg.clone(), ty.clone());
    }

    for (reg, ty) in &func.locals {
        env.insert(reg.clone(), ty.clone());
    }

    for inst in &func.body {
        check_instruction(inst, &mut env, name)?;
    }

    Ok(())
}

fn check_instruction(inst: &Inst, env: &mut HashMap<Reg, Type>, func_name: &str) -> Result<(), String> {
    match inst {
        Inst::Move { dst, src, ty } => {
            let src_ty = infer_value_type(src, env)?;
            if !types_compatible(&src_ty, ty) {
                return Err(format!(
                    "Type mismatch in {}: expected {:?}, got {:?}",
                    func_name, ty, src_ty
                ));
            }
            env.insert(dst.clone(), ty.clone());

            Ok(())
        }
        // Have to account for string concat later
        Inst::Add { dst, lhs, rhs, ty } |
        Inst::Sub { dst, lhs, rhs, ty } |
        Inst::Mul { dst, lhs, rhs, ty } |
        Inst::Div { dst, lhs, rhs, ty } => {
            let lhs_ty = infer_value_type(lhs, env)?;
            let rhs_ty = infer_value_type(rhs, env)?;

            if !is_numeric(&lhs_ty) || !is_numeric(&rhs_ty) {
                return Err(format!(
                    "Arithmetic operation in {} requires numeric types, got {:?} and {:?}",
                    func_name, lhs_ty, rhs_ty
                ));
            }

            env.insert(dst.clone(), ty.clone());

            Ok(())
        }
        Inst::Eq { dst, lhs, rhs, ty } |
        Inst::Lt { dst, lhs, rhs, ty } |
        Inst::Gt { dst, lhs, rhs, ty } => {
            let lhs_ty = infer_value_type(lhs, env)?;
            let rhs_ty = infer_value_type(rhs, env)?;

            if !types_compatible(&lhs_ty, &rhs_ty) {
                return Err(format!(
                    "Comparison in {} requires compatible types, got {:?} and {:?}",
                    func_name, lhs_ty, rhs_ty
                ));
            }

            env.insert(dst.clone(), Type::Bool);

            Ok(())
        }
        Inst::Call { dst, func, args, ty } => {
            for arg in args {
                infer_value_type(arg, env)?;
            }
            
            if let Some(dst_reg) = dst {
                env.insert(dst_reg.clone(), ty.clone());
            }

            Ok(())
        }
        Inst::Return { val } => {
            if let Some(v) = val {
                infer_value_type(v, env)?;
            }

            Ok(())
        }
        // Implement type checking for other insts later
        _ => Ok(())
    }
}

fn infer_value_type(val: &Value, env: &HashMap<Reg, Type>) -> Result<Type, String> {
    match val {
        Value::Const(c) => {
            use fracture_ir::Const;
            Ok(match c {
                Const::I32(_) => Type::I32,
                Const::I64(_) => Type::I64,
                Const::F32(_) => Type::F32,
                Const::F64(_) => Type::F64,
                Const::Bool(_) => Type::Bool,
                Const::String(_) => Type::String,
                Const::Null => Type::Ptr(Box::new(Type::Void))
            })
        }
        Value::Reg(r) => {
            env.get(r).cloned().ok_or_else(|| format!("Register r{} not found in type environment", r.0))
        }
        Value::Label(_) => Ok(Type::Function(vec![], Box::new(Type::Unknown)))
    }
}

fn types_compatible(t1: &Type, t2: &Type) -> bool {
    if t1 == t2 {
        return true;
    }

    if matches!(t1, Type::Unknown) || matches!(t2, Type::Unknown) {
        return true;
    }

    // Add more compatibility rules later
    false
}

fn is_numeric(ty: &Type) -> bool {
    matches!(ty, Type::I32 | Type::I64 | Type::U32 | Type::U64 | Type::F32 | Type::F64)
}

