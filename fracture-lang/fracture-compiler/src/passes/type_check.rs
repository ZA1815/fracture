use fracture_ir::{Program, Function, Inst, Value, Type, Reg};
use std::collections::HashMap;

pub fn check(program: &Program) -> Result<(), String> {
    for (name, func) in &program.functions {
        if func.is_unsafe() {
            println!("  Skipping type check for #[unsafe] function: {}", name);
            continue;
        }

        check_function(name, func, program)?;
    }

    Ok(())
}

fn check_function(name: &str, func: &Function, program: &Program) -> Result<(), String> {
    let mut env: HashMap<Reg, Type> = HashMap::new();

    for (reg, ty) in &func.params {
        env.insert(reg.clone(), ty.clone());
    }

    for (reg, ty) in &func.locals {
        env.insert(reg.clone(), ty.clone());
    }

    for inst in &func.body {
        check_instruction(inst, &mut env, name, program)?;
    }

    Ok(())
}

fn check_instruction(inst: &Inst, env: &mut HashMap<Reg, Type>, func_name: &str, program: &Program) -> Result<(), String> {
    match inst {
        Inst::Move { dst, src, ty } => {
            let src_ty = infer_value_type(src, env)?;

            let final_ty = if *ty == Type::Unknown {
                src_ty.clone()
            }
            else {
                if !types_compatible(&src_ty, ty) {
                    return Err(format!(
                        "Type mismatch in {}: expected {:?}, got {:?}",
                        func_name, ty, src_ty
                    ));
                }

                ty.clone()
            };
            
            env.insert(dst.clone(), final_ty.clone());

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
        Inst::StructAlloc { dst, struct_name } => {
            if !program.structs.contains_key(struct_name) {
                return Err(format!("Unknown struct '{}' in function {}", struct_name, func_name))
            }
            env.insert(dst.clone(), Type::Struct(struct_name.clone()));

            Ok(())
        }
        Inst::FieldStore { struct_reg, field_name, value, ty } => {
            let struct_ty = env.get(struct_reg)
                .ok_or_else(|| format!("Register r{} not found for field access", struct_reg.0))?;

            if let Type::Struct(struct_name) = struct_ty {
                let field_ty = program.field_offset(&struct_name, field_name)
                    .map(|(_, ty)| ty)
                    .ok_or_else(|| format!("Struct '{}' has no field '{}'", struct_name, field_name))?;

                let val_ty = infer_value_type(value, env)?;

                if !types_compatible(field_ty, &val_ty) {
                    return Err(format!("Type mismatch in field '{}': expected {:?}, got {:?}", field_name, field_ty, val_ty));
                }
            }
            else {
                return Err(format!("Cannot access field of non-struct type {:?}", struct_ty));
            }

            Ok(())
        }
        Inst::FieldLoad { dst, struct_reg, field_name, ty } => {
            let struct_ty = env.get(struct_reg)
                .ok_or_else(|| format!("Register r{} not found for field access", struct_reg.0))?;

            if let Type::Struct(struct_name) = struct_ty {
                let field_ty = program.field_offset(&struct_name, field_name)
                    .map(|(_, ty)| ty)
                    .ok_or_else(|| format!("Struct '{}' has no field '{}'", struct_name, field_name))?;

                env.insert(dst.clone(), field_ty.clone());
            }
            else {
                return Err(format!("Cannot access field of non-struct type {:?}", struct_ty));
            }

            Ok(())
        }
        Inst::ArrayAlloc { dst, element_ty, size } => {
            let count = match size {
                Value::Const(fracture_ir::Const::I32(n)) => *n as usize,
                _ => 0 // Placeholder for dynamic sizing
            };

            env.insert(dst.clone(), Type::Array(Box::new(element_ty.clone()), count));

            Ok(())
        }
        Inst::IndexLoad { dst, array, index, element_ty } => {
            if !env.contains_key(array) {
                return Err(format!("Array register r{} not found", array.0));
            }

            let idx_type = infer_value_type(index, env)?;
            if !is_numeric(&idx_type) {
                return Err(format!("Array index must be numeric, got {:?}", idx_type));
            }

            env.insert(dst.clone(), element_ty.clone());

            Ok(())
        }
        Inst::IndexStore { array, index, value, element_ty } => {
            if !env.contains_key(array) {
                return Err(format!("Array register r{} not found", array.0));
            }

            let idx_type = infer_value_type(index, env)?;
            if !is_numeric(&idx_type) {
                return Err(format!("Array index must be numeric, got {:?}", idx_type));
            }

            let val_type = infer_value_type(value, env)?;
            if !types_compatible(element_ty, &val_type) {
                return Err(format!("Type mismatch in array store: expected {:?}, got {:?}", element_ty, val_type));
            }

            Ok(())
        }
        Inst::SliceCreate { dst, array, start, end, element_ty } => {
            if !env.contains_key(array) {
                return Err(format!("Source register r{} not found for slicing", array.0));
            }

            let start_ty = infer_value_type(start, env)?;
            let end_ty = infer_value_type(end, env)?;

            if !is_numeric(&start_ty) || !is_numeric(&end_ty) {
                return Err(format!("Slice indices must be numeric"));
            }

            env.insert(dst.clone(), Type::Slice(Box::new(element_ty.clone())));

            Ok(())
        }
        Inst::SliceLen { dst, slice } => {
            if !env.contains_key(slice) {
                return Err(format!("Slice register r{} not found", slice.0));
            }

            env.insert(dst.clone(), Type::I64);

            Ok(())
        }
        Inst::SliceIndexLoad { dst, slice, index, element_ty } => {
            if !env.contains_key(slice) {
                return Err(format!("Slice register r{} not found", slice.0));
            }

            let idx_ty = infer_value_type(index, env)?;
            if !is_numeric(&idx_ty) {
                return Err(format!("Slice index must be numeric"));
            }

            env.insert(dst.clone(), element_ty.clone());

            Ok(())
        }
        Inst::TupleAlloc { dst, element_types } => {
            env.insert(dst.clone(), Type::Tuple(element_types.clone()));

            Ok(())
        }
        Inst::TupleLoad { dst, tuple_reg, index, ty } => {
            let tuple_ty = env.get(tuple_reg)
                .ok_or_else(|| format!("Register r{} not found for tuple load", tuple_reg.0))?;

            if let Type::Tuple(elem_types) = tuple_ty {
                if *index >= elem_types.len() {
                    return Err(format!("Tuple index out of bounds: {} >= {}", index, elem_types.len()));
                }

                env.insert(dst.clone(), elem_types[*index].clone());
            }
            else {
                return Err(format!("Register r{} is not a tuple, got {:?}", tuple_reg.0, tuple_ty));
            }

            Ok(())
        }
        Inst::TupleStore { tuple_reg, index, value, ty } => {
            let tuple_ty = env.get(tuple_reg)
                .ok_or_else(|| format!("Register r{} not found ofr tuple store", tuple_reg.0))?;

            if let Type::Tuple(elem_types) = tuple_ty {
                if *index >= elem_types.len() {
                    return Err(format!("Tuple index out of bounds: {} >= {}", index, elem_types.len()));
                }

                let val_ty = infer_value_type(value, env)?;
                let expected_ty = &elem_types[*index];

                if !types_compatible(expected_ty, &val_ty) {
                    return Err(format!(
                        "Type mismatch in tuple store at index {}: expected {:?}, got {:?}",
                        index, expected_ty, val_ty
                    ));
                }
            }
            else {
                return Err(format!("Register r{} is not a tuple, got {:?}", tuple_reg.0, tuple_ty));
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

