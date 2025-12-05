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
        Inst::Load { dst, ptr, ty } => {
            let ptr_ty = infer_value_type(ptr, env)?;
            
            if !matches!(ptr_ty, Type::Ptr(_)) {
                return Err(format!("Load requires pointer type, got {:?}", ptr_ty));
            }

            env.insert(dst.clone(), ty.clone());

            Ok(())
        }
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
        Inst::Eq { dst, lhs, rhs, ty: _ } |
        Inst::Lt { dst, lhs, rhs, ty: _ } |
        Inst::Gt { dst, lhs, rhs, ty: _ } |
        Inst::Le { dst, lhs, rhs, ty: _ } |
        Inst::Ge { dst, lhs, rhs, ty: _ } |
        Inst::Ne { dst, lhs, rhs, ty: _ } => {
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
        Inst::Call { dst, func: _, args, ty } => {
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
        Inst::FieldStore { struct_reg, field_name, value, ty: _ } => {
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
        Inst::FieldLoad { dst, struct_reg, field_name, ty: _ } => {
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
        Inst::TupleLoad { dst, tuple_reg, index, ty: _ } => {
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
        Inst::TupleStore { tuple_reg, index, value, ty: _ } => {
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
        Inst::StringAlloc { dst, data: _ } => {
            env.insert(dst.clone(), Type::String);

            Ok(())
        }
        Inst::StringLen { dst, string: _ } => {
            env.insert(dst.clone(), Type::I32);

            Ok(())
        }
        Inst::StringConcat { dst, left, right } => {
            let left_ty = env.get(left)
                .ok_or_else(|| format!("StringConcat: Left register r{} not found", left.0))?;
            let right_ty = env.get(right)
                .ok_or_else(|| format!("StringConcat: Right register r{} not found", right.0))?;

            if !matches!(left_ty, Type::String) || !matches!(right_ty, Type::String) {
                return Err(format!("StringConcat requires two strings, got {:?} and {:?}", left_ty, right_ty));
            }

            env.insert(dst.clone(), Type::String);

            Ok(())
        }
        Inst::VecAlloc { dst, element_ty, initial_cap } => {
            let cap_ty = infer_value_type(initial_cap, env)?;
            if !is_numeric(&cap_ty) {
                return Err(format!("Vec capacity must be numeric, got {:?}", cap_ty));
            }

            env.insert(dst.clone(), Type::Vec(Box::new(element_ty.clone())));

            Ok(())
        }
        Inst::VecPush { vec, value, element_ty: _ } => {
            let vec_ty = env.get(vec)
                .ok_or_else(|| format!("Vec register r{} not found", vec.0))?;

            if let Type::Vec(inner_ty) = vec_ty {
                let val_ty = infer_value_type(value, env)?;
                if !types_compatible(&inner_ty, &val_ty) {
                    return Err(format!(
                        "Type mismatch in vec push: expected {:?}, got {:?}",
                        inner_ty, vec_ty
                    ));
                }
            }
            else {
                return Err(format!("Cannot push to non-Vec type {:?}", vec_ty));
            }

            Ok(())
        }
        Inst::VecPop { dst, vec, element_ty: _ } => {
            let vec_ty = env.get(vec)
                .ok_or_else(|| format!("Vec register r{} not found", vec.0))?;

            if let Type::Vec(inner_ty) = vec_ty {
                env.insert(dst.clone(), (**inner_ty).clone());
            }
            else {
                return Err(format!("Cannot pop from non-Vec type: {:?}", vec_ty));
            }

            Ok(())
        }
        Inst::VecGet { dst, vec, index, element_ty: _ } => {
            let vec_ty = env.get(vec)
                .ok_or_else(|| format!("Vec register r{} not found", vec.0))?;

            let idx_ty = infer_value_type(index, env)?;
            if !is_numeric(&idx_ty) {
                return Err(format!("Vec index must be numeric, got {:?}", idx_ty));
            }

            if let Type::Vec(inner_ty) = vec_ty {
                env.insert(dst.clone(), (**inner_ty).clone());
            }
            else {
                return Err(format!("Cannot index non-Vec type {:?}", vec_ty));
            }

            Ok(())
        }
        Inst::VecSet { vec, index, value, element_ty: _ } => {
            let vec_ty = env.get(vec)
                .ok_or_else(|| format!("Vec register r{} not found", vec.0))?;

            let idx_ty = infer_value_type(index, env)?;
            if !is_numeric(&idx_ty) {
                return Err(format!("Vec index must be numeric, got {:?}", idx_ty));
            }

            if let Type::Vec(inner_ty) = vec_ty {
                let val_ty = infer_value_type(value, env)?;
                if !types_compatible(inner_ty, &val_ty) {
                    return Err(format!(
                        "Type mismatch in Vec set: exepcted {:?}, got {:?}",
                        inner_ty, val_ty
                    ));
                }
            }
            else {
                return Err(format!("Cannot set on non-Vec type {:?}", vec_ty));
            }

            Ok(())
        }
        Inst::VecLen { dst, vec } => {
            if !env.contains_key(vec) {
                return Err(format!("Vec register r{} not found", vec.0));
            }

            env.insert(dst.clone(), Type::I64);

            Ok(())
        }
        Inst::VecCap { dst, vec } => {
            if !env.contains_key(vec) {
                return Err(format!("Vec register r{} not found", vec.0));
            }

            env.insert(dst.clone(), Type::I64);

            Ok(())
        }
        Inst::HashMapAlloc { dst, key_ty, value_ty, initial_cap } => {
            let cap_ty = infer_value_type(initial_cap, env)?;
            if !is_numeric(&cap_ty) {
                return Err(format!("Hashmap initial capacity must be numeric, got {:?}", cap_ty));
            }

            // Add support for anything with the hash trait later
            if !is_hashable(key_ty) {
                return Err(format!("HashMap key must be hashable (numeric or string), got {:?}", key_ty));
            }

            env.insert(dst.clone(), Type::HashMap(Box::new(key_ty.clone()), Box::new(value_ty.clone())));

            Ok(())
        }
        Inst::HashMapInsert { map, key, value, key_ty: _, value_ty: _ } => {
            let map_ty = env.get(map)
                .ok_or_else(|| format!("HashMap register r{} not found", map.0))?;

            if let Type::HashMap(expected_key_ty, expected_value_ty) = map_ty {
                let actual_key_ty = infer_value_type(key, env)?;
                if !types_compatible(&expected_key_ty, &actual_key_ty) {
                    return Err(format!("HashMap key type mismatch: expected {:?}, got {:?}", expected_key_ty, actual_key_ty));
                }

                let actual_value_ty = infer_value_type(value, env)?;
                if !types_compatible(&expected_value_ty, &actual_value_ty) {
                    return Err(format!("HashMap value type mismatch: expected {:?}, got {:?}", expected_value_ty, actual_value_ty));
                }
            }
            else {
                return Err(format!("Cannot insert into non-HashMap type {:?}", map_ty));
            }

            Ok(())
        }
        Inst::HashMapGet { dst, found_dst, map, key, key_ty: _, value_ty: _ } => {
            let map_ty = env.get(map)
                .ok_or_else(|| format!("HashMap register r{} not found", map.0))?;

            if let Type::HashMap(expected_key_ty, expected_value_ty) = map_ty {
                let actual_key_ty = infer_value_type(key, env)?;
                if !types_compatible(&expected_key_ty, &actual_key_ty) {
                    return Err(format!("HashMap key type mismatch: expected {:?}, got {:?}", expected_key_ty, actual_key_ty));
                }

                let expected_value_ty = expected_value_ty.clone();

                env.insert(found_dst.clone(), Type::Bool);

                env.insert(dst.clone(), (*expected_value_ty).clone());
            }
            else {
                return Err(format!("Cannot get from non-HashMap type {:?}", map_ty));
            }

            Ok(())
        }
        Inst::HashMapRemove { success_dst, map, key, key_ty: _, value_ty: _ } => {
            let map_ty = env.get(map)
                .ok_or_else(|| format!("HashMap register r{} not found", map.0))?;

            if let Type::HashMap(expected_key_ty, _) = map_ty {
                let actual_key_ty = infer_value_type(key, env)?;
                if !types_compatible(&expected_key_ty, &actual_key_ty) {
                    return Err(format!("HashMap remove key type mismatch: expected {:?}, got {:?}", expected_key_ty, actual_key_ty));
                }

                env.insert(success_dst.clone(), Type::Bool);
            }
            else {
                return Err(format!("Cannot remove from non-HashMap type {:?}", map_ty));
            }

            Ok(())
        }
        Inst::HashMapContains { dst, map, key, key_ty: _, value_ty: _ } => {
            let map_ty = env.get(map)
                .ok_or_else(|| format!("HashMap register r{} not found", map.0))?;

            if let Type::HashMap(expected_key_ty, _) = map_ty {
                let actual_key_ty = infer_value_type(key, env)?;
                if !types_compatible(expected_key_ty, &actual_key_ty) {
                    return Err(format!("HashMap contains key type mismatch: expected {:?}, got {:?}", expected_key_ty, actual_key_ty));
                }

                env.insert(dst.clone(), Type::Bool);
            }
            else {
                return Err(format!("Cannot check contains on non-HashMap type {:?}", map_ty));
            }

            Ok(())
        }
        Inst::HashMapLen { dst, map } => {
            let map_ty = env.get(map)
                .ok_or_else(|| format!("HashMap register r{} not found", map.0))?;

            if !matches!(map_ty, Type::HashMap(_, _)) {
                return Err(format!(
                    "Cannot get len of non-HashMap type {:?}",
                    map_ty
                ));
            }

            env.insert(dst.clone(), Type::I64);

            Ok(())
        }
        Inst::HashMapCap { dst, map } => {
            let map_ty = env.get(map)
                .ok_or_else(|| format!("HashMap register r{} not found", map.0))?;

            if !matches!(map_ty, Type::HashMap(_, _)) {
                return Err(format!("Cannot get capacity of non-HashMap type {:?}", map_ty));
            }

            env.insert(dst.clone(), Type::I64);

            Ok(())
        }
        Inst::HashMapClear { map } => {
            let map_ty = env.get(map)
                .ok_or_else(|| format!("HashMap register r{} not found", map.0))?;

            if !matches!(map_ty, Type::HashMap(_, _)) {
                return Err(format!(
                    "Cannot clear non-HashMap type {:?}",
                    map_ty
                ));
            }

            Ok(())
        }
        Inst::SysWrite { fd, buf, len, result_dst } => {
            let fd_ty = infer_value_type(fd, env)?;
            if !is_numeric(&fd_ty) {
                return Err(format!("SysWrite in {}: File descriptor must be numeric. got {:?}", func_name, fd_ty));
            }

            if !env.contains_key(buf) {
                return Err(format!("SysWrite in {}: Buffer register r{} not found", func_name, buf.0));
            }

            // Allowing unknown rn for flexibility but will change later
            let buf_ty = env.get(buf).cloned().unwrap_or(Type::Unknown);
            if !matches!(buf_ty, Type::String | Type::Ptr(_) | Type::Unknown | Type::Vec(_)) {
                return Err(format!("SysWrite in {}: Buffer should be String or pointer type, got {:?}", func_name, buf_ty));
            }

            let len_ty = infer_value_type(len, env)?;
            if !is_numeric(&len_ty) {
                return Err(format!("SysWrite in {}: Length must be numeric, got {:?}", func_name, len_ty));
            }

            env.insert(result_dst.clone(), Type::I64);

            Ok(())
        }
        Inst::SysRead { fd, buf, len, result_dst } => {
            let fd_ty = infer_value_type(fd, env)?;
            if !is_numeric(&fd_ty) {
                return Err(format!("SysRead in {}: File descriptor must be numeric, got {:?}", func_name, fd_ty));
            }

            if !env.contains_key(buf) {
                return Err(format!("SysRead in {}: Buffer register r{} not found", func_name, buf.0));
            }

            let len_ty = infer_value_type(len, env)?;
            if !is_numeric(&len_ty) {
                return Err(format!("SysRead in {}: Length must be numeric, got {:?}", func_name, len_ty));
            }

            env.insert(result_dst.clone(), Type::I64);

            Ok(())
        }
        Inst::SysOpen { path, flags, mode, result_dst } => {
            if !env.contains_key(path) {
                return Err(format!("SysOpen in {}: Path register r{} not found", func_name, path.0));
            }

            // Allowing unknown rn for flexibility but will change later
            let path_ty = env.get(path).cloned().unwrap_or(Type::Unknown);
            if !matches!(path_ty, Type::String | Type::Ptr(_) | Type::Unknown) {
                return Err(format!("SysOpen in {}: Path should be String, got {:?}", func_name, path_ty));
            }

            let flags_ty = infer_value_type(flags, env)?;
            if !is_numeric(&flags_ty) {
                return Err(format!("SysOpen in {}: Flags must be numeric, got {:?}", func_name, flags_ty));
            }

            let mode_ty = infer_value_type(mode, env)?;
            if !is_numeric(&mode_ty) {
                return Err(format!("SysOpen in {}: Mode must be numeric, got {:?}", func_name, mode_ty));
            }

            env.insert(result_dst.clone(), Type::I32);

            Ok(())
        }
        Inst::SysClose { fd, result_dst } => {
            let fd_ty = infer_value_type(fd, env)?;
            if !is_numeric(&fd_ty) {
                return Err(format!("SysClose in {}: File descriptor must be numeric, got {:?}", func_name, fd_ty));
            }

            env.insert(result_dst.clone(), Type::I32);

            Ok(())
        }
        Inst::Print { value } | Inst::Println { value } | Inst::Eprint { value } | Inst::Eprintln { value } => {
            if !env.contains_key(value) {
                return Err(format!("Print in {}: Value register r{} not found", func_name, value.0));
            }

            // Allowing unknown rn for flexibility but will change later
            let val_ty = env.get(value).cloned().unwrap_or(Type::Unknown);
            if !matches!(val_ty, Type::String | Type::Unknown) {
                return Err(format!(
                    "Print in {}: Value should be a String, got {:?}. \
                    Suggestion: Use a format function to convert other types to String.",
                    func_name, val_ty
                ));
            }

            Ok(())
        }
        Inst::ReadLine { dst } => {
            env.insert(dst.clone(), Type::String);

            Ok(())
        }
        Inst::IntToString { dst, value } => {
            let val_ty = infer_value_type(value, env)?;
            if !is_numeric(&val_ty) {
                return Err(format!("IntToString in {}: Value must be numeric, got {:?}", func_name, val_ty));
            }

            env.insert(dst.clone(), Type::String);

            Ok(())
        }
        Inst::SysSeek { fd, offset, whence, result_dst } => {
            let fd_ty = infer_value_type(fd, env)?;
            if !is_numeric(&fd_ty) {
                return Err(format!(
                    "SysSeek in {}: fd must be numeric, got {:?}",
                    func_name, fd_ty
                ));
            }
            
            let offset_ty = infer_value_type(offset, env)?;
            if !is_numeric(&offset_ty) {
                return Err(format!(
                    "SysSeek in {}: offset must be numeric, got {:?}",
                    func_name, offset_ty
                ));
            }
            
            let whence_ty = infer_value_type(whence, env)?;
            if !is_numeric(&whence_ty) {
                return Err(format!(
                    "SysSeek in {}: whence must be numeric, got {:?}",
                    func_name, whence_ty
                ));
            }
            
            env.insert(result_dst.clone(), Type::I64);
            
            Ok(())
        }
        
        Inst::SysStat { path, stat_buf, result_dst } => {
            if !env.contains_key(path) {
                return Err(format!(
                    "SysStat in {}: path register r{} not found",
                    func_name, path.0
                ));
            }
            
            let path_ty = env.get(path).cloned().unwrap_or(Type::Unknown);
            if !matches!(path_ty, Type::String | Type::Unknown) {
                return Err(format!(
                    "SysStat in {}: path should be String, got {:?}",
                    func_name, path_ty
                ));
            }
            
            // Could define statbuf struct later
            env.insert(stat_buf.clone(), Type::Ptr(Box::new(Type::Unknown)));
            
            env.insert(result_dst.clone(), Type::I64);
            
            Ok(())
        }
        
        Inst::SysFstat { fd, stat_buf, result_dst } => {
            let fd_ty = infer_value_type(fd, env)?;
            if !is_numeric(&fd_ty) {
                return Err(format!(
                    "SysFstat in {}: fd must be numeric, got {:?}",
                    func_name, fd_ty
                ));
            }
            
            env.insert(stat_buf.clone(), Type::Ptr(Box::new(Type::Unknown)));

            env.insert(result_dst.clone(), Type::I64);
            
            Ok(())
        }
        
        Inst::SysMkdir { path, mode, result_dst } => {
            if !env.contains_key(path) {
                return Err(format!(
                    "SysMkdir in {}: path register r{} not found",
                    func_name, path.0
                ));
            }
            
            let path_ty = env.get(path).cloned().unwrap_or(Type::Unknown);
            if !matches!(path_ty, Type::String | Type::Unknown) {
                return Err(format!(
                    "SysMkdir in {}: path should be String, got {:?}",
                    func_name, path_ty
                ));
            }
            
            let mode_ty = infer_value_type(mode, env)?;
            if !is_numeric(&mode_ty) {
                return Err(format!(
                    "SysMkdir in {}: mode must be numeric, got {:?}",
                    func_name, mode_ty
                ));
            }
            
            env.insert(result_dst.clone(), Type::I64);
            
            Ok(())
        }
        
        Inst::SysRmdir { path, result_dst } => {
            if !env.contains_key(path) {
                return Err(format!(
                    "SysRmdir in {}: path register r{} not found",
                    func_name, path.0
                ));
            }
            
            let path_ty = env.get(path).cloned().unwrap_or(Type::Unknown);
            if !matches!(path_ty, Type::String | Type::Unknown) {
                return Err(format!(
                    "SysRmdir in {}: path should be String, got {:?}",
                    func_name, path_ty
                ));
            }
            
            env.insert(result_dst.clone(), Type::I64);
            
            Ok(())
        }
        
        Inst::SysUnlink { path, result_dst } => {
            if !env.contains_key(path) {
                return Err(format!(
                    "SysUnlink in {}: path register r{} not found",
                    func_name, path.0
                ));
            }
            
            let path_ty = env.get(path).cloned().unwrap_or(Type::Unknown);
            if !matches!(path_ty, Type::String | Type::Unknown) {
                return Err(format!(
                    "SysUnlink in {}: path should be String, got {:?}",
                    func_name, path_ty
                ));
            }
            
            env.insert(result_dst.clone(), Type::I64);
            
            Ok(())
        }
        
        Inst::SysRename { old_path, new_path, result_dst } => {
            if !env.contains_key(old_path) {
                return Err(format!(
                    "SysRename in {}: old_path register r{} not found",
                    func_name, old_path.0
                ));
            }

            let old_path_ty = env.get(old_path).cloned().unwrap_or(Type::Unknown);
            if !matches!(old_path_ty, Type::String | Type::Unknown) {
                return Err(format!(
                    "SysRename in {}: old_path should be String, got {:?}",
                    func_name, old_path_ty
                ));
            }
            
            if !env.contains_key(new_path) {
                return Err(format!(
                    "SysRename in {}: new_path register r{} not found",
                    func_name, new_path.0
                ));
            }

            let new_path_ty = env.get(new_path).cloned().unwrap_or(Type::Unknown);
            if !matches!(new_path_ty, Type::String | Type::Unknown) {
                return Err(format!(
                    "SysRename in {}: new_path should be String, got {:?}",
                    func_name, new_path_ty
                ));
            }
            
            env.insert(result_dst.clone(), Type::I64);
            
            Ok(())
        }
        
        Inst::SysAccess { path, mode, result_dst } => {
            if !env.contains_key(path) {
                return Err(format!(
                    "SysAccess in {}: path register r{} not found",
                    func_name, path.0
                ));
            }
            
            let path_ty = env.get(path).cloned().unwrap_or(Type::Unknown);
            if !matches!(path_ty, Type::String | Type::Unknown) {
                return Err(format!(
                    "SysAccess in {}: path should be String, got {:?}",
                    func_name, path_ty
                ));
            }
            
            let mode_ty = infer_value_type(mode, env)?;
            if !is_numeric(&mode_ty) {
                return Err(format!(
                    "SysAccess in {}: mode must be numeric, got {:?}",
                    func_name, mode_ty
                ));
            }
            
            env.insert(result_dst.clone(), Type::I64);
            
            Ok(())
        }
        
        Inst::SysGetcwd { dst } => {
            env.insert(dst.clone(), Type::String);
            
            Ok(())
        }
        
        Inst::SysChdir { path, result_dst } => {
            if !env.contains_key(path) {
                return Err(format!(
                    "SysChdir in {}: path register r{} not found",
                    func_name, path.0
                ));
            }
            
            let path_ty = env.get(path).cloned().unwrap_or(Type::Unknown);
            if !matches!(path_ty, Type::String | Type::Unknown) {
                return Err(format!(
                    "SysChdir in {}: path should be String, got {:?}",
                    func_name, path_ty
                ));
            }
            
            env.insert(result_dst.clone(), Type::I64);
            
            Ok(())
        }
        
        Inst::FileReadToString { dst, path, result_dst } => {
            if !env.contains_key(path) {
                return Err(format!(
                    "FileReadToString in {}: path register r{} not found",
                    func_name, path.0
                ));
            }
            
            let path_ty = env.get(path).cloned().unwrap_or(Type::Unknown);
            if !matches!(path_ty, Type::String | Type::Unknown) {
                return Err(format!(
                    "FileReadToString in {}: path should be String, got {:?}",
                    func_name, path_ty
                ));
            }
            
            env.insert(dst.clone(), Type::String);
            
            env.insert(result_dst.clone(), Type::I64);
            
            Ok(())
        }
        
        Inst::FileWriteString { path, content, result_dst } => {
            if !env.contains_key(path) {
                return Err(format!(
                    "FileWriteString in {}: path register r{} not found",
                    func_name, path.0
                ));
            }

            let path_ty = env.get(path).cloned().unwrap_or(Type::Unknown);
            if !matches!(path_ty, Type::String | Type::Unknown) {
                return Err(format!(
                    "FileWriteString in {}: path should be String, got {:?}",
                    func_name, path_ty
                ));
            }
            
            if !env.contains_key(content) {
                return Err(format!(
                    "FileWriteString in {}: content register r{} not found",
                    func_name, content.0
                ));
            }

            let content_ty = env.get(content).cloned().unwrap_or(Type::Unknown);
            if !matches!(content_ty, Type::String | Type::Unknown) {
                return Err(format!(
                    "FileWriteString in {}: content should be String, got {:?}",
                    func_name, content_ty
                ));
            }
            
            env.insert(result_dst.clone(), Type::I64);
            
            Ok(())
        }
        
        Inst::FileAppendString { path, content, result_dst } => {
            if !env.contains_key(path) {
                return Err(format!(
                    "FileAppendString in {}: path register r{} not found",
                    func_name, path.0
                ));
            }

            let path_ty = env.get(path).cloned().unwrap_or(Type::Unknown);
            if !matches!(path_ty, Type::String | Type::Unknown) {
                return Err(format!(
                    "FileAppendString in {}: path should be String, got {:?}",
                    func_name, path_ty
                ));
            }
            
            if !env.contains_key(content) {
                return Err(format!(
                    "FileAppendString in {}: content register r{} not found",
                    func_name, content.0
                ));
            }

            let content_ty = env.get(content).cloned().unwrap_or(Type::Unknown);
            if !matches!(content_ty, Type::String | Type::Unknown) {
                return Err(format!(
                    "FileAppendString in {}: content should be String, got {:?}",
                    func_name, content_ty
                ));
            }
            
            env.insert(result_dst.clone(), Type::I64);
            
            Ok(())
        }
        Inst::OptionSome { dst, value, inner_ty } => {
            let val_ty = infer_value_type(value, env)?;
            
            if !types_compatible(inner_ty, &val_ty) && !matches!(inner_ty, Type::Unknown) {
                return Err(format!(
                    "Option::Some type mismatch in {}: expected {:?}, got {:?}",
                    func_name, inner_ty, val_ty
                ));
            }
            
            let actual_inner = if matches!(inner_ty, Type::Unknown) { val_ty } else { inner_ty.clone() };
            env.insert(dst.clone(), Type::Option(Box::new(actual_inner)));
            
            Ok(())
        }
        Inst::OptionNone { dst, inner_ty } => {
            env.insert(dst.clone(), Type::Option(Box::new(inner_ty.clone())));
            Ok(())
        }
        Inst::OptionIsSome { dst, option } => {
            let opt_ty = env.get(option)
                .ok_or_else(|| format!("Option register r{} not found in {}", option.0, func_name))?;
            
            if !opt_ty.is_option() {
                return Err(format!(
                    "OptionIsSome in {}: expected Option type, got {:?}",
                    func_name, opt_ty
                ));
            }
            
            env.insert(dst.clone(), Type::Bool);
            Ok(())
        }
        Inst::OptionIsNone { dst, option } => {
            let opt_ty = env.get(option)
                .ok_or_else(|| format!("Option register r{} not found in {}", option.0, func_name))?;
            
            if !opt_ty.is_option() {
                return Err(format!(
                    "OptionIsNone in {}: expected Option type, got {:?}",
                    func_name, opt_ty
                ));
            }
            
            env.insert(dst.clone(), Type::Bool);
            Ok(())
        }
        Inst::OptionUnwrap { dst, option, inner_ty: _ } => {
            let opt_ty = env.get(option)
                .ok_or_else(|| format!("Option register r{} not found in {}", option.0, func_name))?;
            
            if let Type::Option(inner) = opt_ty {
                env.insert(dst.clone(), (**inner).clone());
            } else {
                return Err(format!(
                    "OptionUnwrap in {}: expected Option type, got {:?}",
                    func_name, opt_ty
                ));
            }
            
            Ok(())
        }
        Inst::OptionUnwrapOr { dst, option, default, inner_ty: _ } => {
            let opt_ty = env.get(option)
                .ok_or_else(|| format!("Option register r{} not found in {}", option.0, func_name))?;
            let default_ty = infer_value_type(default, env)?;
            
            if let Type::Option(inner) = opt_ty {
                if !types_compatible(inner, &default_ty) {
                    return Err(format!(
                        "OptionUnwrapOr in {}: default type mismatch, expected {:?}, got {:?}",
                        func_name, inner, default_ty
                    ));
                }
                env.insert(dst.clone(), (**inner).clone());
            } else {
                return Err(format!(
                    "OptionUnwrapOr in {}: expected Option type, got {:?}",
                    func_name, opt_ty
                ));
            }
            
            Ok(())
        }
        Inst::OptionUnwrapOrElse { dst, option, default_fn: _, inner_ty: _ } => {
            let opt_ty = env.get(option)
                .ok_or_else(|| format!("Option register r{} not found in {}", option.0, func_name))?;
            
            if let Type::Option(inner) = opt_ty {
                env.insert(dst.clone(), (**inner).clone());
            } else {
                return Err(format!(
                    "OptionUnwrapOrElse in {}: expected Option type, got {:?}",
                    func_name, opt_ty
                ));
            }
            
            Ok(())
        }
        Inst::OptionMap { dst, option, map_fn: _, input_ty: _, output_ty } => {
            let opt_ty = env.get(option)
                .ok_or_else(|| format!("Option register r{} not found in {}", option.0, func_name))?;
            
            if !opt_ty.is_option() {
                return Err(format!(
                    "OptionMap in {}: expected Option type, got {:?}",
                    func_name, opt_ty
                ));
            }
            
            env.insert(dst.clone(), Type::Option(Box::new(output_ty.clone())));
            Ok(())
        }
        Inst::OptionMatch { option, value_dst, some_label: _, none_label: _, inner_ty: _ } => {
            let opt_ty = env.get(option)
                .ok_or_else(|| format!("Option register r{} not found in {}", option.0, func_name))?;
            
            if let Type::Option(inner) = opt_ty {
                if let Some(val_dst) = value_dst {
                    env.insert(val_dst.clone(), (**inner).clone());
                }
            } else {
                return Err(format!(
                    "OptionMatch in {}: expected Option type, got {:?}",
                    func_name, opt_ty
                ));
            }
            
            Ok(())
        }
        Inst::ResultOk { dst, value, ok_ty, err_ty } => {
            let val_ty = infer_value_type(value, env)?;
            
            if !types_compatible(ok_ty, &val_ty) && !matches!(ok_ty, Type::Unknown) {
                return Err(format!(
                    "Result::Ok type mismatch in {}: expected {:?}, got {:?}",
                    func_name, ok_ty, val_ty
                ));
            }
            
            let actual_ok = if matches!(ok_ty, Type::Unknown) { val_ty } else { ok_ty.clone() };
            env.insert(dst.clone(), Type::Result(Box::new(actual_ok), Box::new(err_ty.clone())));
            
            Ok(())
        }
        Inst::ResultErr { dst, error, ok_ty, err_ty } => {
            let err_val_ty = infer_value_type(error, env)?;
            
            if !types_compatible(err_ty, &err_val_ty) && !matches!(err_ty, Type::Unknown) {
                return Err(format!(
                    "Result::Err type mismatch in {}: expected {:?}, got {:?}",
                    func_name, err_ty, err_val_ty
                ));
            }
            
            let actual_err = if matches!(err_ty, Type::Unknown) { err_val_ty } else { err_ty.clone() };
            env.insert(dst.clone(), Type::Result(Box::new(ok_ty.clone()), Box::new(actual_err)));
            
            Ok(())
        }
        Inst::ResultIsOk { dst, result } => {
            let res_ty = env.get(result)
                .ok_or_else(|| format!("Result register r{} not found in {}", result.0, func_name))?;
            
            if !res_ty.is_result() {
                return Err(format!(
                    "ResultIsOk in {}: expected Result type, got {:?}",
                    func_name, res_ty
                ));
            }
            
            env.insert(dst.clone(), Type::Bool);
            Ok(())
        }
        Inst::ResultIsErr { dst, result } => {
            let res_ty = env.get(result)
                .ok_or_else(|| format!("Result register r{} not found in {}", result.0, func_name))?;
            
            if !res_ty.is_result() {
                return Err(format!(
                    "ResultIsErr in {}: expected Result type, got {:?}",
                    func_name, res_ty
                ));
            }
            
            env.insert(dst.clone(), Type::Bool);
            Ok(())
        }
        Inst::ResultUnwrap { dst, result, ok_ty: _ } => {
            let res_ty = env.get(result)
                .ok_or_else(|| format!("Result register r{} not found in {}", result.0, func_name))?;
            
            if let Type::Result(ok, _) = res_ty {
                env.insert(dst.clone(), (**ok).clone());
            } else {
                return Err(format!(
                    "ResultUnwrap in {}: expected Result type, got {:?}",
                    func_name, res_ty
                ));
            }
            
            Ok(())
        }
        Inst::ResultUnwrapErr { dst, result, err_ty: _ } => {
            let res_ty = env.get(result)
                .ok_or_else(|| format!("Result register r{} not found in {}", result.0, func_name))?;
            
            if let Type::Result(_, err) = res_ty {
                env.insert(dst.clone(), (**err).clone());
            } else {
                return Err(format!(
                    "ResultUnwrapErr in {}: expected Result type, got {:?}",
                    func_name, res_ty
                ));
            }
            
            Ok(())
        }
        Inst::ResultUnwrapOr { dst, result, default, ok_ty: _ } => {
            let res_ty = env.get(result)
                .ok_or_else(|| format!("Result register r{} not found in {}", result.0, func_name))?;
            let default_ty = infer_value_type(default, env)?;
            
            if let Type::Result(ok, _) = res_ty {
                if !types_compatible(ok, &default_ty) {
                    return Err(format!(
                        "ResultUnwrapOr in {}: default type mismatch, expected {:?}, got {:?}",
                        func_name, ok, default_ty
                    ));
                }
                env.insert(dst.clone(), (**ok).clone());
            } else {
                return Err(format!(
                    "ResultUnwrapOr in {}: expected Result type, got {:?}",
                    func_name, res_ty
                ));
            }
            
            Ok(())
        }
        Inst::ResultExpect { dst, result, message: _, ok_ty: _ } => {
            let res_ty = env.get(result)
                .ok_or_else(|| format!("Result register r{} not found in {}", result.0, func_name))?;
            
            if let Type::Result(ok, _) = res_ty {
                env.insert(dst.clone(), (**ok).clone());
            } else {
                return Err(format!(
                    "ResultExpect in {}: expected Result type, got {:?}",
                    func_name, res_ty
                ));
            }
            
            Ok(())
        }
        Inst::ResultMap { dst, result, map_fn: _, input_ty: _, output_ty, err_ty } => {
            let res_ty = env.get(result)
                .ok_or_else(|| format!("Result register r{} not found in {}", result.0, func_name))?;
            
            if !res_ty.is_result() {
                return Err(format!(
                    "ResultMap in {}: expected Result type, got {:?}",
                    func_name, res_ty
                ));
            }
            
            env.insert(dst.clone(), Type::Result(Box::new(output_ty.clone()), Box::new(err_ty.clone())));
            Ok(())
        }
        Inst::ResultMapErr { dst, result, map_fn: _, ok_ty, input_err_ty: _, output_err_ty } => {
            let res_ty = env.get(result)
                .ok_or_else(|| format!("Result register r{} not found in {}", result.0, func_name))?;
            
            if !res_ty.is_result() {
                return Err(format!(
                    "ResultMapErr in {}: expected Result type, got {:?}",
                    func_name, res_ty
                ));
            }
            
            env.insert(dst.clone(), Type::Result(Box::new(ok_ty.clone()), Box::new(output_err_ty.clone())));
            Ok(())
        }
        Inst::ResultMatch { result, ok_dst, err_dst, ok_label: _, err_label: _, ok_ty: _, err_ty: _ } => {
            let (ok_type, err_type) = {
                let res_ty = env.get(result)
                    .ok_or_else(|| format!("Result register r{} not found in {}", result.0, func_name))?;
                
                if let Type::Result(ok, err) = res_ty {
                    ((**ok).clone(), (**err).clone())
                }
                else {
                    return Err(format!(
                        "ResultMatch in {}: expected Result type, got {:?}",
                        func_name, res_ty
                    ));
                }
            };

            if let Some(ok_d) = ok_dst {
                env.insert(ok_d.clone(), ok_type);
            }
            if let Some(err_d) = err_dst {
                env.insert(err_d.clone(), err_type);
            }
            
            Ok(())
        }
        Inst::ResultTry { dst, result, ok_ty: _, err_ty: _, error_return_label: _ } => {
            let res_ty = env.get(result)
                .ok_or_else(|| format!("Result register r{} not found in {}", result.0, func_name))?;
            
            if let Type::Result(ok, _) = res_ty {
                env.insert(dst.clone(), (**ok).clone());
            } else {
                return Err(format!(
                    "ResultTry in {}: expected Result type, got {:?}",
                    func_name, res_ty
                ));
            }
            
            Ok(())
        }
        Inst::Panic { message } => {
            if !env.contains_key(message) {
                return Err(format!(
                    "Panic in {}: message register r{} not found",
                    func_name, message.0
                ));
            }
            
            let msg_ty = env.get(message).cloned().unwrap_or(Type::Unknown);
            if !matches!(msg_ty, Type::String | Type::Unknown) {
                return Err(format!(
                    "Panic in {}: message should be String, got {:?}",
                    func_name, msg_ty
                ));
            }
            
            Ok(())
        }
        Inst::PanicStatic { .. } => {
            Ok(())
        }
        Inst::Unreachable => {
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

    if is_numeric(t1) && is_numeric(t2) {
        return true;
    }

    if let (Type::HashMap(k1, v1), Type::HashMap(k2, v2)) = (t1, t2) {
        return types_compatible(&k1, k2) && types_compatible(&v1, v2);
    }

    if let (Type::Option(inner1), Type::Option(inner2)) = (t1, t2) {
        return types_compatible(inner1, inner2);
    }

    if let (Type::Result(ok1, err1), Type::Result(ok2, err2)) = (t1, t2) {
        return types_compatible(ok1, ok2) && types_compatible(err1, err2);
    }

    if let (Type::Vec(inner1), Type::Vec(inner2)) = (t1, t2) {
        return types_compatible(inner1, inner2);
    }

    if let (Type::Array(inner1, size1), Type::Array(inner2, size2)) = (t1, t2) {
        return size1 == size2 && types_compatible(inner1, inner2);
    }

    if let (Type::Slice(inner1), Type::Slice(inner2)) = (t1, t2) {
        return types_compatible(inner1, inner2);
    }

    if let (Type::Ptr(inner1), Type::Ptr(inner2)) = (t1, t2) {
        return types_compatible(inner1, inner2);
    }

    // Add more compatibility rules later
    false
}

fn is_numeric(ty: &Type) -> bool {
    matches!(ty, Type::I32 | Type::I64 | Type::U32 | Type::U64 | Type::F32 | Type::F64)
}

fn is_hashable(ty: &Type) -> bool {
    match ty {
        Type::I32 | Type::I64 | Type::U32 | Type::U64 => true,

        Type::String => true,

        Type::Bool => true,

        Type::F32 | Type::F64 => false,

        // Support this later using recursive hashing
        Type::Struct(_) | Type::Array(_, _) | Type::Tuple(_) => false,

        Type::Unknown => true,

        _ => false
    }
}