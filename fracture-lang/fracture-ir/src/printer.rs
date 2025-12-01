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

pub fn print_module(module: &Module, indent: usize) -> String {
    let pad = "  ".repeat(indent);
    let mut output = String::new();
    
    output.push_str(&format!("{}mod {} ({:?}) {{\n", 
        pad, 
        module.name, 
        module.visibility
    ));
    
    for use_stmt in &module.uses {
        output.push_str(&format!("{}  {:?};\n", pad, use_stmt));
    }
    
    if !module.uses.is_empty() {
        output.push('\n');
    }
    
    for (name, func) in &module.functions {
        output.push_str(&format!("{}  {:?} fn {}(...)\n", 
            pad, 
            func.visibility, 
            name
        ));
    }
    
    for (name, s) in &module.structs {
        output.push_str(&format!("{}  {:?} struct {} {{ ... }}\n", 
            pad, 
            s.visibility, 
            name
        ));
    }
    
    for (_, child) in &module.children {
        output.push_str(&print_module(child, indent + 1));
    }
    
    output.push_str(&format!("{}}}\n", pad));
    
    output
}

pub fn print_path(path: &ModulePath) -> String {
    path.segments.iter()
        .map(|seg| match seg {
            PathSegment::Ident(name) => name.clone(),
            PathSegment::SelfKw => "self".to_string(),
            PathSegment::Super => "super".to_string(),
            PathSegment::Shard => "shard".to_string(),
        })
        .collect::<Vec<_>>()
        .join("::")
}

pub fn print_use_tree(tree: &UseTree) -> String {
    match tree {
        UseTree::Simple { path, alias } => {
            let path_str = print_path(path);
            match alias {
                Some(a) => format!("{} as {}", path_str, a),
                None => path_str
            }
        }
        UseTree::Nested { path, items } => {
            let path_str = print_path(path);
            let items_str = items.iter()
                .map(|item| print_use_tree(item))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}::{{{}}}", path_str, items_str)
        }
        UseTree::Glob { path } => {
            format!("{}::*", print_path(path))
        }
    }
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
        Type::I32 => "i32".to_string(),
        Type::I64 => "i64".to_string(),
        Type::U32 => "u32".to_string(),
        Type::U64 => "u64".to_string(),
        Type::F32 => "f32".to_string(),
        Type::F64 => "f64".to_string(),
        Type::Bool => "bool".to_string(),
        Type::String => "string".to_string(),
        Type::Void => "void".to_string(),
        Type::Unknown => "?".to_string(),
        Type::Vec(inner) => format!("Vec<{}>", print_type(inner)),
        Type::HashMap(k, v) => format!("HashMap<{}, {}>", print_type(k), print_type(v)),
        Type::Array(inner, size) => format!("[{}; {}]", print_type(inner), size),
        Type::Slice(inner) => format!("&[{}]", print_type(inner)),
        Type::Tuple(types) => {
            let types_str = types.iter()
                .map(|t| print_type(t))
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", types_str)
        }
        Type::Ptr(inner) => format!("*{}", print_type(inner)),
        Type::Ref(inner, is_mut) => {
            if *is_mut {
                format!("&mut {}", print_type(inner))
            } else {
                format!("&{}", print_type(inner))
            }
        }
        Type::Struct(name) => name.clone(),
        Type::Function(params, ret) => {
            let params_str = params.iter()
                .map(|t| print_type(t))
                .collect::<Vec<_>>()
                .join(", ");
            format!("fn({}) -> {}", params_str, print_type(ret))
        }
        Type::Future(inner) => format!("Future<{}>", print_type(inner)),
    }
}