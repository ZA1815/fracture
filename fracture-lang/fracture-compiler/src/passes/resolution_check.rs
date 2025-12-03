use fracture_ir::{Program, Module, Function, Inst, Value, ModulePath, PathSegment, Visibility, UseTree};
use std::collections::HashMap;

type ImportMap = HashMap<String, ModulePath>;

pub fn check(program: &Program) -> Result<(), String> {
    let resolver = Resolver::new(program);
    resolver.check_module(&program.root_module, &ModulePath { segments: vec![PathSegment::Shard] })?;

    Ok(())
}

struct Resolver<'a> {
    program: &'a Program
}

impl<'a> Resolver<'a> {
    fn new(program: &'a Program) -> Self {
        Self { program }
    }

    fn check_module(&self, module: &Module, current_path: &ModulePath) -> Result<(), String> {
        let imports = self.resolve_imports(module, current_path)?;

        for func in module.functions.values() {
            self.check_function(func, module, current_path, &imports)?;
        }

        for child in module.children.values() {
            let mut child_path_segments = current_path.segments.clone();
            if current_path.segments.len() == 1 && matches!(current_path.segments[0], PathSegment::Shard) {
                child_path_segments = vec![PathSegment::Shard, PathSegment::Ident(child.name.clone())];
            }
            else {
                child_path_segments.push(PathSegment::Ident(child.name.clone()));
            }

            let child_path = ModulePath { segments: child_path_segments };
            self.check_module(child, &child_path)?;
        }

        Ok(())
    }

    fn resolve_imports(&self, module: &Module, current_mod_path: &ModulePath) -> Result<ImportMap, String> {
        let mut imports = HashMap::new();

        for use_stmt in &module.uses {
            self.expand_use_tree(&use_stmt.tree, current_mod_path, &mut imports)?;
        }

        Ok(imports)
    }

    fn expand_use_tree(&self, tree: &UseTree, current_mod_path: &ModulePath, imports: &mut ImportMap) -> Result<(), String> {
        match tree {
            UseTree::Simple { path, alias } => {
                if let Ok(target_func) = self.resolve_absolute_path(path, current_mod_path) {
                    self.check_visibility(target_func, current_mod_path)?;
                }
                else if let Ok(_) = self.find_module(path, current_mod_path) {
                    // It's a valid module, that's fine
                }
                else {
                    return Err(format!("Could not resolve import: {}", path.to_string()));
                }

                let local_name = if let Some(a) = alias {
                    a.clone()
                }
                else {
                    path.leaf_name()
                        .ok_or_else(|| format!("Cannot import root or empty path: {:?}", path))?
                        .to_string()
                };

                imports.insert(local_name, path.clone());
            }
            UseTree::Nested { path: prefix, items } => {
                for item in items {
                    let combined_tree = self.combine_prefix(prefix, item);
                    let _ = self.expand_use_tree(&combined_tree, current_mod_path, imports);
                }
            }
            UseTree::Glob { path } => {
                let target_module = self.find_module(path, current_mod_path)?;

                for (func_name, func) in &target_module.functions {
                    if func.visibility == Visibility::Public {
                        let mut full_path_segments = path.segments.clone();
                        full_path_segments.push(PathSegment::Ident(func_name.clone()));
                        imports.insert(func_name.clone(), ModulePath { segments: full_path_segments });
                    }
                }
            }
        }

        Ok(())
    }

    fn combine_prefix(&self, prefix: &ModulePath, nested: &UseTree) -> UseTree {
        let mut new_segments = prefix.segments.clone();
        match nested {
            UseTree::Simple { path, alias } => {
                new_segments.extend(path.segments.clone());
                UseTree::Simple { path: ModulePath { segments: new_segments }, alias: alias.clone() }
            }
            UseTree::Nested { path, items } => {
                new_segments.extend(path.segments.clone());
                UseTree::Nested { path: ModulePath { segments: new_segments }, items: items.clone() }
            }
            UseTree::Glob { path } => {
                new_segments.extend(path.segments.clone());
                UseTree::Glob { path: ModulePath { segments: new_segments } }
            }
        }
    }

    fn check_function(&self, func: &Function, _module: &Module, current_mod_path: &ModulePath, imports: &ImportMap) -> Result<(), String> {
        for inst in &func.body {
            if let Inst::Call { func: target_val, ..} = inst {
                if let Value::Label(label) = target_val {
                    let raw_path_str = &label.0;
                    // Change when adding customization to this token
                    let parts: Vec<&str> = raw_path_str.split("::").collect();
                    let path_segments: Vec<PathSegment> = parts.iter().map(|s| match *s {
                        "shard" => PathSegment::Shard,
                        "super" => PathSegment::Super,
                        "self" => PathSegment::SelfKw,
                        id => PathSegment::Ident(id.to_string())
                    }).collect();

                    let call_path = ModulePath { segments: path_segments };

                    let target_func = self.resolve_call_path(&call_path, current_mod_path, imports)
                        .map_err(|e| format!("In function '{}': {}", func.name, e))?;

                    self.check_visibility(target_func, current_mod_path)
                        .map_err(|e| format!("In function '{}': {}", func.name, e))?;
                }
            }
        }

        Ok(())
    }

    fn resolve_call_path(&self, path: &ModulePath, current_mod_path: &ModulePath, imports: &ImportMap) -> Result<&Function, String> {
        if let Some(first) = path.segments.first() {
            if let PathSegment::Ident(name) = first {
                if let Some(imported_path) = imports.get(name) {
                    let mut new_segments = imported_path.segments.clone();
                    new_segments.extend_from_slice(&path.segments[1..]);
                    
                    let new_path = ModulePath { segments: new_segments };
                    return self.resolve_absolute_path(&new_path, current_mod_path);
                }
            }
        }

        let canonical_path = self.canonicalize(path, current_mod_path)?;
        self.resolve_absolute_path(&canonical_path, current_mod_path)
    }

    fn canonicalize(&self, path: &ModulePath, current_mod_path: &ModulePath) -> Result<ModulePath, String> {
        if path.segments.is_empty() {
            return Err("Empty path".to_string());
        }

        match &path.segments[0] {
            PathSegment::Shard => Ok(path.clone()),
            PathSegment::Super => {
                let mut base = current_mod_path.segments.clone();
                let mut rest_idx = 0;

                while rest_idx < path.segments.len() && matches!(path.segments[rest_idx], PathSegment::Super) {
                    if base.is_empty() || (base.len() == 1 && matches!(base[0], PathSegment::Shard)) {
                        return Err("Too many 'super' segments: Cannot go above shard root".to_string());
                    }

                    base.pop();
                    rest_idx += 1;
                }

                base.extend_from_slice(&path.segments[rest_idx..]);

                Ok(ModulePath { segments: base })
            }
            PathSegment::SelfKw => {
                let mut base = current_mod_path.segments.clone();
                base.extend_from_slice(&path.segments[1..]);

                Ok(ModulePath { segments: base })
            }
            PathSegment::Ident(_) => {
                let mut base = current_mod_path.segments.clone();
                base.extend_from_slice(&path.segments);

                Ok(ModulePath { segments: base })
            }
        }
    }

    fn resolve_absolute_path(&self, path: &ModulePath, _context: &ModulePath) -> Result<&Function, String> {
        if let Some(func) = self.program.resolve_function(path) {
            Ok(func)
        }
        else {
            Err(format!("Item not found: {}", path.to_string()))
        }
    }

    fn find_module(&self, path: &ModulePath, current_mod_path: &ModulePath) -> Result<&Module, String> {
        let canon = self.canonicalize(path, current_mod_path)?;

        let mut current = &self.program.root_module;

        let start_idx = if !canon.segments.is_empty() && matches!(canon.segments[0], PathSegment::Shard) { 1 } else { 0 };
        for segment in &canon.segments[start_idx..] {
            match segment {
                PathSegment::Ident(name) => {
                    current = current.children.get(name)
                        .ok_or_else(|| format!("Module not found: {}", name))?;
                }
                _ => return Err(format!("Invalid segment in module path: {:?}", segment))
            }
        }

        Ok(current)
    }

    fn check_visibility(&self, target: &Function, context: &ModulePath) -> Result<(), String> {
        if target.visibility == Visibility::Public {
            return Ok(());
        }

        if target.visibility == Visibility::Shard {
            return Ok(());
        }
        
        let target_mod_path = target.module_path.as_ref()
            .ok_or("Target function missing module path metadata")?;

        let context_str = context.to_string();
        let target_mod_str = target_mod_path.to_string();

        if context_str == target_mod_str {
            return Ok(());
        }

        if context_str.starts_with(&format!("{}::", target_mod_str)) {
            return Ok(());
        }

        // If target is strictly private, it cannot be accessed outside its module
        // (unless we implement friend/pub(super) logic later)
        if target.visibility == Visibility::Private {
            return Err(format!(
                "Function '{}' is private in module '{}' and cannot be accessed from '{}'",
                target.name, target_mod_str, context_str
            ));
        }

        Ok(())
    }
}