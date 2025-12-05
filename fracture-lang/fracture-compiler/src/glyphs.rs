use crate::passes;
use fracture_ir::Program;
use std::collections::HashMap;

pub type GlyphFn = fn(&Program) -> Result<(), String>;

pub struct GlyphRegistry {
    registry: HashMap<String, GlyphFn>
}

impl GlyphRegistry {
    pub fn new() -> Self {
        let mut reg = Self {
            registry: HashMap::new(),
        };

        reg.register("type_check", passes::type_check::check);
        reg.register("std::type_check", passes::type_check::check);

        reg.register("borrow_check", passes::borrow_check::check);
        reg.register("std::borrow_check", passes::borrow_check::check);

        reg
    }

    pub fn register(&mut self, name: &str, func: GlyphFn) {
        self.registry.insert(name.to_string(), func);
    }

    pub fn get(&self, name: &str) -> Option<GlyphFn> {
        if let Some(func) = self.registry.get(name) {
            return Some(*func);
        }

        if let Some(last_segment) = name.split("::").last() {
            if let Some(func) = self.registry.get(last_segment) {
                return Some(*func);
            }
        }

        if !name.contains("::") {
            let with_std = format!("std::{}", name);
            if let Some(func) = self.registry.get(&with_std) {
                return Some(*func);
            }
        }

        None
    }

    pub fn list_glyphs(&self) -> Vec<String> {
        let mut names: Vec<String> = self.registry.keys().cloned().collect();
        names.sort();
        names
    }
}