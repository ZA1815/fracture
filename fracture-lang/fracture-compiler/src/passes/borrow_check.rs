use fracture_ir::{Program, Function, Inst, Reg};
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct BorrowState {
    immutable_borrows: HashMap<Reg, Vec<Reg>>,
    mutable_borrows: HashMap<Reg, Option<Reg>>
}

impl BorrowState {
    fn new() -> Self {
        Self {
            immutable_borrows: HashMap::new(),
            mutable_borrows: HashMap::new()
        }
    }

    fn begin_borrow(&mut self, reg: &Reg, borrower: &Reg, is_mut: bool) -> Result<(), String> {
        if is_mut {
            if let Some(borrows) = self.immutable_borrows.get(reg) {
                if !borrows.is_empty() {
                    // Replace with actual variable name later
                    return Err(format!(
                        "Cannot mutably borrow r{} while it has {} immutable borrow(s)",
                        reg.0, borrows.len()
                    ));
                }
            }
            if let Some(Some(existing)) = self.mutable_borrows.get(reg) {
                // Replace with actual variable name and borrow location later
                return Err(format!(
                    "Cannot mutably borrow r{} - already mutably borrowed by r{}",
                    reg.0, existing.0
                ));
            }
            self.mutable_borrows.insert(reg.clone(), Some(borrower.clone()));
        }
        else {
            if let Some(Some(existing)) = self.mutable_borrows.get(reg) {
                // Replace with actual variable name and borrow location later
                return Err(format!(
                    "Cannot immutably borrow r{} - already borrowed mutably by r{}",
                    reg.0, existing.0
                ));
            }
            self.immutable_borrows.entry(reg.clone()).or_insert_with(Vec::new).push(borrower.clone());
        }

        Ok(())
    }

    fn end_borrow(&mut self, reg: &Reg) {
        self.immutable_borrows.remove(reg);
        self.mutable_borrows.remove(reg);
    }
}

pub fn check(program: &Program) -> Result<(), String> {
    for (name, func) in &program.functions {
        if func.is_unsafe() {
            println!("  Skipping borrow check for #[unsafe] function: {}", name);
            continue;
        }

        check_function(name, func)?;
    }

    Ok(())
}

fn check_function(name: &str, func: &Function) -> Result<(), String> {
    let mut state = BorrowState::new();

    for inst in &func.body {
        match inst {
            Inst::BeginBorrow { reg, is_mut } => {
                // Complicated logic, implement later
            }
            Inst::EndBorrow { reg } => {
                state.end_borrow(reg);
            }
            _ => {
                // Check if instruction uses any borrowed registers
            }
        }
    }

    Ok(())
}