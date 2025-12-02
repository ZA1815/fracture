use crate::hsir::*;

pub struct Parser {
    input: Vec<String>,
    pos: usize
}

impl Parser {
    pub fn new(input: &str) -> Self {
        Self {
            input: input.lines().map(|s| s.to_string()).collect(),
            pos: 0
        }
    }

    pub fn parse_inst(&mut self) -> Option<Inst> {
        if self.pos >= self.input.len() {
            return None;
        }

        let line = &self.input[self.pos];
        self.pos += 1;

        let line = line.trim();
        if line.is_empty() || line.starts_with("#") {
            return self.parse_inst();
        }

        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.is_empty() {
            return self.parse_inst();
        }

        match parts[0] {
            "move" => {
                let dst = self.parse_reg(parts[1]);
                let src = self.parse_value(parts[2]);
                let ty = self.parse_type(parts[3]);
                Some(Inst::Move { dst, src, ty })
            }
            "add" => {
                let dst = self.parse_reg(parts[1]);
                let lhs = self.parse_value(parts[2]);
                let rhs = self.parse_value(parts[3]);
                let ty = self.parse_type(parts[4]);
                Some(Inst::Add { dst, lhs, rhs, ty })
            }
            "jump" => {
                let target = Label(parts[1].to_string());
                Some(Inst::Jump { target })
            }
            "call" => {
                let dst = if parts[1] == "_" {
                    None
                }
                else {
                    Some(self.parse_reg(parts[1]))
                };
                let func = Value::Label(Label(parts[2].to_string()));
                let args = vec![];
                let ty = self.parse_type(parts.last().unwrap());
                Some(Inst::Call { dst, func, args, ty })
            }
            _ => None
        }
    }

    fn parse_reg(&self, s: &str) -> Reg {
        if s.starts_with('r') {
            let num = s[1..].parse().unwrap_or(0);
            Reg(num)
        }
        else {
            Reg(0)
        }
    }

    fn parse_value(&self, s: &str) -> Value {
        if s.starts_with('r') {
            Value::Reg(self.parse_reg(s))
        }
        else if let Ok(i) = s.parse::<i32>() {
            Value::Const(Const::I32(i))
        }
        else {
            Value::Label(Label(s.to_string()))
        }
    }

    fn parse_type(&self, s: &str) -> Type {
        match s {
            "i32" => Type::I32,
            "i64" => Type::I64,
            "f32" => Type::F32,
            "f64" => Type::F64,
            "bool" => Type::Bool,
            "string" => Type::String,
            "void" => Type::Void,
            _ => Type::Unknown
        }
    }
}