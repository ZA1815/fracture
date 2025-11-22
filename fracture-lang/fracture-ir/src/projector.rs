use crate::hsir::*;
use crate::syntax_config::*;
use crate::lexer::*;
use std::collections::HashMap;

pub struct SyntaxProjector {
    config: SyntaxConfig,
    lexer: Lexer,
    current: Token,

    var_types: HashMap<String, Type>,
    next_reg: u32,
    var_regs: HashMap<String, Reg>
}

impl SyntaxProjector {
    pub fn new(input: &str, config: SyntaxConfig) -> Self {
        let mut lexer = Lexer::new(input, config.clone());
        let current = lexer.next_token();

        Self {
            config,
            lexer,
            current,
            var_types: HashMap::new(),
            next_reg: 0,
            var_regs: HashMap::new()
        }
    }

    pub fn project_to_hsir(&mut self) -> Result<Program, String> {
        let mut program = Program {
            functions: HashMap::new(),
            entry: "main".to_string()
        };

        while self.current != Token::Eof {
            if self.current == Token::Function {
                let func = self.parse_function()?;
                program.functions.insert(func.name.clone(), func);
            }
            else {
                self.advance();
            }
        }

        if !program.functions.contains_key("main") {
            // Wrap all top-level statements into main automatically
        }

        Ok(program)
    }

    fn parse_function(&mut self) -> Result<Function, String> {
        self.var_regs.clear();
        self.var_types.clear();

        self.expect(Token::Function)?;

        let name = self.expect_ident()?;

        self.expect(Token::LeftParentheses)?;

        // Parse params later

        self.expect(Token::RightParentheses)?;

        let return_type = if self.current == Token::Arrow {
            self.advance();
            self.parse_type()?
        }
        else {
            Type::Void
        };

        let body = self.parse_block()?;

        let mut locals: HashMap<Reg, Type> = HashMap::new();

        for (var_name, reg) in &self.var_regs {
            if let Some(ty) = self.var_types.get(var_name) {
                locals.insert(reg.clone(), ty.clone());
            }
        }

        Ok(Function {
            name,
            params: vec![],
            return_type,
            body,
            locals
        })
    }

    fn parse_block(&mut self) -> Result<Vec<Inst>, String> {
        let mut instructions = Vec::new();

        match self.config.style.block_style {
            BlockStyle::Braces => {
                self.expect(Token::LeftBrace)?;

                while self.current != Token::RightBrace && self.current != Token::Eof {
                    instructions.extend(self.parse_statement()?);
                }

                self.expect(Token::RightBrace)?;
            }
            BlockStyle::Indentation => {
                self.expect(Token::Colon)?;
                self.skip_newlines();
                self.expect(Token::Indent)?;

                while self.current != Token::Dedent && self.current != Token::Eof {
                    instructions.extend(self.parse_statement()?);
                    self.skip_newlines();
                }

                if self.current == Token::Dedent {
                    self.advance();
                }
            }
            _ => return Err("Block style not implemented yet".to_string())
        }

        Ok(instructions)
    }

    fn parse_statement(&mut self) -> Result<Vec<Inst>, String> {
        let mut instructions = Vec::new();

        match &self.current {
            Token::Let | Token::Ident(_) => {
                let is_declaration = self.current == Token::Let;
                if is_declaration {
                    self.advance();
                    if self.current == Token::Mut {
                        self.advance();
                    }
                }

                let var_name = self.expect_ident()?;

                let var_type = if self.current == Token::Colon {
                    self.advance();
                    self.parse_type()?
                }
                else {
                    Type::Unknown
                };

                self.expect(Token::Equals)?;

                let (expr_insts, result_reg) = self.parse_expression()?;
                instructions.extend(expr_insts);

                let var_reg = self.get_or_create_var(&var_name, &var_type);
                instructions.push(Inst::Move {
                    dst: var_reg,
                    src: Value::Reg(result_reg),
                    ty: var_type
                });
            }
            Token::Return => {
                self.advance();
                if self.current != Token::Semicolon && self.current != Token::Newline {
                    let (expr_insts, result_reg) = self.parse_expression()?;
                    instructions.extend(expr_insts);
                    instructions.push(Inst::Return { val: Some(Value::Reg(result_reg)) });
                }
                else {
                    instructions.push(Inst::Return { val: None });
                }
            }
            Token::If => {
                // Placeholder
            }
            _ => {
                let (expr_insts, _) = self.parse_expression()?;
                instructions.extend(expr_insts);
            }
        }

        if self.config.style.needs_semicolon {
            self.expect(Token::Semicolon)?;
        }

        Ok(instructions)
    }

    fn parse_expression(&mut self) -> Result<(Vec<Inst>, Reg), String> {
        let (mut instructions, mut left_reg) = self.parse_term()?;

        while self.current == Token::Plus || self.current == Token::Minus {
            let op = self.current.clone();
            self.advance();

            let (right_insts, right_reg) = self.parse_term()?;
            instructions.extend(right_insts);

            let result_reg = self.alloc_reg();

            let inst = match op {
                Token::Plus => Inst::Add {
                    dst: result_reg.clone(),
                    lhs: Value::Reg(left_reg),
                    rhs: Value::Reg(right_reg),
                    ty: Type::I32 // Need proper type inference
                },
                Token::Minus => Inst::Sub {
                    dst: result_reg.clone(),
                    lhs: Value::Reg(left_reg),
                    rhs: Value::Reg(right_reg),
                    ty: Type::I32 // Need proper type inference
                },
                _ => unreachable!()
            };

            instructions.push(inst);
            left_reg = result_reg;
        }

        Ok((instructions, left_reg))
    }

    fn parse_term(&mut self) -> Result<(Vec<Inst>, Reg), String> {
        let mut instructions = Vec::new();
        let result_reg = self.alloc_reg();

        match &self.current.clone() {
            Token::Number(n) => {
                instructions.push(Inst::Move {
                    dst: result_reg.clone(),
                    src: Value::Const(Const::I32(*n as i32)),
                    ty: Type::I32
                });
                self.advance();
            }
            Token::Ident(name) => {
                if let Some(var_reg) = self.var_regs.get(name) {
                    instructions.push(Inst::Move {
                        dst: result_reg.clone(),
                        src: Value::Reg(var_reg.clone()),
                        ty: self.var_types.get(name).cloned().unwrap_or(Type::Unknown)
                    });
                }
                else {
                    return Err(format!("Unknown variable: {}", name));
                }
                self.advance();
            }
            Token::LeftParentheses => {
                self.advance();
                let (expr_insts, expr_reg) = self.parse_expression()?;
                instructions.extend(expr_insts);
                instructions.push(Inst::Move {
                    dst: result_reg.clone(),
                    src: Value::Reg(expr_reg),
                    ty: Type::Unknown
                });
                self.expect(Token::RightParentheses)?;
            }
            _ => return Err(format!("Unexpected token in expression: {:?}", self.current))
        }

        Ok((instructions, result_reg))
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        let type_name = self.expect_ident()?;

        if type_name == self.config.keywords.int_type {
            Ok(Type::I32)
        }
        else if type_name == self.config.keywords.bool_type {
            Ok(Type::Bool)
        }
        else if type_name == self.config.keywords.string_type {
            Ok(Type::String)
        }
        else {
            Ok(Type::Struct(type_name))
        }
    }

    fn get_or_create_var(&mut self, name: &str, ty: &Type) -> Reg {
        if let Some(reg) = self.var_regs.get(name) {
            reg.clone()
        }
        else {
            let reg = self.alloc_reg();
            self.var_regs.insert(name.to_string(), reg.clone());
            self.var_types.insert(name.to_string(), ty.clone());

            reg
        }
    }

    fn alloc_reg(&mut self) -> Reg {
        let reg = Reg(self.next_reg);
        self.next_reg += 1;

        reg
    }

    fn advance(&mut self) {
        self.current = self.lexer.next_token();

        if !matches!(self.config.style.block_style, BlockStyle::Indentation) {
            while self.current == Token::Newline {
                self.current = self.lexer.next_token();
            }
        }
    }

    fn skip_newlines(&mut self) {
        while self.current == Token::Newline {
            self.current = self.lexer.next_token();
        }
    }

    fn expect(&mut self, token: Token) -> Result<(), String> {
        if std::mem::discriminant(&self.current) == std::mem::discriminant(&token) {
            self.advance();

            Ok(())
        }
        else {
            Err(format!("Expected {:?}, got {:?}", token, self.current))
        }
    }

    fn expect_ident(&mut self) -> Result<String, String> {
        match &self.current {
            Token::Ident(name) => {
                let n = name.clone();
                self.advance();
                
                Ok(n)
            }
            _ => Err(format!("Expected identifier, got {:?}", self.current))
        }
    }
}