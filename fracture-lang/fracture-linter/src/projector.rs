use fracture_ir::hsir::*;
use fracture_ir::{SyntaxConfig, syntax_config::BlockStyle};
use crate::lexer::*;
use std::collections::HashMap;

pub struct SyntaxProjector {
    config: SyntaxConfig,
    lexer: Lexer,
    current: Token,
    peek_buffer: Option<Token>,

    var_types: HashMap<String, Type>,
    next_reg: u32,
    var_regs: HashMap<String, Reg>,
    next_label: u32
}

impl SyntaxProjector {
    pub fn new(input: &str, config: SyntaxConfig) -> Self {
        let mut lexer = Lexer::new(input, config.clone());
        let current = lexer.next_token();

        Self {
            config,
            lexer,
            current,
            peek_buffer: None,
            var_types: HashMap::new(),
            next_reg: 0,
            var_regs: HashMap::new(),
            next_label: 0
        }
    }

    pub fn peek(&mut self) -> Token {
        if let Some(token) = &self.peek_buffer {
            return token.clone();
        }

        let next = self.lexer.next_token();
        self.peek_buffer = Some(next.clone());

        next
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
            return Err("Program must have a main function".to_string());
        }

        Ok(program)
    }

    fn parse_function(&mut self) -> Result<Function, String> {
        self.var_regs.clear();
        self.var_types.clear();
        self.next_reg = 0;

        self.expect(Token::Function)?;

        let name = self.expect_ident()?;

        self.expect(Token::LeftParentheses)?;

        let mut params = Vec::new();
        while self.current != Token::RightParentheses {
            let param_name = self.expect_ident()?;

            let param_type = if self.current == Token::Colon {
                self.advance();
                self.parse_type()?
            }
            else {
                Type::Unknown // Infer the type later
            };

            let param_reg = self.alloc_reg();
            self.var_regs.insert(param_name.clone(), param_reg.clone());
            self.var_types.insert(param_name, param_type.clone());
            params.push((param_reg, param_type));

            if self.current == Token::Comma {
                self.advance();
            }
        }

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
            params,
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

        let token = self.current.clone();
        let mut requires_semicolon = true;

        match token {
            Token::Let => {
                self.advance();
                if self.current == Token::Mut {
                    self.advance();
                }

                let var_name = self.expect_ident()?;
                let var_type = if self.current == Token::Colon {
                    self.advance();
                    self.parse_type()?
                }
                else {
                    Type::Unknown
                };

                if self.current == Token::Assignment {
                    self.advance();

                    let (expr_insts, result_reg) = self.parse_expression()?;
                    instructions.extend(expr_insts);

                    let var_reg = self.get_or_create_var(&var_name, &var_type);
                    instructions.push(Inst::Move {
                        dst: var_reg,
                        src: Value::Reg(result_reg),
                        ty: var_type
                    });
                }
                else {
                    // Placeholder for declaring a var with no value
                }
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
                self.advance();
                instructions.extend(self.parse_if_statement()?);
                requires_semicolon = false;
            }
            Token::While => {
                self.advance();
                instructions.extend(self.parse_while_statement()?);
                requires_semicolon = false;
            }
            Token::Ident(name) => {
                let peek_token = self.peek();
                if peek_token == Token::Assignment || peek_token == Token::Colon {
                    let var_name = name.clone();
                    self.advance();
                    let mut var_type = Type::Unknown;
                    if peek_token == Token::Colon {
                        self.advance();
                        var_type = self.parse_type()?;
                    }
                    
                    self.expect(Token::Assignment)?;

                    let (expr_insts, result_reg) = self.parse_expression()?;
                    instructions.extend(expr_insts);

                    let var_reg = self.get_or_create_var(&var_name, &var_type);

                    instructions.push(Inst::Move {
                        dst: var_reg,
                        src: Value::Reg(result_reg),
                        ty: var_type
                    });
                }
                else {
                    let (expr_insts, _) = self.parse_expression()?;
                    instructions.extend(expr_insts);
                }
            }
            _ => {
                let (expr_insts, _) = self.parse_expression()?;
                instructions.extend(expr_insts);
            }
        }

        if self.config.style.needs_semicolon && requires_semicolon {
            self.expect(Token::Semicolon)?;
        }

        Ok(instructions)
    }

    fn parse_if_statement(&mut self) -> Result<Vec<Inst>, String> {
        let mut instructions = Vec::new();

        let (cond_insts, cond_reg) = self.parse_expression()?;
        instructions.extend(cond_insts);

        let else_label = self.alloc_label();
        let end_label = self.alloc_label();

        // Need to add comparison ops
        instructions.push(Inst::JumpIfFalse { cond: Value::Reg(cond_reg), target: else_label.clone() });

        let if_body = self.parse_block()?;
        instructions.extend(if_body);

        instructions.push(Inst::Jump { target: end_label.clone() });

        instructions.push(Inst::Label { target: else_label });

        if self.current == Token::ElseIf {
            self.advance();
            instructions.extend(self.parse_if_statement()?);
        }

        if self.current == Token::Else {
            self.advance();
            let else_body = self.parse_block()?;
            instructions.extend(else_body);
        }

        instructions.push(Inst::Label { target: end_label });

        Ok(instructions)
    }

    // This is missing critical logic to actually exit the loop (break and continue)
    fn parse_while_statement(&mut self) -> Result<Vec<Inst>, String> {
        let mut instructions = Vec::new();

        let loop_start = self.alloc_label();
        let loop_end = self.alloc_label();

        instructions.push(Inst::Label { target: loop_start.clone() });

        let (cond_insts, cond_reg) = self.parse_expression()?;
        instructions.extend(cond_insts);

        instructions.push(Inst::JumpIfFalse { cond: Value::Reg(cond_reg), target: loop_end.clone() });

        let body = self.parse_block()?;
        instructions.extend(body);

        instructions.push(Inst::Jump { target: loop_start });

        instructions.push(Inst::Label { target: loop_end });

        Ok(instructions)
    }

    fn parse_expression(&mut self) -> Result<(Vec<Inst>, Reg), String> {
        self.parse_comparison()
    }

    fn parse_comparison(&mut self) -> Result<(Vec<Inst>, Reg), String> {
        let (mut instructions, mut left_reg) = self.parse_additive()?;

        while self.current == Token::DoubleEquals || self.current == Token::Less || self.current == Token::Greater {
            let op = self.current.clone();
            self.advance();

            let (right_insts, right_reg) = self.parse_additive()?;
            instructions.extend(right_insts);

            let result_reg = self.alloc_reg();

            let inst = match op {
                Token::DoubleEquals => Inst::Eq {
                    dst: result_reg.clone(),
                    lhs: Value::Reg(left_reg),
                    rhs: Value::Reg(right_reg),
                    ty: Type::I32 // Add type inference later
                },
                Token::Less => Inst::Lt {
                    dst: result_reg.clone(),
                    lhs: Value::Reg(left_reg),
                    rhs: Value::Reg(right_reg),
                    ty: Type::I32 // Add type inference later
                },
                Token::Greater => Inst::Gt {
                    dst: result_reg.clone(),
                    lhs: Value::Reg(left_reg),
                    rhs: Value::Reg(right_reg),
                    ty: Type::I32 // Add type inference later
                },
                _ => return Err(format!("Comparison operator {:?} not yet implemented", op))
            };

            instructions.push(inst);
            left_reg = result_reg;
        }

        Ok((instructions, left_reg))
    }

    fn parse_additive(&mut self) -> Result<(Vec<Inst>, Reg), String> {
        let (mut instructions, mut left_reg) = self.parse_multiplicative()?;

        while self.current == Token::Plus || self.current == Token::Minus {
            let op = self.current.clone();
            self.advance();

            let (right_insts, right_reg) = self.parse_multiplicative()?;
            instructions.extend(right_insts);

            let result_reg = self.alloc_reg();

            let inst = match op {
                Token::Plus => Inst::Add {
                    dst: result_reg.clone(),
                    lhs: Value::Reg(left_reg),
                    rhs: Value::Reg(right_reg),
                    ty: Type::I32 // Add type inference later
                },
                Token::Minus => Inst::Sub {
                    dst: result_reg.clone(),
                    lhs: Value::Reg(left_reg),
                    rhs: Value::Reg(right_reg),
                    ty: Type::I32 // Add type inference later
                },
                _ => unreachable!()
            };

            instructions.push(inst);
            left_reg = result_reg
        }

        Ok((instructions, left_reg))
    }

    fn parse_multiplicative(&mut self) -> Result<(Vec<Inst>, Reg), String> {
        let (mut instructions, mut left_reg) = self.parse_term()?;

        while self.current == Token::Star || self.current == Token::Slash {
            let op = self.current.clone();
            self.advance();

            let (right_insts, right_reg) = self.parse_term()?;
            instructions.extend(right_insts);

            let result_reg = self.alloc_reg();

            let inst = match op {
                Token::Star => Inst::Mul {
                    dst: result_reg.clone(),
                    lhs: Value::Reg(left_reg),
                    rhs: Value::Reg(right_reg),
                    ty: Type::I32 // Add type inference later
                },
                Token::Slash => Inst::Div {
                    dst: result_reg.clone(),
                    lhs: Value::Reg(left_reg),
                    rhs: Value::Reg(right_reg),
                    ty: Type::I32 // Add type inference later
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
                self.advance();

                if self.current == Token::LeftParentheses {
                    self.advance();

                    let mut args = Vec::new();
                    while self.current != Token::RightParentheses {
                        let (arg_insts, arg_reg) = self.parse_expression()?;
                        instructions.extend(arg_insts);
                        args.push(Value::Reg(arg_reg));

                        if self.current == Token::Comma {
                            self.advance();
                        }
                    }

                    self.expect(Token::RightParentheses)?;

                    instructions.push(Inst::Call {
                        dst: Some(result_reg.clone()),
                        func: Value::Label(Label(name.to_string())),
                        args,
                        ty: Type::Unknown // Need type inference
                    });
                }
                else {
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
                }
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

    fn alloc_label(&mut self) -> Label {
        let label = Label(format!("l{}", self.next_label));
        self.next_label += 1;

        label
    }

    fn advance(&mut self) {
        if let Some(token) = self.peek_buffer.take() {
            self.current = token;
        }
        else {
            self.current = self.lexer.next_token();
        }

        if !matches!(self.config.style.block_style, BlockStyle::Indentation) {
            while self.current == Token::Newline {
                if let Some(token) = self.peek_buffer.take() {
                    self.current = token;
                }
                else {
                    self.current = self.lexer.next_token();
                }
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