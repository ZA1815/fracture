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
    next_label: u32,

    struct_defs: HashMap<String, StructDef>,
    reg_types: HashMap<Reg, Type>,

    current_module_path: Vec<String>,
    pending_uses: Vec<UseStatement>
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
            next_label: 0,
            struct_defs: HashMap::new(),
            reg_types: HashMap::new(),
            current_module_path: Vec::new(),
            pending_uses: Vec::new()
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
        let mut root_module = Module::root();
        
        // For backwards compat, eventually will migrate completely
        let mut all_functions: HashMap<String, Function> = HashMap::new();
        let mut all_structs: HashMap<String, StructDef> = HashMap::new();
        let mut root_uses: Vec<UseStatement> = Vec::new();

        while self.current != Token::Eof {
            if self.current == Token::Newline {
                self.advance();
                continue;
            }

            if self.current == Token::Use {
                let use_statement = self.parse_use_statement(Visibility::Private)?;
                root_uses.push(use_statement);
                continue;
            }

            if self.current == Token::Pub {
                let peek = self.peek();
                if peek == Token::Use {
                    self.advance();
                    let use_statement = self.parse_use_statement(Visibility::Public)?;
                    root_uses.push(use_statement);
                    continue;
                }
            }

            if self.current == Token::Mod {
                let module = self.parse_module_declaration(Visibility::Private)?;

                for (name, func) in &module.functions {
                    // Fix formatting later to use user token
                    let qualified = format!("{}::{}", module.name, name);
                    all_functions.insert(qualified, func.clone());
                }
                for (name, s) in &module.structs {
                    let qualified = format!("{}::{}", module.name, name);
                    all_structs.insert(qualified, s.clone());
                }

                root_module.add_child(module);
                continue;
            }

            if self.current == Token::Pub {
                let peek = self.peek();
                if peek == Token::Mod {
                    self.advance();
                    let module = self.parse_module_declaration(Visibility::Public)?;
                    root_module.add_child(module);
                    continue;
                }
            }

            if self.current == Token::Struct {
                let struct_def = self.parse_struct(Visibility::Private)?;
                self.struct_defs.insert(struct_def.name.clone(), struct_def.clone());
                all_structs.insert(struct_def.name.clone(), struct_def.clone());
                root_module.add_struct(struct_def);
                continue;
            }

            if self.current == Token::Pub {
                let peek = self.peek();
                if peek == Token::Struct {
                    self.advance();
                    let struct_def = self.parse_struct(Visibility::Public)?;
                    self.struct_defs.insert(struct_def.name.clone(), struct_def.clone());
                    all_structs.insert(struct_def.name.clone(), struct_def.clone());
                    root_module.add_struct(struct_def);
                    continue;
                }
            }

            if self.current == Token::Hash {
                let attrs = self.parse_attributes()?;

                let visibility = if self.current == Token::Pub {
                    self.advance();

                    Visibility::Public
                }
                else {
                    Visibility::Private
                };

                if self.current != Token::Function {
                    return Err(format!("Attributes can only be applied to functions, found {:?}", self.current));
                }

                let mut func = self.parse_function(visibility)?;
                func.attributes = attrs;
                all_functions.insert(func.name.clone(), func.clone());
                root_module.add_function(func);
                continue;
            }

            if self.current == Token::Pub {
                let peek = self.peek();
                if peek == Token::Function {
                    self.advance();
                    let func = self.parse_function(Visibility::Public)?;
                    all_functions.insert(func.name.clone(), func.clone());
                    root_module.add_function(func);
                    continue;
                }
            }

            if self.current == Token::Function {
                let func = self.parse_function(Visibility::Private)?;
                all_functions.insert(func.name.clone(), func.clone());
                root_module.add_function(func);
                continue;
            }

            // Could throw error instead
            self.advance();
        }

        if !all_functions.contains_key("main") {
            return Err("Program must have a main function".to_string());
        }

        Ok(Program {
            functions: all_functions,
            structs: all_structs,
            entry: "main".to_string(),
            root_module,
            uses: root_uses,
        })
    }

    fn parse_module_declaration(&mut self, visibility: Visibility) -> Result<Module, String> {
        self.expect(Token::Mod)?;

        let module_name = self.expect_ident()?;

        self.current_module_path.push(module_name.clone());

        let mut module = Module::new(&module_name);
        module.visibility = visibility;
        module.path = ModulePath {
            segments: self.current_module_path.iter()
                .map(|s| PathSegment::Ident(s.clone()))
                .collect()
        };

        if self.current == Token::LeftBrace {
            self.advance();

            while self.current != Token::RightBrace && self.current != Token::Eof {
                if self.current == Token::Newline {
                    self.advance();
                    continue;
                }

                if self.current == Token::Use {
                    let use_statement = self.parse_use_statement(Visibility::Private)?;
                    module.uses.push(use_statement);
                    continue;
                }

                if self.current == Token::Mod {
                    let nested = self.parse_module_declaration(Visibility::Private)?;
                    module.add_child(nested);
                    continue;
                }

                if self.current == Token::Pub {
                    let peek = self.peek();

                    match peek {
                        Token::Use => {
                            self.advance();
                            let use_statement = self.parse_use_statement(Visibility::Public)?;
                            module.uses.push(use_statement);
                            continue;
                        }
                        Token::Mod => {
                            self.advance();
                            let nested = self.parse_module_declaration(Visibility::Public)?;
                            module.add_child(nested);
                            continue;
                        }
                        Token::Function => {
                            self.advance();
                            let func = self.parse_function(Visibility::Public)?;
                            module.add_function(func);
                            continue;
                        }
                        Token::Struct => {
                            self.advance();
                            let s = self.parse_struct(Visibility::Public)?;
                            module.add_struct(s);
                            continue;
                        }
                        _ => {}
                    }
                }

                if self.current == Token::Function {
                    let func = self.parse_function(Visibility::Private)?;
                    module.add_function(func);
                    continue;
                }

                if self.current == Token::Struct {
                    let s = self.parse_struct(Visibility::Private)?;
                    module.add_struct(s);
                    continue;
                }

                self.advance();
            }
            
            self.expect(Token::RightBrace)?;
        }
        else {
            module.external_mods.push(module_name.clone());

            if self.config.style.needs_semicolon {
                self.expect(Token::Semicolon)?;
            }
        }

        self.current_module_path.pop();

        Ok(module)
    }

    fn parse_use_statement(&mut self, visibility: Visibility) -> Result<UseStatement, String> {
        self.expect(Token::Use)?;

        let tree = self.parse_use_tree()?;

        if self.config.style.needs_semicolon {
            self.expect(Token::Semicolon)?;
        }

        Ok(UseStatement { tree, visibility })
    }

    fn parse_use_tree(&mut self) -> Result<UseTree, String> {
        let mut path_segments: Vec<PathSegment> = Vec::new();

        loop {
            let segment = match &self.current {
                Token::SelfKw => {
                    self.advance();
                    PathSegment::SelfKw
                }
                Token::Super => {
                    self.advance();
                    PathSegment::Super
                }
                Token::Ident(name) => {
                    let name = name.clone();
                    self.advance();
                    PathSegment::Ident(name)
                }
                _ => break
            };

            path_segments.push(segment);

            if self.current == Token::DoubleColon {
                self.advance();

                if self.current == Token::Star {
                    self.advance();
                    return Ok(UseTree::Glob { path: ModulePath { segments: path_segments } });
                }

                if self.current == Token::LeftBrace {
                    self.advance();
                    let items = self.parse_use_tree_list()?;
                    self.expect(Token::RightBrace)?;
                    return Ok(UseTree::Nested { path: ModulePath { segments: path_segments }, items });
                }
            }
            else {
                break;
            }
        }

        let alias = if self.current == Token::As {
            self.advance();

            Some(self.expect_ident()?)
        }
        else {
            None
        };

        Ok(UseTree::Simple { path: ModulePath { segments: path_segments }, alias })
    }

    fn parse_use_tree_list(&mut self) -> Result<Vec<UseTree>, String> {
        let mut items = Vec::new();

        while self.current != Token::RightBrace && self.current != Token::Eof {
            let item = self.parse_use_tree()?;
            items.push(item);

            if self.current == Token::Comma {
                self.advance();
            }
            else {
                break;
            }
        }

        Ok(items)
    }

    fn parse_function(&mut self, visibility: Visibility) -> Result<Function, String> {
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

        let module_path = if self.current_module_path.is_empty() {
            None
        }
        else {
            Some(ModulePath {
                segments: self.current_module_path.iter().map(|s| PathSegment::Ident(s.clone())).collect()
            })
        };

        Ok(Function {
            name,
            params,
            return_type,
            body,
            locals,
            attributes: Vec::new(),
            visibility,
            module_path
        })
    }

    fn parse_struct(&mut self, visibility: Visibility) -> Result<StructDef, String> {
        self.advance();

        let struct_name = self.expect_ident()?;

        self.expect(Token::LeftBrace)?;

        let mut fields = Vec::new();
        let mut field_visibility = HashMap::new();

        while self.current != Token::RightBrace && self.current != Token::Eof {
            if self.current == Token::Newline {
                self.advance();
                continue;
            }

            let field_vis = if self.current == Token::Pub {
                self.advance();

                Visibility::Public
            }
            else {
                Visibility::Private
            };

            let field_name = self.expect_ident()?;
            // Maybe customize this later
            self.expect(Token::Colon)?;
            let field_type = self.parse_type()?;

            fields.push((field_name.clone(), field_type));
            field_visibility.insert(field_name, field_vis);

            if self.current == Token::Comma {
                self.advance();
            }

            while self.current == Token::Newline {
                self.advance();
            }
        }

        self.advance();

        Ok(StructDef { name: struct_name, fields, visibility, field_visibility })
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

                    let var_type = if matches!(var_type, Type::Unknown) {
                        self.reg_types.get(&result_reg).cloned().unwrap_or(Type::Unknown)
                    } else {
                        var_type
                    };

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

        while self.current == Token::DoubleEquals
            || self.current == Token::Less
            || self.current == Token::Greater
            || self.current == Token::GreaterEquals
            || self.current == Token::LessEquals
            || self.current == Token::NotEquals
        {
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
                Token::LessEquals => Inst::Le {
                    dst: result_reg.clone(),
                    lhs: Value::Reg(left_reg),
                    rhs: Value::Reg(right_reg),
                    ty: Type::I32
                },
                Token::GreaterEquals => Inst::Ge {
                    dst: result_reg.clone(),
                    lhs: Value::Reg(left_reg),
                    rhs: Value::Reg(right_reg),
                    ty: Type::I32
                },
                Token::NotEquals => Inst::Ne {
                    dst: result_reg.clone(),
                    lhs: Value::Reg(left_reg),
                    rhs: Value::Reg(right_reg),
                    ty: Type::I32 
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

            let left_is_string = self.reg_types.get(&left_reg)
                .map(|ty| matches!(ty, Type::String))
                .unwrap_or(false);
            let right_is_string = self.reg_types.get(&right_reg)
                .map(|ty| matches!(ty, Type::String))
                .unwrap_or(false);

            if left_is_string || right_is_string {
                if op != Token::Plus {
                    return Err("Can only use `Plus Token` operator with strings, not `Minus Token`".to_string());
                }

                instructions.push(Inst::StringConcat {
                    dst: result_reg.clone(),
                    left: left_reg,
                    right: right_reg
                });

                self.reg_types.insert(result_reg.clone(), Type::String);
            }
            else {
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
            }
            left_reg = result_reg
        }

        Ok((instructions, left_reg))
    }

    fn parse_multiplicative(&mut self) -> Result<(Vec<Inst>, Reg), String> {
        let (mut instructions, mut left_reg) = self.parse_postfix()?;

        while self.current == Token::Star || self.current == Token::Slash {
            let op = self.current.clone();
            self.advance();

            let (right_insts, right_reg) = self.parse_postfix()?;
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

    fn parse_postfix(&mut self) -> Result<(Vec<Inst>, Reg), String> {
        let (mut instructions, mut reg) = self.parse_term()?;

        loop {
            let result_reg = self.alloc_reg();
            if self.current == Token::Dot {
                self.advance();

                match &self.current {
                    Token::Number(n) => {
                        let index = *n as usize;
                        self.advance();

                        instructions.push(Inst::TupleLoad {
                            dst: result_reg.clone(),
                            tuple_reg: reg,
                            index,
                            ty: Type::Unknown
                        });
                    }
                    Token::Ident(field_name) => {
                        let field = field_name.clone();
                        self.advance();

                        if self.current == Token::LeftParentheses {
                            self.advance();

                            let reg_type = self.reg_types.get(&reg).cloned();

                            match field.as_str() {
                                "len" => {
                                    self.expect(Token::RightParentheses)?;

                                    let is_string = reg_type.as_ref().map(|ty| matches!(ty, Type::String)).unwrap_or(false);
                                    let is_vec = reg_type.as_ref().map(|ty| matches!(ty, Type::Vec(_))).unwrap_or(false);
                                    let is_hashmap = reg_type.as_ref().map(|ty| matches!(ty, Type::HashMap(_, _))).unwrap_or(false);

                                    if is_string {
                                        instructions.push(Inst::StringLen { dst: result_reg.clone(), string: reg });
                                    }
                                    else if is_vec {
                                        instructions.push(Inst::VecLen { dst: result_reg.clone(), vec: reg });
                                    }
                                    else if is_hashmap {
                                        instructions.push(Inst::HashMapLen { dst: result_reg.clone(), map: reg });
                                    }
                                    else {
                                        instructions.push(Inst::SliceLen { dst: result_reg.clone(), slice: reg });
                                    }

                                    self.reg_types.insert(result_reg.clone(), Type::I64);
                                }
                                "push" => {
                                    let (val_insts, val_reg) = self.parse_expression()?;
                                    instructions.extend(val_insts);
                                    self.expect(Token::RightParentheses)?;

                                    let elem_ty = if let Some(Type::Vec(inner)) = self.reg_types.get(&reg) {
                                        (**inner).clone()
                                    }
                                    // Could give error here
                                    else {
                                        Type::I32
                                    };

                                    instructions.push(Inst::VecPush {
                                        vec: reg.clone(),
                                        value: Value::Reg(val_reg),
                                        element_ty: elem_ty
                                    });

                                    reg = reg.clone();

                                    continue;
                                }
                                "pop" => {
                                    self.expect(Token::RightParentheses)?;

                                    let elem_ty = if let Some(Type::Vec(inner)) = self.reg_types.get(&reg) {
                                        (**inner).clone()
                                    }
                                    // Could give error here
                                    else {
                                        Type::I32
                                    };

                                    instructions.push(Inst::VecPop {
                                        dst: result_reg.clone(),
                                        vec: reg,
                                        element_ty: elem_ty.clone()
                                    });

                                    self.reg_types.insert(result_reg.clone(), elem_ty);
                                }
                                "cap" | "capacity" => {
                                    self.expect(Token::RightParentheses)?;

                                    let is_hashmap = reg_type.as_ref().map(|ty| matches!(ty, Type::HashMap(_, _))).unwrap_or(false);

                                    if is_hashmap {
                                        instructions.push(Inst::HashMapCap { dst: result_reg.clone(), map: reg });
                                    }
                                    else {
                                        instructions.push(Inst::VecCap { dst: result_reg.clone(), vec: reg });
                                    }

                                    self.reg_types.insert(result_reg.clone(), Type::I64);
                                }
                                "insert" => {
                                    let (key_insts, key_reg) = self.parse_expression()?;
                                    instructions.extend(key_insts);

                                    self.expect(Token::Comma)?;

                                    let (val_insts, val_reg) = self.parse_expression()?;
                                    instructions.extend(val_insts);

                                    self.expect(Token::RightParentheses)?;

                                    let (key_ty, value_ty) = if let Some(Type::HashMap(k, v)) = &reg_type {
                                        ((**k).clone(), (**v).clone())
                                    }
                                    else {
                                        // Change from defaults later
                                        (Type::I32, Type::I32)
                                    };

                                    instructions.push(Inst::HashMapInsert {
                                        map: reg.clone(),
                                        key: Value::Reg(key_reg),
                                        value: Value::Reg(val_reg),
                                        key_ty,
                                        value_ty
                                    });

                                    // Could return something later
                                    reg = reg.clone();

                                    continue;
                                }
                                "get" => {
                                    // User needs to check found before using value, change later
                                    let (key_insts, key_reg) = self.parse_expression()?;
                                    instructions.extend(key_insts);

                                    self.expect(Token::RightParentheses)?;

                                    let (key_ty, value_ty) = if let Some(Type::HashMap(k, v)) = &reg_type {
                                        ((**k).clone(), (**v).clone())
                                    }
                                    else {
                                        // Change from defaults later
                                        (Type::I32, Type::I32)
                                    };

                                    let found_reg = self.alloc_reg();

                                    instructions.push(Inst::HashMapGet {
                                        dst: result_reg.clone(),
                                        found_dst: found_reg,
                                        map: reg,
                                        key: Value::Reg(key_reg),
                                        key_ty,
                                        value_ty: value_ty.clone()
                                    });

                                    self.reg_types.insert(result_reg.clone(), value_ty);
                                }
                                "remove" => {
                                    let (key_insts, key_reg) = self.parse_expression()?;
                                    instructions.extend(key_insts);

                                    self.expect(Token::RightParentheses)?;

                                    let (key_ty, value_ty) = if let Some(Type::HashMap(k, v)) = &reg_type {
                                        ((**k).clone(), (**v).clone())
                                    }
                                    else {
                                        // Change from defaults later
                                        (Type::I32, Type::I32)
                                    };

                                    instructions.push(Inst::HashMapRemove {
                                        success_dst: result_reg.clone(),
                                        map: reg,
                                        key: Value::Reg(key_reg),
                                        key_ty,
                                        value_ty
                                    });

                                    self.reg_types.insert(result_reg.clone(), Type::Bool);
                                }
                                "contains_key" | "contains" => {
                                    let (key_insts, key_reg) = self.parse_expression()?;
                                    instructions.extend(key_insts);

                                    self.expect(Token::RightParentheses)?;

                                    let (key_ty, value_ty) = if let Some(Type::HashMap(k, v)) = &reg_type {
                                        ((**k).clone(), (**v).clone())
                                    }
                                    else {
                                        // Change from defaults later
                                        (Type::I32, Type::I32)
                                    };

                                    instructions.push(Inst::HashMapContains {
                                        dst: result_reg.clone(),
                                        map: reg,
                                        key: Value::Reg(key_reg),
                                        key_ty,
                                        value_ty
                                    });

                                    self.reg_types.insert(result_reg.clone(), Type::Bool);
                                }
                                "clear" => {
                                    self.expect(Token::RightParentheses)?;

                                    instructions.push(Inst::HashMapClear {
                                        map: reg.clone()
                                    });

                                    reg = reg.clone();

                                    continue;
                                }
                                _ => {
                                    return Err(format!("Unknown method: {}", field));
                                }
                            }
                        }
                        else {
                            let struct_type = self.var_types.values()
                            .find(|ty| matches!(ty, Type::Struct(_)))
                            .cloned()
                            .unwrap_or(Type::Unknown);

                            instructions.push(Inst::FieldLoad {
                                dst: result_reg.clone(),
                                struct_reg: reg,
                                field_name: field,
                                ty: struct_type
                            });
                        }
                    }
                    _ => return Err(format!("Expected field name or number after '.', got {:?}", self.current))
                }
                reg = result_reg;

                continue;
            }

            if self.current == Token::LeftBracket {
                self.advance();

                let (first_insts, first_reg) = self.parse_expression()?;
                instructions.extend(first_insts);

                if self.current == Token::SliceDot {
                    self.advance();

                    let (end_insts, end_reg) = self.parse_expression()?;
                    instructions.extend(end_insts);

                    self.expect(Token::RightBracket)?;

                    let result_reg = self.alloc_reg();

                    instructions.push(Inst::SliceCreate {
                        dst: result_reg.clone(),
                        array: reg,
                        start: Value::Reg(first_reg),
                        end: Value::Reg(end_reg),
                        element_ty: Type::I32 // Default, change later
                    });

                    self.reg_types.insert(result_reg.clone(), Type::Slice(Box::new(Type::I32)));

                    reg = result_reg;
                }
                else {
                    self.expect(Token::RightBracket)?;

                    let result_reg = self.alloc_reg();

                    let reg_type = self.reg_types.get(&reg);
                    let is_slice = self.reg_types.get(&reg)
                        .map(|ty| matches!(ty, Type::Slice(_)))
                        .unwrap_or(false);
                    let is_vec = self.reg_types.get(&reg)
                        .map(|ty| matches!(ty, Type::Vec(_)))
                        .unwrap_or(false);

                    if is_slice {
                        instructions.push(Inst::SliceIndexLoad {
                            dst: result_reg.clone(),
                            slice: reg,
                            index: Value::Reg(first_reg),
                            element_ty: Type::I32 // Default, change later
                        });
                    }
                    else if is_vec {
                        let elem_ty = if let Some(Type::Vec(inner)) = reg_type {
                            (**inner).clone()
                        }
                        // Likely return error here later
                        else {
                            Type::I32
                        };

                        instructions.push(Inst::VecGet {
                            dst: result_reg.clone(),
                            vec: reg,
                            index: Value::Reg(first_reg),
                            element_ty: elem_ty
                        });
                    }
                    else {
                        // Improve with type tracking later
                        instructions.push(Inst::IndexLoad {
                            dst: result_reg.clone(),
                            array: reg,
                            index: Value::Reg(first_reg),
                            element_ty: Type::I32
                        });
                    }

                    reg = result_reg;

                    continue;
                }
            }
            break;
        }

        Ok((instructions, reg))
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
            Token::Bool(b) => {
                instructions.push(Inst::Move {
                    dst: result_reg.clone(),
                    src: Value::Const(Const::Bool(*b)),
                    ty: Type::Bool
                });
                self.advance();
            }
            Token::Ident(name) => {
                let name = name.clone();
                self.advance();

                if self.current == Token::DoubleColon {
                    let mut path_parts = vec![name.clone()];
                    while self.current == Token::DoubleColon {
                        self.advance();
                        let part = self.expect_ident()?;
                        path_parts.push(part);
                    }
                    
                    let full_path = path_parts.join("::");
                    let last = path_parts.last().unwrap().as_str();
                    let prefix = &path_parts[..path_parts.len() - 1].join("::");

                    if name == "Vec" && last == "new" {
                        self.expect(Token::LeftParentheses)?;
                        self.expect(Token::RightParentheses)?;

                        instructions.push(Inst::VecAlloc {
                            dst: result_reg.clone(),
                            element_ty: Type::I32, // Default, change later
                            initial_cap: Value::Const(Const::I64(8))
                        });

                        self.reg_types.insert(result_reg.clone(), Type::Vec(Box::new(Type::I32)));

                        return Ok((instructions, result_reg));
                    }
                    else if name == "HashMap" && last == "new" {
                        self.expect(Token::LeftParentheses)?;
                        self.expect(Token::RightParentheses)?;

                        let key_ty = Type::I32;
                        let value_ty = Type::I32;

                        instructions.push(Inst::HashMapAlloc {
                            dst: result_reg.clone(),
                            key_ty: key_ty.clone(),
                            value_ty: value_ty.clone(),
                            initial_cap: Value::Const(Const::I64(8))
                        });

                        self.reg_types.insert(result_reg.clone(), Type::HashMap(Box::new(key_ty), Box::new(value_ty)));

                        return Ok((instructions, result_reg));
                    }
                    else if name == "HashMap" && last == "with_capacity" {
                        self.expect(Token::LeftParentheses)?;

                        let (cap_insts, cap_reg) = self.parse_expression()?;
                        instructions.extend(cap_insts);

                        self.expect(Token::RightParentheses)?;

                        let key_ty = Type::I32;
                        let value_ty = Type::I32;

                        instructions.push(Inst::HashMapAlloc {
                            dst: result_reg.clone(),
                            key_ty: key_ty.clone(),
                            value_ty: value_ty.clone(),
                            initial_cap: Value::Reg(cap_reg)
                        });

                        self.reg_types.insert(result_reg.clone(), Type::HashMap(Box::new(key_ty), Box::new(value_ty)));

                        return Ok((instructions, result_reg));
                    }
                    else if self.current == Token::LeftParentheses {
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
                            func: Value::Label(Label(full_path)),
                            args,
                            ty: Type::Unknown
                        });

                        return Ok((instructions, result_reg));
                    }
                    else {
                        return Err(format!("Unknown path expression: {}", full_path));
                    }
                }

                if self.current == Token::LeftParentheses {
                    if name == "print" {
                        self.advance();

                        let (arg_insts, arg_reg) = self.parse_expression()?;
                        instructions.extend(arg_insts);

                        self.expect(Token::RightParentheses)?;

                        instructions.push(Inst::Print { value: arg_reg });

                        // Change to void later as the backend doesn't suppor it yet
                        instructions.push(Inst::Move {
                            dst: result_reg.clone(),
                            src: Value::Const(Const::I32(0)),
                            ty: Type::I32
                        });

                        return Ok((instructions, result_reg));
                    }
                    else if name == "println" {
                        self.advance();
                        
                        let (arg_insts, arg_reg) = self.parse_expression()?;
                        instructions.extend(arg_insts);
                        
                        self.expect(Token::RightParentheses)?;
                        
                        instructions.push(Inst::Println { 
                            value: arg_reg 
                        });
                        
                        // Change to void later as the backend doesn't support it yet
                        instructions.push(Inst::Move {
                            dst: result_reg.clone(),
                            src: Value::Const(Const::I32(0)),
                            ty: Type::I32
                        });
                        
                        return Ok((instructions, result_reg));
                    }
                    else if name == "eprint" {
                        self.advance();
                        
                        let (arg_insts, arg_reg) = self.parse_expression()?;
                        instructions.extend(arg_insts);
                        
                        self.expect(Token::RightParentheses)?;
                        
                        instructions.push(Inst::Eprint { value: arg_reg });
                        
                        // Change to void later as the backend doesn't suppor it yet
                        instructions.push(Inst::Move {
                            dst: result_reg.clone(),
                            src: Value::Const(Const::I32(0)),
                            ty: Type::I32
                        });
                        
                        return Ok((instructions, result_reg));
                    }
                    else if name == "eprintln" {
                        self.advance();
                        
                        let (arg_insts, arg_reg) = self.parse_expression()?;
                        instructions.extend(arg_insts);
                        
                        self.expect(Token::RightParentheses)?;
                        
                        instructions.push(Inst::Eprintln { value: arg_reg });
                        
                        // Change to void later as the backend doesn't support it yet
                        instructions.push(Inst::Move {
                            dst: result_reg.clone(),
                            src: Value::Const(Const::I32(0)),
                            ty: Type::I32
                        });
                        
                        return Ok((instructions, result_reg));
                    }
                    else if name == "read_line" {
                        self.advance();
                        self.expect(Token::RightParentheses)?;
                        
                        instructions.push(Inst::ReadLine { 
                            dst: result_reg.clone() 
                        });
                        
                        self.reg_types.insert(result_reg.clone(), Type::String);
                        
                        return Ok((instructions, result_reg));
                    }
                    else if name == "itoa" || name == "to_string" {
                        self.advance();

                        let (val_insts, val_reg) = self.parse_expression()?;
                        instructions.extend(val_insts);

                        self.expect(Token::RightParentheses)?;

                        instructions.push(Inst::IntToString { dst: result_reg.clone(), value: Value::Reg(val_reg) });

                        self.reg_types.insert(result_reg.clone(), Type::String);

                        return Ok((instructions, result_reg));
                    }
                    else if name == "sys_write" {
                        self.advance();

                        let (fd_insts, fd_reg) = self.parse_expression()?;
                        instructions.extend(fd_insts);
                        self.expect(Token::Comma)?;

                        let (buf_insts, buf_reg) = self.parse_expression()?;
                        instructions.extend(buf_insts);
                        self.expect(Token::Comma)?;

                        let (len_insts, len_reg) = self.parse_expression()?;
                        instructions.extend(len_insts);
                        
                        self.expect(Token::RightParentheses)?;

                        instructions.push(Inst::SysWrite {
                            fd: Value::Reg(fd_reg),
                            buf: buf_reg,
                            len: Value::Reg(len_reg),
                            result_dst: result_reg.clone()
                        });

                        self.reg_types.insert(result_reg.clone(), Type::I64);

                        return Ok((instructions, result_reg));
                    }
                    else if name == "sys_read" {
                        self.advance();
                        
                        let (fd_insts, fd_reg) = self.parse_expression()?;
                        instructions.extend(fd_insts);
                        self.expect(Token::Comma)?;
                        
                        let (buf_insts, buf_reg) = self.parse_expression()?;
                        instructions.extend(buf_insts);
                        self.expect(Token::Comma)?;
                        
                        let (len_insts, len_reg) = self.parse_expression()?;
                        instructions.extend(len_insts);
                        
                        self.expect(Token::RightParentheses)?;
                        
                        instructions.push(Inst::SysRead {
                            fd: Value::Reg(fd_reg),
                            buf: buf_reg,
                            len: Value::Reg(len_reg),
                            result_dst: result_reg.clone()
                        });
                        
                        self.reg_types.insert(result_reg.clone(), Type::I64);
                        
                        return Ok((instructions, result_reg));
                    }
                    else if name == "sys_open" {
                        self.advance();

                        let (path_insts, path_reg) = self.parse_expression()?;
                        instructions.extend(path_insts);
                        self.expect(Token::Comma)?;

                        let (flag_insts, flag_reg) = self.parse_expression()?;
                        instructions.extend(flag_insts);
                        self.expect(Token::Comma)?;

                        let (mode_insts, mode_reg) = self.parse_expression()?;
                        instructions.extend(mode_insts);
                        
                        self.expect(Token::RightParentheses)?;

                        instructions.push(Inst::SysOpen {
                            path: path_reg,
                            flags: Value::Reg(flag_reg),
                            mode: Value::Reg(mode_reg),
                            result_dst: result_reg.clone()
                        });

                        self.reg_types.insert(result_reg.clone(), Type::I32);

                        return Ok((instructions, result_reg));
                    }
                    else if name == "sys_close" {
                        self.advance();

                        let (fd_insts, fd_reg) = self.parse_expression()?;
                        instructions.extend(fd_insts);

                        self.expect(Token::RightParentheses)?;

                        instructions.push(Inst::SysClose {
                            fd: Value::Reg(fd_reg),
                            result_dst: result_reg.clone()
                        });

                        self.reg_types.insert(result_reg.clone(), Type::I32);

                        return Ok((instructions, result_reg))
                    }
                    else if name == "file_exists" || name == "path_exists" {
                        self.advance();

                        let (path_insts, path_reg) = self.parse_expression()?;
                        instructions.extend(path_insts);

                        self.expect(Token::RightParentheses)?;

                        let access_result = self.alloc_reg();

                        instructions.push(Inst::SysAccess {
                            path: path_reg,
                            mode: Value::Const(Const::I32(0)),
                            result_dst: access_result.clone()
                        });

                        instructions.push(Inst::Eq {
                            dst: result_reg.clone(),
                            lhs: Value::Reg(access_result),
                            rhs: Value::Const(Const::I64(0)),
                            ty: Type::I64
                        });

                        self.reg_types.insert(result_reg.clone(), Type::Bool);

                        return Ok((instructions, result_reg));
                    }
                    else if name == "is_readable" {
                        self.advance();

                        let (path_insts, path_reg) = self.parse_expression()?;
                        instructions.extend(path_insts);

                        self.expect(Token::RightParentheses)?;

                        let access_result = self.alloc_reg();

                        instructions.push(Inst::SysAccess {
                            path: path_reg,
                            mode: Value::Const(Const::I32(4)),
                            result_dst: access_result.clone()
                        });

                        instructions.push(Inst::Eq {
                            dst: result_reg.clone(),
                            lhs: Value::Reg(access_result),
                            rhs: Value::Const(Const::I64(0)),
                            ty: Type::I64
                        });

                        self.reg_types.insert(result_reg.clone(), Type::Bool);

                        return Ok((instructions, result_reg));
                    }
                    else if name == "is_writable" {
                        self.advance();

                        let (path_insts, path_reg) = self.parse_expression()?;
                        instructions.extend(path_insts);

                        self.expect(Token::RightParentheses)?;

                        let access_result = self.alloc_reg();

                        instructions.push(Inst::SysAccess {
                            path: path_reg,
                            mode: Value::Const(Const::I32(2)),
                            result_dst: access_result.clone()
                        });

                        instructions.push(Inst::Eq {
                            dst: result_reg.clone(),
                            lhs: Value::Reg(access_result),
                            rhs: Value::Const(Const::I64(0)),
                            ty: Type::I64
                        });

                        self.reg_types.insert(result_reg.clone(), Type::Bool);

                        return Ok((instructions, result_reg));
                    }
                    else if name == "read_file" || name == "fs_read" {
                        self.advance();

                        let (path_insts, path_reg) = self.parse_expression()?;
                        instructions.extend(path_insts);

                        self.expect(Token::RightParentheses)?;

                        let error_reg = self.alloc_reg();

                        instructions.push(Inst::FileReadToString {
                            dst: result_reg.clone(),
                            path: path_reg,
                            result_dst: error_reg
                        });

                        self.reg_types.insert(result_reg.clone(), Type::String);

                        return Ok((instructions, result_reg));
                    }
                    else if name == "write_file" || name == "fs_write" {
                        self.advance();

                        let (path_insts, path_reg) = self.parse_expression()?;
                        instructions.extend(path_insts);

                        self.expect(Token::Comma)?;

                        let (content_insts, content_reg) = self.parse_expression()?;
                        instructions.extend(content_insts);

                        self.expect(Token::RightParentheses)?;

                        instructions.push(Inst::FileWriteString {
                            path: path_reg,
                            content: content_reg,
                            result_dst: result_reg.clone()
                        });

                        self.reg_types.insert(result_reg.clone(), Type::I64);

                        return Ok((instructions, result_reg));
                    }
                    else if name == "append_file" || name == "fs_append" {
                        self.advance();

                        let (path_insts, path_reg) = self.parse_expression()?;
                        instructions.extend(path_insts);

                        self.expect(Token::Comma)?;

                        let (content_insts, content_reg) = self.parse_expression()?;
                        instructions.extend(content_insts);

                        self.expect(Token::RightParentheses)?;

                        instructions.push(Inst::FileAppendString {
                            path: path_reg,
                            content: content_reg,
                            result_dst: result_reg.clone()
                        });

                        self.reg_types.insert(result_reg.clone(), Type::I64);

                        return Ok((instructions, result_reg));
                    }
                    else if name == "mkdir" || name == "create_dir" {
                        self.advance();

                        let (path_insts, path_reg) = self.parse_expression()?;
                        instructions.extend(path_insts);

                        let mode = if self.current == Token::Comma {
                            self.advance();
                            
                            let (mode_insts, mode_reg) = self.parse_expression()?;
                            instructions.extend(mode_insts);

                            Value::Reg(mode_reg)
                        }
                        else {
                            Value::Const(Const::I32(493))
                        };

                        self.expect(Token::RightParentheses)?;

                        instructions.push(Inst::SysMkdir {
                            path: path_reg,
                            mode,
                            result_dst: result_reg.clone()
                        });

                        self.reg_types.insert(result_reg.clone(), Type::I64);

                        return Ok((instructions, result_reg));
                    }
                    else if name == "rmdir" || name == "remove_dir" {
                        self.advance();

                        let (path_insts, path_reg) = self.parse_expression()?;
                        instructions.extend(path_insts);

                        self.expect(Token::RightParentheses)?;

                        instructions.push(Inst::SysRmdir {
                            path: path_reg,
                            result_dst: result_reg.clone()
                        });

                        self.reg_types.insert(result_reg.clone(), Type::I64);

                        return Ok((instructions, result_reg));
                    }
                    else if name == "unlink" || name == "remove_file" || name == "delete_file" {
                        self.advance();

                        let (path_insts, path_reg) = self.parse_expression()?;
                        instructions.extend(path_insts);

                        self.expect(Token::RightParentheses)?;

                        instructions.push(Inst::SysUnlink {
                            path: path_reg,
                            result_dst: result_reg.clone()
                        });

                        self.reg_types.insert(result_reg.clone(), Type::I64);

                        return Ok((instructions, result_reg));
                    }
                    else if name == "rename" || name == "mv" {
                        self.advance();

                        let (old_path_insts, old_path_reg) = self.parse_expression()?;
                        instructions.extend(old_path_insts);

                        self.expect(Token::Comma)?;

                        let (new_path_insts, new_path_reg) = self.parse_expression()?;
                        instructions.extend(new_path_insts);

                        self.expect(Token::RightParentheses)?;

                        instructions.push(Inst::SysRename {
                            old_path: old_path_reg,
                            new_path: new_path_reg,
                            result_dst: result_reg.clone()
                        });

                        self.reg_types.insert(result_reg.clone(), Type::I64);

                        return Ok((instructions, result_reg));
                    }
                    else if name == "getcwd" || name == "current_dir" || name == "pwd" {
                        self.advance();

                        self.expect(Token::RightParentheses)?;

                        instructions.push(Inst::SysGetcwd {
                            dst: result_reg.clone()
                        });

                        self.reg_types.insert(result_reg.clone(), Type::String);

                        return Ok((instructions, result_reg));
                    }
                    else if name == "chdir" || name == "cd" || name == "set_current_dir" {
                        self.advance();

                        let (path_insts, path_reg) = self.parse_expression()?;
                        instructions.extend(path_insts);

                        self.expect(Token::RightParentheses)?;

                        instructions.push(Inst::SysChdir {
                            path: path_reg,
                            result_dst: result_reg.clone()
                        });

                        self.reg_types.insert(result_reg.clone(), Type::I64);

                        return Ok((instructions, result_reg));
                    }
                    else if name == "file_size" {
                        self.advance();

                        let (path_insts, path_reg) = self.parse_expression()?;
                        instructions.extend(path_insts);

                        self.expect(Token::RightParentheses)?;

                        let stat_buf = self.alloc_reg();
                        let stat_result = self.alloc_reg();

                        instructions.push(Inst::SysStat {
                            path: path_reg,
                            stat_buf: stat_buf.clone(),
                            result_dst: stat_result.clone()
                        });

                        instructions.push(Inst::Load {
                            dst: result_reg.clone(),
                            ptr: Value::Reg(stat_buf),
                            ty: Type::I64
                        });

                        self.reg_types.insert(result_reg.clone(), Type::I64);

                        return Ok((instructions, result_reg));
                    }
                    else if name == "sys_seek" || name == "lseek" {
                        self.advance();

                        let (fd_insts, fd_reg) = self.parse_expression()?;
                        instructions.extend(fd_insts);

                        self.expect(Token::Comma)?;

                        let (offset_insts, offset_reg) = self.parse_expression()?;
                        instructions.extend(offset_insts);

                        self.expect(Token::Comma)?;

                        let (whence_insts, whence_reg) = self.parse_expression()?;
                        instructions.extend(whence_insts);

                        self.expect(Token::RightParentheses)?;

                        instructions.push(Inst::SysSeek {
                            fd: Value::Reg(fd_reg),
                            offset: Value::Reg(offset_reg),
                            whence: Value::Reg(whence_reg),
                            result_dst: result_reg.clone()
                        });

                        self.reg_types.insert(result_reg.clone(), Type::I64);

                        return Ok((instructions, result_reg));
                    }
                    else if name == "sys_access" || name == "access" {
                        self.advance();

                        let (path_insts, path_reg) = self.parse_expression()?;
                        instructions.extend(path_insts);

                        self.expect(Token::Comma)?;

                        let (mode_insts, mode_reg) = self.parse_expression()?;
                        instructions.extend(mode_insts);

                        self.expect(Token::RightParentheses)?;

                        instructions.push(Inst::SysAccess {
                            path: path_reg,
                            mode: Value::Reg(mode_reg),
                            result_dst: result_reg.clone()
                        });

                        self.reg_types.insert(result_reg.clone(), Type::I64);

                        return Ok((instructions, result_reg));
                    }                    
                    else {
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
                }
                else if self.current == Token::LeftBrace {
                    self.advance();

                    let struct_reg = self.alloc_reg();
                    instructions.push(Inst::StructAlloc {
                        dst: struct_reg.clone(),
                        struct_name: name.to_string()
                    });

                    while self.current != Token::RightBrace && self.current != Token::Eof {
                        let field_name = self.expect_ident()?;
                        self.expect(Token::Colon)?;

                        let (field_insts, field_reg) = self.parse_expression()?;
                        instructions.extend(field_insts);

                        instructions.push(Inst::FieldStore {
                            struct_reg: struct_reg.clone(),
                            field_name,
                            value: Value::Reg(field_reg),
                            ty: Type::Unknown
                        });

                        if self.current == Token::Comma {
                            self.advance();
                        }
                    }

                    self.advance();

                    instructions.push(Inst::Move {
                        dst: result_reg.clone(),
                        src: Value::Reg(struct_reg),
                        ty: Type::Struct(name.to_string())
                    });
                }
                else {
                    if let Some(var_reg) = self.var_regs.get(&name) {
                        let var_type = self.var_types.get(&name).cloned().unwrap_or(Type::Unknown);
                        instructions.push(Inst::Move {
                            dst: result_reg.clone(),
                            src: Value::Reg(var_reg.clone()),
                            ty: var_type.clone()
                        });
                        // Also register the type for the new register so postfix operations can find it
                        self.reg_types.insert(result_reg.clone(), var_type);
                    }
                    else {
                        return Err(format!("Unknown variable: {}", name));
                    }
                }
            }
            Token::LeftParentheses => {
                self.advance();

                let (first_insts, first_reg) = self.parse_expression()?;
                instructions.extend(first_insts);

                if self.current == Token::Comma {
                    self.advance();

                    let mut elements = vec![first_reg];
                    let mut element_types = vec![Type::Unknown];

                    while self.current != Token::RightParentheses && self.current != Token::Eof {
                        let (elem_insts, elem_reg) = self.parse_expression()?;
                        instructions.extend(elem_insts);
                        elements.push(elem_reg);
                        element_types.push(Type::Unknown);

                        if self.current == Token::Comma {
                            self.advance();
                        }
                        else {
                            break;
                        }
                    }

                    self.expect(Token::RightParentheses)?;

                    let tuple_reg = self.alloc_reg();
                    instructions.push(Inst::TupleAlloc {
                        dst: tuple_reg.clone(),
                        element_types: element_types.clone()
                    });

                    for (i, elem_reg) in elements.iter().enumerate() {
                        instructions.push(Inst::TupleStore {
                            tuple_reg: tuple_reg.clone(),
                            index: i,
                            value: Value::Reg(elem_reg.clone()),
                            ty: element_types[i].clone()
                        });
                    }

                    instructions.push(Inst::Move {
                        dst: result_reg.clone(),
                        src: Value::Reg(tuple_reg),
                        ty: Type::Tuple(element_types)
                    });

                    self.reg_types.insert(result_reg.clone(), Type::Tuple(vec![Type::Unknown; elements.len()]));
                }
                else {
                    self.expect(Token::RightParentheses)?;

                        instructions.push(Inst::Move {
                        dst: result_reg.clone(),
                        src: Value::Reg(first_reg),
                        ty: Type::Unknown
                    });
                }        
            }
            Token::String(s) => {
                instructions.push(Inst::StringAlloc {
                    dst: result_reg.clone(),
                    data: s.clone()
                });
                self.advance();

                self.reg_types.insert(result_reg.clone(), Type::String);
            }
            Token::LeftBracket => {
                self.advance();

                let mut elements = Vec::new();
                let mut element_type = Type::Unknown;

                while self.current != Token::RightBracket && self.current != Token::Eof {
                    let (elem_insts, elem_reg) = self.parse_expression()?;
                    instructions.extend(elem_insts);
                    elements.push(elem_reg);

                    // Hardcoded, fix later
                    if element_type == Type::Unknown {
                        element_type = Type::I32
                    }

                    if self.current == Token::Comma {
                        self.advance();
                    }
                }

                self.expect(Token::RightBracket)?;

                let array_size = elements.len();

                let array_reg = self.alloc_reg();
                instructions.push(Inst::ArrayAlloc {
                    dst: array_reg.clone(),
                    element_ty: element_type.clone(),
                    size: Value::Const(Const::I32(array_size as i32))
                });

                for (i, elem_reg) in elements.iter().enumerate() {
                    instructions.push(Inst::IndexStore {
                        array: array_reg.clone(),
                        index: Value::Const(Const::I32(i as i32)),
                        value: Value::Reg(elem_reg.clone()),
                        element_ty: element_type.clone()
                    });
                }

                instructions.push(Inst::Move {
                    dst: result_reg.clone(),
                    src: Value::Reg(array_reg),
                    ty: Type::Array(Box::new(element_type), array_size)
                });
            }
            _ => return Err(format!("Unexpected token in expression: {:?}", self.current))
        }

        Ok((instructions, result_reg))
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        let type_name = self.expect_ident()?;

        if type_name == "Vec" {
            if self.current == Token::Less {
                self.advance();

                let inner_type = self.parse_type()?;
                self.expect(Token::Greater)?;

                return Ok(Type::Vec(Box::new(inner_type)));
            }

            return Ok(Type::Vec(Box::new(Type::Unknown)));
        }

        if type_name == "HashMap" {
            if self.current == Token::Less {
                self.advance();

                let key_type = self.parse_type()?;
                self.expect(Token::Comma)?;
                let value_type = self.parse_type()?;
                self.expect(Token::Greater)?;

                return Ok(Type::HashMap(Box::new(key_type), Box::new(value_type)));
            }
            // Fix with type inference later
            return Ok(Type::HashMap(Box::new(Type::I32), Box::new(Type::I32)));
        }

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

    fn parse_attributes(&mut self) -> Result<Vec<String>, String> {
        let mut attributes = Vec::new();

        while self.current == Token::Hash {
            self.expect(Token::Hash)?;
            self.expect(Token::LeftBracket)?;

            let attr_name = self.expect_ident()?;
            attributes.push(attr_name);

            self.expect(Token::RightBracket)?;
        }

        Ok(attributes)
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