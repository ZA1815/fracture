use fracture_ir::hsir::*;
use fracture_ir::{SyntaxConfig, syntax_config::BlockStyle, DynamicKeywords, GlyphManifest, load_glyph};
use crate::lexer::*;
use crate::errors::{Span, Diagnostic, DiagnosticCollector};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::fs;

pub struct SyntaxProjector {
    config: SyntaxConfig,
    lexer: Lexer,
    current: SpannedToken,
    peek_buffer: Option<SpannedToken>,

    source: String,
    filename: String,

    var_types: HashMap<String, Type>,
    next_reg: u32,
    var_regs: HashMap<String, Reg>,
    next_label: u32,

    struct_defs: HashMap<String, StructDef>,
    reg_types: HashMap<Reg, Type>,

    current_module_path: Vec<String>,
    pending_uses: Vec<UseStatement>,

    diagnostics: DiagnosticCollector,

    known_funcs: Vec<String>,
    func_signatures: HashMap<String, Type>,
    known_vars: Vec<String>,

    dependencies: HashMap<String, PathBuf>,
    active_glyphs: Vec<String>,
    loaded_glyph_manifests: Vec<GlyphManifest>,
    glyph_keywords: DynamicKeywords
}

impl SyntaxProjector {
    pub fn new(input: &str, config: SyntaxConfig, dependencies: HashMap<String, PathBuf>) -> Self {
        let mut lexer = Lexer::new(input, config.clone());
        let current = lexer.next_token();

        Self {
            config,
            lexer,
            current,
            peek_buffer: None,
            source: input.to_string(),
            filename: "<input>".to_string(),
            var_types: HashMap::new(),
            next_reg: 0,
            var_regs: HashMap::new(),
            next_label: 0,
            struct_defs: HashMap::new(),
            reg_types: HashMap::new(),
            current_module_path: Vec::new(),
            pending_uses: Vec::new(),
            diagnostics: DiagnosticCollector::new(input.to_string(), "<input>".to_string()),
            // Will likely change this to include user made functions
            known_funcs: vec![
                "print".to_string(), "println".to_string(),
                "read_line".to_string(), "itoa".to_string(), "to_string".to_string(),
            ],
            func_signatures: {
                let mut m = HashMap::new();
                m.insert("itoa".to_string(), Type::String);
                m.insert("to_string".to_string(), Type::String);
                m.insert("read_line".to_string(), Type::String);
                m.insert("read_file".to_string(), Type::String);
                m.insert("fs_read".to_string(), Type::String);
                m.insert("getcwd".to_string(), Type::String);
                m.insert("current_dir".to_string(), Type::String);
                m.insert("pwd".to_string(), Type::String);
                
                m.insert("sys_write".to_string(), Type::I64);
                m.insert("sys_read".to_string(), Type::I64);
                m.insert("sys_open".to_string(), Type::I32);
                m.insert("sys_close".to_string(), Type::I32);
                m.insert("file_exists".to_string(), Type::Bool);
                m.insert("path_exists".to_string(), Type::Bool);
                m.insert("is_readable".to_string(), Type::Bool);
                m.insert("is_writable".to_string(), Type::Bool);
                m.insert("write_file".to_string(), Type::I64);
                m.insert("fs_write".to_string(), Type::I64);
                m.insert("append_file".to_string(), Type::I64);
                m.insert("fs_append".to_string(), Type::I64);
                m.insert("mkdir".to_string(), Type::I64);
                m.insert("create_dir".to_string(), Type::I64);
                m.insert("rmdir".to_string(), Type::I64);
                m.insert("remove_dir".to_string(), Type::I64);
                m.insert("unlink".to_string(), Type::I64);
                m.insert("remove_file".to_string(), Type::I64);
                m.insert("delete_file".to_string(), Type::I64);
                m.insert("rename".to_string(), Type::I64);
                m.insert("mv".to_string(), Type::I64);
                m.insert("chdir".to_string(), Type::I64);
                m.insert("cd".to_string(), Type::I64);
                m.insert("set_current_dir".to_string(), Type::I64);
                m.insert("file_size".to_string(), Type::I64);
                m.insert("sys_seek".to_string(), Type::I64);
                m.insert("lseek".to_string(), Type::I64);
                m.insert("sys_access".to_string(), Type::I64);
                m.insert("access".to_string(), Type::I64);
                m
            },
            known_vars: Vec::new(),
            dependencies,
            active_glyphs: Vec::new(),
            loaded_glyph_manifests: Vec::new(),
            glyph_keywords: DynamicKeywords::new()
        }
    }

    pub fn with_filename(mut self, filename: &str) -> Self {
        self.filename = filename.to_string();
        self.diagnostics = DiagnosticCollector::new(self.source.clone(), filename.to_string());

        self
    }

    fn load_and_activate_glyph(&mut self, glyph_path: &str) -> Result<(), String> {
        let stdlib_path = self.dependencies.get("std")
            .ok_or_else(|| format!("stdlib dependency not found"))?;

        let manifest = load_glyph(glyph_path, stdlib_path)
            .map_err(|e| format!("Failed to load glyph '{}': {}", glyph_path, e))?;

        println!("[Glyph] Loaded '{}' v{}", manifest.glyph.name, manifest.glyph.version);

        self.glyph_keywords.add_from_glyph(&manifest);

        self.loaded_glyph_manifests.push(manifest);

        self.lexer.set_glyph_keywords(self.glyph_keywords.clone());

        Ok(())
    }

    pub fn peek(&mut self) -> SpannedToken {
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

        while self.current.token != Token::Eof {
            if self.current.token == Token::Newline {
                self.advance();
                continue;
            }

            if self.current.token == Token::Use {
                if let Ok(use_statement) = self.parse_use_statement(Visibility::Private) {
                    root_uses.push(use_statement.clone());
                    self.pending_uses.push(use_statement.clone());

                    if use_statement.import_type == ImportType::Glyph {
                        if let UseTree::Simple { path, .. } = &use_statement.tree {
                            let glyph_path = path.to_string();
                            if !self.active_glyphs.contains(&glyph_path) {
                                if let Err(e) = self.load_and_activate_glyph(&glyph_path) {
                                    let diag = Diagnostic::error(
                                        &format!("Failed to load glyph: {}", e),
                                        self.current.span
                                    );
                                    self.diagnostics.emit(diag);
                                    continue;
                                }
                                self.active_glyphs.push(glyph_path);
                            }
                        }
                    }
                }
                continue;
            }

            if self.current.token == Token::Pub {
                let peek = self.peek().token;
                if peek == Token::Use {
                    self.advance();
                    if let Ok(use_statement) = self.parse_use_statement(Visibility::Public) {
                        root_uses.push(use_statement.clone());
                        self.pending_uses.push(use_statement.clone());

                        if use_statement.import_type == ImportType::Glyph {
                            if let UseTree::Simple { path, .. } = &use_statement.tree {
                                let glyph_path = path.to_string();
                                if !self.active_glyphs.contains(&glyph_path) {
                                    if let Err(e) = self.load_and_activate_glyph(&glyph_path) {
                                        let diag = Diagnostic::error(
                                            &format!("Failed to load glyph: {}", e),
                                            self.current.span
                                        );
                                        self.diagnostics.emit(diag);
                                        continue;
                                    }
                                    self.active_glyphs.push(glyph_path);
                                }
                            }
                        }
                    }
                    continue;
                }
            }

            if self.current.token == Token::Mod {
                if let Ok(module) = self.parse_module_declaration(Visibility::Private) {
                    self.collect_module_functions(&module, &mut all_functions);
                    self.collect_module_structs(&module, &mut all_structs);
                    root_module.add_child(module);
                }
                continue;
            }

            if self.current.token == Token::Pub {
                let peek = self.peek().token;
                if peek == Token::Mod {
                    self.advance();
                    if let Ok(module) = self.parse_module_declaration(Visibility::Public) {
                        self.collect_module_functions(&module, &mut all_functions);
                        self.collect_module_structs(&module, &mut all_structs);
                        root_module.add_child(module);
                    }
                    continue;
                }
            }

            if self.current.token == Token::Struct {
                if let Ok(struct_def) = self.parse_struct(Visibility::Private) {
                    self.struct_defs.insert(struct_def.name.clone(), struct_def.clone());
                    all_structs.insert(struct_def.name.clone(), struct_def.clone());
                    root_module.add_struct(struct_def);
                }
                continue;
            }

            if self.current.token == Token::Pub {
                let peek = self.peek().token;
                if peek == Token::Struct {
                    self.advance();
                    if let Ok(struct_def) = self.parse_struct(Visibility::Public) {
                        self.struct_defs.insert(struct_def.name.clone(), struct_def.clone());
                        all_structs.insert(struct_def.name.clone(), struct_def.clone());
                        root_module.add_struct(struct_def);
                    }
                    continue;
                }
            }

            if self.current.token == Token::Hash {
                if let Ok(attrs) = self.parse_attributes() {
                    let visibility = if self.current.token == Token::Pub {
                        self.advance();
                        Visibility::Public
                    } else {
                        Visibility::Private
                    };

                    if self.current.token != Token::Function {
                        let diag = Diagnostic::error(
                            format!("attributes can only be applied to functions, found `{}`", 
                                self.token_to_string(&self.current.token)),
                            self.current.span
                        );
                        self.diagnostics.emit(diag);
                        self.advance();
                        continue;
                    }

                    if let Ok(mut func) = self.parse_function(visibility) {
                        func.attributes = attrs;
                        self.known_funcs.push(func.name.clone());
                        all_functions.insert(func.name.clone(), func.clone());
                        root_module.add_function(func);
                    }
                }
                continue;
            }

            if self.current.token == Token::Pub {
                let peek = self.peek().token;
                if peek == Token::Function {
                    self.advance();
                    if let Ok(func) = self.parse_function(Visibility::Public) {
                        self.known_funcs.push(func.name.clone());
                        all_functions.insert(func.name.clone(), func.clone());
                        root_module.add_function(func);
                    }
                    continue;
                }
            }

            if self.current.token == Token::Function {
                if let Ok(func) = self.parse_function(Visibility::Private) {
                    self.known_funcs.push(func.name.clone());
                    all_functions.insert(func.name.clone(), func.clone());
                    root_module.add_function(func);
                }
                continue;
            }

            let diag = Diagnostic::error(
                format!("Unexpected token `{}` at module level", self.token_to_string(&self.current.token)),
                self.current.span
            ).with_help("expected `fn`, `struct`, `mod`, or `use`");

            self.diagnostics.emit(diag);
            self.advance();
        }

        for (name, path) in &self.dependencies {
            let entry_v1 = path.join("src/lib.frac");
            let entry_v2 = path.join("shards/src/lib.frac");

            let entry = if entry_v1.exists() {
                Some(entry_v1)
            } else if entry_v2.exists() {
                Some(entry_v2)
            } else {
                None
            };

            if let Some(entry_path) = entry {
                match self.parse_file_module(&entry_path, name, Visibility::Public) {
                    Ok(module) => {
                        self.collect_module_functions(&module, &mut all_functions);
                        self.collect_module_structs(&module, &mut all_structs);
                        root_module.add_child(module);
                    }
                    Err(_e) => {
                        // Dependency failed to parse
                    }
                }
            }
        }

        // Make this use the users syntax mapping instead of hardcoding
        if !all_functions.contains_key("main") {
            let diag = Diagnostic::error(
                "Program must have a `main` function",
                self.current.span
            ).with_help("Add a main function: `fn main() { ... }");

            self.diagnostics.emit(diag);
        }

        if self.diagnostics.has_errors() {
            self.diagnostics.emit_all();

            return Err(format!("{} error(s) found", self.diagnostics.error_count()));
        }

        let mut active_glyphs: Vec<String> = Vec::new();
        for use_stmt in &root_uses {
            if use_stmt.import_type == ImportType::Glyph {
                match &use_stmt.tree {
                    UseTree::Simple { path, .. } => {
                        if let Some(glyph_name) = path.leaf_name() {
                            if !active_glyphs.contains(&glyph_name.to_string()) {
                                active_glyphs.push(glyph_name.to_string());
                                println!("[Parser] Activating glyph: {}", glyph_name);
                            }
                        }
                    }
                    UseTree::Nested { .. } => {
                        let diag = Diagnostic::error(
                            "Nested glyph imports are not yet supported",
                            self.current.span
                        ).with_help("Use individual glyph imports like: use glyph std::type_check;");
                        self.diagnostics.emit(diag);
                    }
                    UseTree::Glob { .. } => {
                        let diag = Diagnostic::error(
                            "Glob glyph imports (use glyph std::*) are not yet supported",
                            self.current.span
                        ).with_help("Use individual glyph imports like: use glyph std::type_check;");
                        self.diagnostics.emit(diag);
                    }
                }
            }
        }

        Ok(Program {
            functions: all_functions,
            structs: all_structs,
            entry: "main".to_string(),
            root_module: {
                root_module.uses = root_uses.clone();
                root_module.active_glyphs = active_glyphs.clone();
                root_module
            },
            uses: root_uses,
            active_glyphs,
        })
    }

    fn parse_module_declaration(&mut self, visibility: Visibility) -> Result<Module, ()> {
        self.expect(Token::Mod)?;
        let module_name = self.expect_ident()?;

        if self.current.token == Token::LeftBrace {
            self.current_module_path.push(module_name.clone());
            
            let mut module = Module::new(&module_name);
            if let ModuleData::Shard(ref mut shard) = module.data {
                shard.visibility = visibility;
            }
            module.path = ModulePath {
                segments: self.current_module_path.iter()
                    .map(|s| PathSegment::Ident(s.clone()))
                    .collect()
            };

            self.advance();
            
            self.parse_module_items(&mut module, Token::RightBrace)?;
            
            self.expect(Token::RightBrace)?;
            self.current_module_path.pop();
            
            Ok(module)
        }
        else {
            if self.config.style.needs_semicolon {
                self.expect(Token::Semicolon)?;
            }
            
            let file_path = self.resolve_module_path(&module_name)?;
            
            println!("[Projector] Loading module '{}' from {:?}", module_name, file_path);

            let module = self.parse_file_module(&file_path, &module_name, visibility)?;
            
            Ok(module)
        }
    }

    fn parse_use_statement(&mut self, visibility: Visibility) -> Result<UseStatement, ()> {
        self.expect(Token::Use)?;

        let import_type = if self.current.token == Token::Glyph {
            self.advance();
            ImportType::Glyph
        }
        else if self.current.token == Token::Shard {
            self.advance();
            ImportType::Shard
        }
        else {
            // Default to Shard for backward compatibility
            ImportType::Shard
        };

        let tree = self.parse_use_tree()?;

        if self.config.style.needs_semicolon {
            self.expect(Token::Semicolon)?;
        }

        Ok(UseStatement { tree, visibility, import_type })
    }

    fn parse_use_tree(&mut self) -> Result<UseTree, ()> {
        let mut path_segments: Vec<PathSegment> = Vec::new();

        loop {
            let segment = match &self.current.token {
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

            if self.current.token == Token::DoubleColon {
                self.advance();

                if self.current.token == Token::Star {
                    self.advance();
                    return Ok(UseTree::Glob { path: ModulePath { segments: path_segments } });
                }

                if self.current.token == Token::LeftBrace {
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

        let alias = if self.current.token == Token::As {
            self.advance();

            Some(self.expect_ident()?)
        }
        else {
            None
        };

        Ok(UseTree::Simple { path: ModulePath { segments: path_segments }, alias })
    }

    fn parse_use_tree_list(&mut self) -> Result<Vec<UseTree>, ()> {
        let mut items = Vec::new();

        while self.current.token != Token::RightBrace && self.current.token != Token::Eof {
            let item = self.parse_use_tree()?;
            items.push(item);

            if self.current.token == Token::Comma {
                self.advance();
            }
            else {
                break;
            }
        }

        Ok(items)
    }

    fn parse_function(&mut self, visibility: Visibility) -> Result<Function, ()> {
        self.var_regs.clear();
        self.var_types.clear();
        self.known_vars.clear();
        self.reg_types.clear();
        self.next_reg = 0;

        self.expect(Token::Function)?;

        let name = self.expect_ident()?;

        self.expect(Token::LeftParentheses)?;

        let mut params = Vec::new();
        while self.current.token != Token::RightParentheses {
            let param_name = self.expect_ident()?;

            let param_type = if self.current.token == Token::Colon {
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

            if self.current.token == Token::Comma {
                self.advance();
            }
        }

        self.expect(Token::RightParentheses)?;

        let return_type = if self.current.token == Token::Arrow {
            self.advance();
            self.parse_type()?
        }
        else {
            Type::Void
        };

        let (mut body, body_result) = self.parse_block()?;

        self.func_signatures.insert(name.clone(), return_type.clone());

        if let Some(reg) = body_result {
            body.push(Inst::Return { val: Some(Value::Reg(reg)) });
        } else if !matches!(return_type, Type::Void) {
            body.push(Inst::Return { val: None });
        }
        else {
            body.push(Inst::Return { val: None });
        }

        let mut locals: HashMap<Reg, Type> = HashMap::new();

        for (reg, ty) in &self.reg_types {
            locals.insert(reg.clone(), ty.clone());
        }

        let module_path = if self.current_module_path.is_empty() {
            Some(ModulePath { segments: vec![PathSegment::Shard] })
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

    fn parse_struct(&mut self, visibility: Visibility) -> Result<StructDef, ()> {
        self.advance();

        let struct_name = self.expect_ident()?;

        self.expect(Token::LeftBrace)?;

        let mut fields = Vec::new();
        let mut field_visibility = HashMap::new();

        while self.current.token != Token::RightBrace && self.current.token != Token::Eof {
            if self.current.token == Token::Newline {
                self.advance();
                continue;
            }

            let field_vis = if self.current.token == Token::Pub {
                self.advance();

                Visibility::Public
            }
            else {
                Visibility::Private
            };

            let field_name = self.expect_ident()?;
            // Maybe customize this later
            self.expect(Token::Colon)?;
            let field_type = self.parse_type();

            fields.push((field_name.clone(), field_type.unwrap()));
            field_visibility.insert(field_name, field_vis);

            if self.current.token == Token::Comma {
                self.advance();
            }

            while self.current.token == Token::Newline {
                self.advance();
            }
        }

        self.advance();

        Ok(StructDef { name: struct_name, fields, visibility, field_visibility })
    }

    fn parse_block(&mut self) -> Result<(Vec<Inst>, Option<Reg>), ()> {
        let mut instructions = Vec::new();
        let mut last_reg = None;

        match self.config.style.block_style {
            BlockStyle::Braces => {
                self.skip_newlines();
                self.expect(Token::LeftBrace)?;

                self.skip_newlines();

                while self.current.token != Token::RightBrace && self.current.token != Token::Eof {
                    let (stmt_insts, stmt_reg) = self.parse_statement()?;
                    instructions.extend(stmt_insts);
                    last_reg = stmt_reg;
                    self.skip_newlines();
                }

                self.expect(Token::RightBrace)?;
            }
            BlockStyle::Indentation => {
                self.expect(Token::Colon)?;
                self.skip_newlines();
                self.expect(Token::Indent)?;

                while self.current.token != Token::Dedent && self.current.token != Token::Eof {
                    let (stmt_insts, stmt_reg) = self.parse_statement()?;
                    instructions.extend(stmt_insts);
                    last_reg = stmt_reg;
                    self.skip_newlines();
                }

                if self.current.token == Token::Dedent {
                    self.advance();
                }
            }
            _ => {
                let diag = Diagnostic::error("Block style not implemented yet", self.current.span);
                self.diagnostics.emit(diag);
                return Err(());
            }
        }

        Ok((instructions, last_reg))
    }

    fn parse_statement(&mut self) -> Result<(Vec<Inst>, Option<Reg>), ()> {
        self.skip_newlines();

        let mut instructions = Vec::new();
        let mut result_reg = None;

        let token = self.current.clone();
        let mut requires_semicolon = true;

        match token.token {
            Token::Let => {
                self.advance();
                if self.current.token == Token::Mut {
                    self.advance();
                }

                let var_name = self.expect_ident()?;
                let var_type = if self.current.token == Token::Colon {
                    self.advance();
                    self.parse_type()?
                }
                else {
                    Type::Unknown
                };

                if self.current.token == Token::Assignment {
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
                if self.current.token != Token::Semicolon && self.current.token != Token::Newline {
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
            Token::For => {
                self.advance();
                instructions.extend(self.parse_for_statement()?);
                requires_semicolon = false;
            }
            Token::Ident(name) => {
                let peek_token = self.peek().token;
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
                    let (expr_insts, reg) = self.parse_expression()?;
                    instructions.extend(expr_insts);
                    result_reg = Some(reg);
                }
            }
            _ => {
                let (expr_insts, reg) = self.parse_expression()?;
                instructions.extend(expr_insts);
                result_reg = Some(reg);
            }
        }

        if self.config.style.needs_semicolon && requires_semicolon {
            if self.current.token == Token::RightBrace && result_reg.is_some() {
                // Implicit return
            }
            else {
                self.expect(Token::Semicolon)?;
                result_reg = None;
            }
        }

        Ok((instructions, result_reg))
    }

    fn parse_if_statement(&mut self) -> Result<Vec<Inst>, ()> {
        let mut instructions = Vec::new();

        let (cond_insts, cond_reg) = self.parse_expression()?;
        instructions.extend(cond_insts);

        let else_label = self.alloc_label();
        let end_label = self.alloc_label();

        instructions.push(Inst::JumpIfFalse { cond: Value::Reg(cond_reg), target: else_label.clone() });

        let (if_body, _) = self.parse_block()?;
        instructions.extend(if_body);

        instructions.push(Inst::Jump { target: end_label.clone() });

        instructions.push(Inst::Label { target: else_label });

        if self.current.token == Token::ElseIf {
            self.advance();
            instructions.extend(self.parse_if_statement()?);
        }

        if self.current.token == Token::Else {
            self.advance();
            let (else_body, _) = self.parse_block()?;
            instructions.extend(else_body);
        }

        instructions.push(Inst::Label { target: end_label });

        Ok(instructions)
    }

    fn parse_while_statement(&mut self) -> Result<Vec<Inst>, ()> {
        let mut instructions = Vec::new();

        let loop_start = self.alloc_label();
        let loop_end = self.alloc_label();

        instructions.push(Inst::Label { target: loop_start.clone() });

        let (cond_insts, cond_reg) = self.parse_expression()?;
        instructions.extend(cond_insts);

        instructions.push(Inst::JumpIfFalse { cond: Value::Reg(cond_reg), target: loop_end.clone() });

        let (body, _) = self.parse_block()?;
        instructions.extend(body);

        instructions.push(Inst::Jump { target: loop_start });

        instructions.push(Inst::Label { target: loop_end });

        Ok(instructions)
    }

    fn parse_for_statement(&mut self) -> Result<Vec<Inst>, ()> {
        let mut instructions = Vec::new();

        let var_name = self.expect_ident()?;

        self.expect(Token::In)?;

        let (start_insts, start_reg) = self.parse_comparison()?;
        instructions.extend(start_insts);

        let is_inclusive = if self.current.token == Token::RangeInclusive {
            self.advance();
            true
        }
        else if self.current.token == Token::Range {
            self.advance();
            false
        }
        else {
            eprintln!("Expected range operator (..) or (..=) in for loop");
            return Err(());
        };

        let (end_insts, end_reg) = self.parse_comparison()?;
        instructions.extend(end_insts);

        let loop_var = self.get_or_create_var(&var_name, &Type::I32);
        instructions.push(Inst::Move {
            dst: loop_var.clone(),
            src: Value::Reg(start_reg),
            ty: Type::I32
        });

        let loop_start = self.alloc_label();
        let loop_end = self.alloc_label();

        instructions.push(Inst::Label { target: loop_start.clone() });

        let cond_reg = self.alloc_reg();
        if is_inclusive {
            instructions.push(Inst::Gt {
                dst: cond_reg.clone(),
                lhs: Value::Reg(loop_var.clone()),
                rhs: Value::Reg(end_reg.clone()),
                ty: Type::I32
            });
        }
        else {
            instructions.push(Inst::Ge {
                dst: cond_reg.clone(),
                lhs: Value::Reg(loop_var.clone()),
                rhs: Value::Reg(end_reg.clone()),
                ty: Type::I32
            });
        }
        self.reg_types.insert(cond_reg.clone(), Type::Bool);

        instructions.push(Inst::JumpIf { cond: Value::Reg(cond_reg), target: loop_end.clone() });

        let (body, _) = self.parse_block()?;
        instructions.extend(body);

        let one_reg = self.alloc_reg();
        instructions.push(Inst::Move {
            dst: one_reg.clone(),
            src: Value::Const(Const::I32(1)),
            ty: Type::I32
        });

        let next_val_reg = self.alloc_reg();
        instructions.push(Inst::Add {
            dst: next_val_reg.clone(),
            lhs: Value::Reg(loop_var.clone()),
            rhs: Value::Reg(one_reg),
            ty: Type::I32
        });
        self.reg_types.insert(next_val_reg.clone(), Type::I32);

        instructions.push(Inst::Move {
            dst: loop_var,
            src: Value::Reg(next_val_reg),
            ty: Type::I32
        });

        instructions.push(Inst::Jump { target: loop_start });

        instructions.push(Inst::Label { target: loop_end });

        Ok(instructions)
    }

    fn parse_expression(&mut self) -> Result<(Vec<Inst>, Reg), ()> {
        self.parse_logical_or()
    }

    fn parse_logical_or(&mut self) -> Result<(Vec<Inst>, Reg), ()> {
        let (mut instructions, mut left_reg) = self.parse_logical_and()?;

        while self.current.token == Token::LogicalOr {
            self.advance();
            let (right_insts, right_reg) = self.parse_logical_and()?;
            instructions.extend(right_insts);

            let result_reg = self.alloc_reg();
            instructions.push(Inst::Or {
                dst: result_reg.clone(),
                lhs: Value::Reg(left_reg),
                rhs: Value::Reg(right_reg),
                ty: Type::Bool
            });

            self.reg_types.insert(result_reg.clone(), Type::Bool);
            left_reg = result_reg;
        }

        Ok((instructions, left_reg))
    }

    fn parse_logical_and(&mut self) -> Result<(Vec<Inst>, Reg), ()> {
        let (mut instructions, mut left_reg) = self.parse_comparison()?;

        while self.current.token == Token::LogicalAnd {
            self.advance();
            let (right_insts, right_reg) = self.parse_comparison()?;
            instructions.extend(right_insts);

            let result_reg = self.alloc_reg();
            instructions.push(Inst::And {
                dst: result_reg.clone(),
                lhs: Value::Reg(left_reg),
                rhs: Value::Reg(right_reg),
                ty: Type::Bool
            });

            self.reg_types.insert(result_reg.clone(), Type::Bool);
            left_reg = result_reg;
        }

        Ok((instructions, left_reg))
    }

    fn parse_comparison(&mut self) -> Result<(Vec<Inst>, Reg), ()> {
        let (mut instructions, mut left_reg) = self.parse_additive()?;

        while self.current.token == Token::DoubleEquals
            || self.current.token == Token::Less
            || self.current.token == Token::Greater
            || self.current.token == Token::GreaterEquals
            || self.current.token == Token::LessEquals
            || self.current.token == Token::NotEquals
        {
            let op = self.current.clone();
            self.advance();

            let (right_insts, right_reg) = self.parse_additive()?;
            instructions.extend(right_insts);

            let result_reg = self.alloc_reg();

            let inst = match op.token {
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
                _ => unreachable!()
            };

            instructions.push(inst);
            left_reg = result_reg;
        }

        Ok((instructions, left_reg))
    }

    fn parse_additive(&mut self) -> Result<(Vec<Inst>, Reg), ()> {
        let (mut instructions, mut left_reg) = self.parse_multiplicative()?;

        while self.current.token == Token::Plus || self.current.token == Token::Minus {
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
                if op.token != Token::Plus {
                    let diag = Diagnostic::error(
                        "Cannot use `-` operator with strings",
                        self.current.span
                    ).with_help("Use `+` to concatenate strings");
                    self.diagnostics.emit(diag);
                    return Err(());
                }

                instructions.push(Inst::StringConcat {
                    dst: result_reg.clone(),
                    left: left_reg,
                    right: right_reg
                });

                self.reg_types.insert(result_reg.clone(), Type::String);
            }
            else {
                let inst = match op.token {
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

    fn parse_multiplicative(&mut self) -> Result<(Vec<Inst>, Reg), ()> {
        let (mut instructions, mut left_reg) = self.parse_postfix()?;

        while self.current.token == Token::Star || self.current.token == Token::Slash {
            let op = self.current.clone();
            self.advance();

            let (right_insts, right_reg) = self.parse_postfix()?;
            instructions.extend(right_insts);

            let result_reg = self.alloc_reg();

            let inst = match op.token {
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

    fn parse_postfix(&mut self) -> Result<(Vec<Inst>, Reg), ()> {
        let (mut instructions, mut reg) = self.parse_term()?;

        loop {
            let result_reg = self.alloc_reg();
            if self.current.token == Token::Dot {
                self.advance();

                match &self.current.token {
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
                        let field_span = self.current.span;
                        self.advance();

                        if self.current.token == Token::LeftParentheses {
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
                                "is_some" => {
                                    self.expect(Token::RightParentheses)?;

                                    instructions.push(Inst::OptionIsSome { dst: result_reg.clone(), option: reg });
                                    self.reg_types.insert(result_reg.clone(), Type::Bool);
                                }
                                "is_none" => {
                                    self.expect(Token::RightParentheses)?;

                                    instructions.push(Inst::OptionIsNone { dst: result_reg.clone(), option: reg });
                                    self.reg_types.insert(result_reg.clone(), Type::Bool);
                                }
                                "unwrap" => {
                                    self.expect(Token::RightParentheses)?;

                                    let is_option = reg_type.as_ref()
                                        .map(|t| matches!(t, Type::Option(_)))
                                        .unwrap_or(false);
                                    let is_result = reg_type.as_ref()
                                        .map(|t| matches!(t, Type::Result(_, _)))
                                        .unwrap_or(false);

                                    if is_option {
                                        let inner_ty = if let Some(Type::Option(inner)) = &reg_type {
                                            (**inner).clone()
                                        }
                                        else {
                                            Type::Unknown
                                        };

                                        instructions.push(Inst::OptionUnwrap {
                                            dst: result_reg.clone(),
                                            option: reg,
                                            inner_ty: inner_ty.clone()
                                        });

                                        self.reg_types.insert(result_reg.clone(), inner_ty);
                                    }
                                    else if is_result {
                                        let ok_ty = if let Some(Type::Result(ok, _)) = &reg_type {
                                            (**ok).clone()
                                        }
                                        else {
                                            Type::Unknown
                                        };

                                        instructions.push(Inst::ResultUnwrap {
                                            dst: result_reg.clone(),
                                            result: reg,
                                            ok_ty: ok_ty.clone()
                                        });

                                        self.reg_types.insert(result_reg.clone(), ok_ty);
                                    }
                                    else {
                                        let diag = Diagnostic::error(
                                            "unwrap() can only be called on Option or Result types",
                                            field_span
                                        );
                                        self.diagnostics.emit(diag);
                                        return Err(());
                                    }
                                }
                                "unwrap_or" => {
                                    let (default_insts, default_reg) = self.parse_expression()?;
                                    instructions.extend(default_insts);
                                    self.expect(Token::RightParentheses)?;

                                    let is_option = reg_type.as_ref()
                                        .map(|t| matches!(t, Type::Option(_)))
                                        .unwrap_or(false);
                                    let is_result = reg_type.as_ref()
                                        .map(|t| matches!(t, Type::Result(_, _)))
                                        .unwrap_or(false);

                                    if is_option {
                                        let inner_ty = if let Some(Type::Option(inner)) = &reg_type {
                                            (**inner).clone()
                                        } else {
                                            Type::Unknown
                                        };
                                        
                                        instructions.push(Inst::OptionUnwrapOr {
                                            dst: result_reg.clone(),
                                            option: reg,
                                            default: Value::Reg(default_reg),
                                            inner_ty: inner_ty.clone(),
                                        });
                                        
                                        self.reg_types.insert(result_reg.clone(), inner_ty);
                                    }
                                    else if is_result {
                                        let ok_ty = if let Some(Type::Result(ok, _)) = &reg_type {
                                            (**ok).clone()
                                        } else {
                                            Type::Unknown
                                        };
                                        
                                        instructions.push(Inst::ResultUnwrapOr {
                                            dst: result_reg.clone(),
                                            result: reg,
                                            default: Value::Reg(default_reg),
                                            ok_ty: ok_ty.clone(),
                                        });
                                        
                                        self.reg_types.insert(result_reg.clone(), ok_ty);
                                    }
                                    else {
                                        let diag = Diagnostic::error(
                                            "unwrap_or() can only be called on Option or Result types",
                                            field_span
                                        );
                                        self.diagnostics.emit(diag);
                                        return Err(());
                                    }
                                }
                                "is_ok" => {
                                    self.expect(Token::RightParentheses);
                                    
                                    instructions.push(Inst::ResultIsOk {
                                        dst: result_reg.clone(),
                                        result: reg,
                                    });
                                    
                                    self.reg_types.insert(result_reg.clone(), Type::Bool);
                                }
                                "is_err" => {
                                    self.expect(Token::RightParentheses);
                                    
                                    instructions.push(Inst::ResultIsErr {
                                        dst: result_reg.clone(),
                                        result: reg,
                                    });
                                    
                                    self.reg_types.insert(result_reg.clone(), Type::Bool);
                                }
                                "unwrap_err" => {
                                    self.expect(Token::RightParentheses);
                                    
                                    let err_ty = if let Some(Type::Result(_, err)) = &reg_type {
                                        (**err).clone()
                                    }
                                    else {
                                        Type::Unknown
                                    };
                                    
                                    instructions.push(Inst::ResultUnwrapErr {
                                        dst: result_reg.clone(),
                                        result: reg,
                                        err_ty: err_ty.clone(),
                                    });
                                    
                                    self.reg_types.insert(result_reg.clone(), err_ty);
                                }
                                "expect" => {
                                    let (msg_insts, _msg_reg) = self.parse_expression()?;
                                    instructions.extend(msg_insts);
                                    self.expect(Token::RightParentheses)?;
                                    
                                    let ok_ty = if let Some(Type::Result(ok, _)) = &reg_type {
                                        (**ok).clone()
                                    } else {
                                        Type::Unknown
                                    };
                                    
                                    // Using static message (change later)
                                    instructions.push(Inst::ResultExpect {
                                        dst: result_reg.clone(),
                                        result: reg,
                                        message: "expectation failed".to_string(),
                                        ok_ty: ok_ty.clone(),
                                    });
                                    
                                    self.reg_types.insert(result_reg.clone(), ok_ty);
                                }
                                _ => {
                                    let diag = Diagnostic::error(
                                        format!("Unknown method `{}`", field),
                                        field_span
                                    );
                                    self.diagnostics.emit(diag);
                                    
                                    while self.current.token != Token::RightParentheses && self.current.token != Token::Eof {
                                        self.advance();
                                    }
                                    self.advance();
                                    continue;
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
                    _ => {
                        let diag = Diagnostic::error(
                            format!("Expected field name after `.`, found `{}`",
                                self.token_to_string(&self.current.token)),
                            self.current.span
                        );
                        self.diagnostics.emit(diag);
                        return Err(());
                    }
                }
                reg = result_reg;

                continue;
            }

            if self.current.token == Token::LeftBracket {
                self.advance();

                let (first_insts, first_reg) = self.parse_expression()?;
                instructions.extend(first_insts);

                if self.current.token == Token::Range {
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
            if self.current.token == Token::QuestionMark {
                self.advance();

                let reg_type = self.reg_types.get(&reg).cloned();

                if let Some(Type::Result(ok_ty, err_ty)) = reg_type {
                    let error_label = self.alloc_label();
                    let continue_label = self.alloc_label();

                    let is_err_reg = self.alloc_reg();
                    instructions.push(Inst::ResultIsErr {
                        dst: is_err_reg.clone(),
                        result: reg.clone()
                    });

                    instructions.push(Inst::JumpIfFalse {
                        cond: Value::Reg(is_err_reg),
                        target: continue_label.clone()
                    });

                    let err_val_reg = self.alloc_reg();
                    instructions.push(Inst::ResultUnwrapErr {
                        dst: err_val_reg.clone(),
                        result: reg.clone(),
                        err_ty: (*err_ty).clone()
                    });

                    let return_err_reg = self.alloc_reg();
                    instructions.push(Inst::ResultErr {
                        dst: return_err_reg.clone(),
                        error: Value::Reg(err_val_reg),
                        ok_ty: Type::Unknown,
                        err_ty: (*err_ty).clone()
                    });

                    instructions.push(Inst::Return { val: Some(Value::Reg(return_err_reg)) });

                    instructions.push(Inst::Label { target: continue_label });
                    
                    let unwrapped_reg = self.alloc_reg();
                    instructions.push(Inst::ResultUnwrap {
                        dst: unwrapped_reg.clone(),
                        result: reg,
                        ok_ty: (*ok_ty).clone()
                    });

                    self.reg_types.insert(unwrapped_reg.clone(), (*ok_ty).clone());
                    reg = unwrapped_reg;

                    continue;
                }
                else if let Some(Type::Option(inner_ty)) = reg_type {
                    let none_label = self.alloc_label();
                    let continue_label = self.alloc_label();

                    let is_none_reg = self.alloc_reg();
                    instructions.push(Inst::OptionIsNone {
                        dst: is_none_reg.clone(),
                        option: reg.clone()
                    });

                    instructions.push(Inst::JumpIfFalse {
                        cond: Value::Reg(is_none_reg),
                        target: continue_label.clone()
                    });

                    let return_none_reg = self.alloc_reg();
                    instructions.push(Inst::OptionNone {
                        dst: return_none_reg.clone(),
                        inner_ty: Type::Unknown
                    });

                    instructions.push(Inst::Return { val: Some(Value::Reg(return_none_reg)) });

                    instructions.push(Inst::Label { target: continue_label });
                    
                    let unwrapped_reg = self.alloc_reg();
                    instructions.push(Inst::OptionUnwrap {
                        dst: unwrapped_reg.clone(),
                        option: reg,
                        inner_ty: (*inner_ty).clone(),
                    });
                    
                    self.reg_types.insert(unwrapped_reg.clone(), (*inner_ty).clone());
                    reg = unwrapped_reg;
                    
                    continue;
                }
                else {
                    let diag = Diagnostic::error(
                        "The `?` operator can only be used on Result or Option types",
                        self.current.span
                    );
                    self.diagnostics.emit(diag);
                }
            }
            break;
        }

        Ok((instructions, reg))
    }

    fn parse_term(&mut self) -> Result<(Vec<Inst>, Reg), ()> {
        let mut instructions = Vec::new();
        let result_reg = self.alloc_reg();

        match self.current.token.clone() {
            Token::Number(n) => {
                instructions.push(Inst::Move {
                    dst: result_reg.clone(),
                    src: Value::Const(Const::I32(n as i32)),
                    ty: Type::I32
                });
                self.advance();
            }
            Token::Bool(b) => {
                instructions.push(Inst::Move {
                    dst: result_reg.clone(),
                    src: Value::Const(Const::Bool(b)),
                    ty: Type::Bool
                });
                self.advance();
            }
            Token::GlyphKeyword(keyword, semantic_type) => {
                match semantic_type.as_str() {
                    "option_some" => {
                        self.advance();
                        self.expect(Token::LeftParentheses)?;

                        let (val_insts, val_reg) = self.parse_expression()?;
                        instructions.extend(val_insts);

                        self.expect(Token::RightParentheses)?;

                        let inner_ty = self.reg_types.get(&val_reg).cloned().unwrap_or(Type::Unknown);

                        instructions.push(Inst::OptionSome {
                            dst: result_reg.clone(),
                            value: Value::Reg(val_reg),
                            inner_ty: inner_ty.clone()
                        });

                        self.reg_types.insert(result_reg.clone(), Type::Option(Box::new(inner_ty)));
                    }
                    "option_none" => {
                        self.advance();

                        let inner_ty = Type::Unknown;

                        instructions.push(Inst::OptionNone {
                            dst: result_reg.clone(),
                            inner_ty: inner_ty.clone()
                        });

                        self.reg_types.insert(result_reg.clone(), Type::Option(Box::new(inner_ty)));
                    }
                    "result_ok" => {
                        self.advance();
                        self.expect(Token::LeftParentheses)?;

                        let (val_insts, val_reg) = self.parse_expression()?;
                        instructions.extend(val_insts);

                        self.expect(Token::RightParentheses)?;

                        let ok_ty = self.reg_types.get(&val_reg).cloned().unwrap_or(Type::Unknown);
                        let err_ty = Type::Unknown;

                        instructions.push(Inst::ResultOk {
                            dst: result_reg.clone(),
                            value: Value::Reg(val_reg),
                            ok_ty: ok_ty.clone(),
                            err_ty: err_ty.clone()
                        });

                        self.reg_types.insert(result_reg.clone(), Type::Result(Box::new(ok_ty), Box::new(err_ty)));
                    }
                    "result_err" => {
                        self.advance();
                        self.expect(Token::LeftParentheses)?;

                        let (err_insts, err_reg) = self.parse_expression()?;
                        instructions.extend(err_insts);

                        self.expect(Token::RightParentheses)?;

                        let ok_ty = Type::Unknown;
                        let err_ty = self.reg_types.get(&err_reg).cloned().unwrap_or(Type::Unknown);

                        instructions.push(Inst::ResultErr {
                            dst: result_reg.clone(),
                            error: Value::Reg(err_reg),
                            ok_ty: ok_ty.clone(),
                            err_ty: err_ty.clone(),
                        });

                        self.reg_types.insert(result_reg.clone(), Type::Result(Box::new(ok_ty), Box::new(err_ty)));
                    }
                    _ => {
                        let diag = Diagnostic::error(
                            &format!("Unexpected glyph keyword '{}' in expression", keyword),
                            self.current.span
                        ).with_help("Glyph keywords can only be used in specific contexts");
                        self.diagnostics.emit(diag);
                        return Err(());
                    }
                }
            }
            Token::Panic => {
                self.advance();
                self.expect(Token::LeftParentheses)?;

                if let Token::String(msg) = self.current.token.clone() {
                    let message = msg.clone();
                    self.advance();
                    self.expect(Token::RightParentheses)?;

                    instructions.push(Inst::PanicStatic { message });
                }
                else {
                    let (msg_insts, msg_reg) = self.parse_expression()?;
                    instructions.extend(msg_insts);
                    self.expect(Token::RightParentheses)?;

                    instructions.push(Inst::Panic { message: msg_reg });
                }

                instructions.push(Inst::Move {
                    dst: result_reg.clone(),
                    src: Value::Const(Const::I32(0)),
                    ty: Type::I32
                });
            }
            Token::Ident(name) => {
                let name = name.clone();
                let ident_span = self.current.span;
                self.advance();

                if self.current.token == Token::DoubleColon {
                    let mut path_parts = if let Some(resolved) = self.resolve_import(&name) {
                        resolved
                    }
                    else {
                        vec![name.clone()]
                    };

                    while self.current.token == Token::DoubleColon {
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
                    else if self.current.token == Token::LeftParentheses {
                        self.advance();

                        let mut args = Vec::new();
                        while self.current.token != Token::RightParentheses {
                            let (arg_insts, arg_reg) = self.parse_expression()?;
                            instructions.extend(arg_insts);
                            args.push(Value::Reg(arg_reg));

                            if self.current.token == Token::Comma {
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
                        let diag = Diagnostic::error(
                            format!("Unknown path expression: {}", full_path),
                            ident_span
                        );
                        self.diagnostics.emit(diag);

                        return Err(());
                    }
                }



                if self.current.token == Token::LeftParentheses {    
                    self.advance();

                    let mut args = Vec::new();
                    while self.current.token != Token::RightParentheses {
                        let (arg_insts, arg_reg) = self.parse_expression()?;
                        instructions.extend(arg_insts);
                        args.push(Value::Reg(arg_reg));

                        if self.current.token == Token::Comma {
                            self.advance();
                        }
                    }

                    self.expect(Token::RightParentheses)?;

                    if name == "sys_write" {
                        if args.len() != 3 {
                             let diag = Diagnostic::error(
                                format!("sys_write expects 3 arguments, found {}", args.len()),
                                ident_span
                            );
                            self.diagnostics.emit(diag);
                        } else {
                            let fd = args[0].clone();
                            let buf_val = args[1].clone();
                            let len = args[2].clone();

                            if let Value::Reg(buf_reg) = buf_val {
                                instructions.push(Inst::SysWrite {
                                    fd,
                                    buf: buf_reg,
                                    len,
                                    result_dst: result_reg.clone()
                                });
                                self.reg_types.insert(result_reg.clone(), Type::I64);
                            } else {
                                 let diag = Diagnostic::error(
                                    "sys_write buffer argument must be a variable/register".to_string(),
                                    ident_span
                                );
                                self.diagnostics.emit(diag);
                            }
                        }
                    }
                    else {
                        let call_ty = self.func_signatures.get(&name).cloned().unwrap_or(Type::Unknown);
                        self.reg_types.insert(result_reg.clone(), call_ty.clone());

                        instructions.push(Inst::Call {
                            dst: Some(result_reg.clone()),
                            func: Value::Label(Label(name.to_string())),
                            args,
                            ty: call_ty
                        });
                    }
                }
                else if self.current.token == Token::LeftBrace && self.struct_defs.contains_key(&name) {
                    self.advance();
                    self.skip_newlines();

                    let struct_reg = self.alloc_reg();
                    instructions.push(Inst::StructAlloc {
                        dst: struct_reg.clone(),
                        struct_name: name.to_string()
                    });

                    while self.current.token != Token::RightBrace && self.current.token != Token::Eof {
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

                        if self.current.token == Token::Comma {
                            self.advance();
                        }

                        self.skip_newlines();
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
                        self.error_undefined_variable(&name, ident_span);

                        instructions.push(Inst::Move {
                            dst: result_reg.clone(),
                            src: Value::Const(Const::I32(0)),
                            ty: Type::I32
                        });
                    }
                }
            }
            Token::LeftParentheses => {
                self.advance();

                let (first_insts, first_reg) = self.parse_expression()?;
                instructions.extend(first_insts);

                if self.current.token == Token::Comma {
                    self.advance();

                    let mut elements = vec![first_reg];
                    let mut element_types = vec![Type::Unknown];

                    while self.current.token != Token::RightParentheses && self.current.token != Token::Eof {
                        let (elem_insts, elem_reg) = self.parse_expression()?;
                        instructions.extend(elem_insts);
                        elements.push(elem_reg);
                        element_types.push(Type::Unknown);

                        if self.current.token == Token::Comma {
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

                while self.current.token != Token::RightBracket && self.current.token != Token::Eof {
                    let (elem_insts, elem_reg) = self.parse_expression()?;
                    instructions.extend(elem_insts);
                    elements.push(elem_reg);

                    // Hardcoded, fix later
                    if element_type == Type::Unknown {
                        element_type = Type::I32
                    }

                    if self.current.token == Token::Comma {
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
            Token::Minus => {
                self.advance();
                let (term_insts, term_reg) = self.parse_term()?;
                instructions.extend(term_insts);

                let zero_reg = self.alloc_reg();
                instructions.push(Inst::Move {
                    dst: zero_reg.clone(),
                    src: Value::Const(Const::I32(0)),
                    ty: Type::I32
                });

                instructions.push(Inst::Sub {
                    dst: result_reg.clone(),
                    lhs: Value::Reg(zero_reg),
                    rhs: Value::Reg(term_reg),
                    ty: Type::I32
                });

                self.reg_types.insert(result_reg.clone(), Type::I32);
            }
            Token::Not => {
                self.advance();
                let (term_insts, term_reg) = self.parse_term()?;
                instructions.extend(term_insts);

                instructions.push(Inst::Not {
                    dst: result_reg.clone(),
                    src: Value::Reg(term_reg),
                    ty: Type::Bool
                });

                self.reg_types.insert(result_reg.clone(), Type::Bool);
            }
            Token::If => {
                self.advance();

                let (cond_insts, cond_reg) = self.parse_expression()?;
                instructions.extend(cond_insts);

                let else_label = self.alloc_label();
                let end_label = self.alloc_label();

                instructions.push(Inst::JumpIfFalse {
                    cond: Value::Reg(cond_reg),
                    target: else_label.clone()
                });

                self.skip_newlines();
                self.expect(Token::LeftBrace)?;
                self.skip_newlines();
                let then_value_reg = if self.current.token != Token::RightBrace {
                    let (block_insts, block_reg) = self.parse_expression()?;
                    instructions.extend(block_insts);
                    self.skip_newlines();
                    block_reg
                }
                else {
                    let unit_reg = self.alloc_reg();
                    instructions.push(Inst::Move {
                        dst: unit_reg.clone(),
                        src: Value::Const(Const::I32(0)),
                        ty: Type::I32
                    });
                    unit_reg
                };
                self.expect(Token::RightBrace)?;

                instructions.push(Inst::Move {
                    dst: result_reg.clone(),
                    src: Value::Reg(then_value_reg),
                    ty: Type::I32
                });

                instructions.push(Inst::Jump { target: end_label.clone() });

                instructions.push(Inst::Label { target: else_label });

                self.skip_newlines();
                self.expect(Token::Else)?;
                self.skip_newlines();
                self.expect(Token::LeftBrace)?;
                self.skip_newlines();

                let else_value_reg = if self.current.token != Token::RightBrace {
                    let (block_insts, block_reg) = self.parse_expression()?;
                    instructions.extend(block_insts);
                    self.skip_newlines();
                    block_reg
                }
                else {
                    let unit_reg = self.alloc_reg();
                    instructions.push(Inst::Move {
                        dst: unit_reg.clone(),
                        src: Value::Const(Const::I32(0)),
                        ty: Type::I32
                    });
                    unit_reg
                };
                self.expect(Token::RightBrace)?;

                instructions.push(Inst::Move {
                    dst: result_reg.clone(),
                    src: Value::Reg(else_value_reg),
                    ty: Type::I32
                });

                instructions.push(Inst::Label { target: end_label });

                self.reg_types.insert(result_reg.clone(), Type::I32);
            }
            _ => {
                let diag = Diagnostic::error(
                    format!("Unexpected token `{}` in expression",
                        self.token_to_string(&self.current.token)),
                    self.current.span
                );
                self.diagnostics.emit(diag);
                
                self.advance();
                instructions.push(Inst::Move {
                    dst: result_reg.clone(),
                    src: Value::Const(Const::I32(0)),
                    ty: Type::I32
                });
            }
        }

        Ok((instructions, result_reg))
    }

    fn parse_type(&mut self) -> Result<Type, ()> {
        let type_name = self.expect_ident()?;

        if type_name == "Vec" {
            if self.current.token == Token::Less {
                self.advance();

                let inner_type = self.parse_type()?;
                self.expect(Token::Greater)?;

                return Ok(Type::Vec(Box::new(inner_type)));
            }

            return Ok(Type::Vec(Box::new(Type::Unknown)));
        }
        if type_name == "HashMap" {
            if self.current.token == Token::Less {
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
        if type_name == "Option" {
            if self.current.token == Token::Less {
                self.advance();
                let inner_type = self.parse_type()?;
                self.expect(Token::Greater)?;
                return Ok(Type::Option(Box::new(inner_type)));
            }
            return Ok(Type::Option(Box::new(Type::Unknown)));
        }
        if type_name == "Result" {
            if self.current.token == Token::Less {
                self.advance();
                let ok_type = self.parse_type()?;
                self.expect(Token::Comma)?;
                let err_type = self.parse_type()?;
                self.expect(Token::Greater)?;
                return Ok(Type::Result(Box::new(ok_type), Box::new(err_type)));
            }
            return Ok(Type::Result(Box::new(Type::Unknown), Box::new(Type::Unknown)));
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

    fn parse_attributes(&mut self) -> Result<Vec<String>, ()> {
        let mut attributes = Vec::new();

        while self.current.token == Token::Hash {
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
            self.reg_types.insert(reg.clone(), ty.clone());

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

        while matches!(self.current.token, Token::Comment(_)) {
            if let Some(token) = self.peek_buffer.take() {
                self.current = token;
            }
            else {
                self.current = self.lexer.next_token();
            }
        }
    }

    fn skip_newlines(&mut self) {
        while self.current.token == Token::Newline {
            self.current = self.lexer.next_token();
        }
    }

    fn expect(&mut self, expected: Token) -> Result<SpannedToken, ()> {
        if std::mem::discriminant(&self.current.token) == std::mem::discriminant(&expected) {
            let token = self.current.clone();
            self.advance();

            Ok(token)
        }
        else {
            let found_str = self.token_to_string(&self.current.token);
            let expected_str = self.token_to_string(&expected);

            let diag = Diagnostic::unexpected_token(&found_str, &expected_str, self.current.span)
                .with_note(format!("while parsing at this location"));

            self.diagnostics.emit(diag);

            Err(())
        }
    }

    fn expect_ident(&mut self) -> Result<String, ()> {
        match &self.current.token {
            Token::Ident(name) => {
                let n = name.clone();
                self.advance();
                
                Ok(n)
            }
            _ => {
                let found = self.token_to_string(&self.current.token);
                let diag = Diagnostic::error(
                    format!("Expected identifier, found `{}`", found),
                    self.current.span
                );

                self.diagnostics.emit(diag);

                Err(())
            }
        }
    }

    fn token_to_string(&self, token: &Token) -> String {
        match token {
            Token::Function => self.config.keywords.function_kw.clone(),
            Token::Return => self.config.keywords.return_kw.clone(),
            Token::If => self.config.keywords.if_kw.clone(),
            Token::Else => self.config.keywords.else_kw.clone(),
            Token::While => self.config.keywords.while_kw.clone(),
            Token::For => self.config.keywords.for_kw.clone(),
            Token::Let => self.config.keywords.let_kw.clone(),
            Token::Struct => self.config.keywords.struct_kw.clone(),
            Token::Ident(s) => s.clone(),
            Token::Number(n) => n.to_string(),
            Token::Float(f) => f.to_string(),
            Token::String(s) => format!("\"{}\"", s),
            Token::Bool(b) => b.to_string(),
            Token::Plus => "+".to_string(),
            Token::Minus => "-".to_string(),
            Token::Star => "*".to_string(),
            Token::Slash => "/".to_string(),
            Token::Assignment => "=".to_string(),
            Token::DoubleEquals => "==".to_string(),
            Token::Less => "<".to_string(),
            Token::Greater => ">".to_string(),
            Token::LeftParentheses => "(".to_string(),
            Token::RightParentheses => ")".to_string(),
            Token::LeftBrace => "{".to_string(),
            Token::RightBrace => "}".to_string(),
            Token::LeftBracket => "[".to_string(),
            Token::RightBracket => "]".to_string(),
            Token::Semicolon => ";".to_string(),
            Token::Colon => ":".to_string(),
            Token::Comma => ",".to_string(),
            Token::Arrow => "->".to_string(),
            Token::Dot => ".".to_string(),
            Token::Eof => "end of file".to_string(),
            Token::Newline => "newline".to_string(),
            Token::GlyphKeyword(keyword, _) => keyword.clone(),
            Token::Match => "match".to_string(),
            Token::QuestionMark => "?".to_string(),
            Token::Panic => "panic".to_string(),
            Token::Glyph => self.config.keywords.glyph_kw.clone(),
            Token::Shard => self.config.keywords.shard_kw.clone(),
            _ => format!("{:?}", token),
        }
    }

    fn error_undefined_variable(&mut self, name: &str, span: Span) {
        let known: Vec<&str> = self.var_regs.keys().map(|s| s.as_str()).collect();

        let diag = Diagnostic::undefined_variable(name, span, &known);

        self.diagnostics.emit(diag);
    }

    fn error_undefined_function(&mut self, name: &str, span: Span) {
        let known: Vec<&str> = self.known_funcs.iter().map(|s| s.as_str()).collect();

        let diag = Diagnostic::undefined_function(name, span, &known);

        self.diagnostics.emit(diag);
    }

    fn resolve_module_path(&mut self, mod_name: &str) -> Result<PathBuf, ()> {
        if let Some(root_path) = self.dependencies.get(mod_name) {
            // Try both src/lib.frac and shards/src/lib.frac
            let entry_v1 = root_path.join("src/lib.frac");
            if entry_v1.exists() {
                return Ok(entry_v1);
            }
            let entry_v2 = root_path.join("shards/src/lib.frac");
            if entry_v2.exists() {
                return Ok(entry_v2);
            }
        }

        let current_path = Path::new(&self.filename);
        let parent_dir = current_path.parent().unwrap_or(Path::new("."));

        let file_ext = "frac";

        let path_dir = parent_dir.join(format!("{}.{}", mod_name, file_ext));
        if path_dir.exists() {
            return Ok(path_dir);
        }

        let path_nested = parent_dir.join(mod_name).join(format!("mod.{}", file_ext));
        if path_nested.exists() {
            return Ok(path_nested);
        }

        let diag = Diagnostic::error(
            format!("Could not find module `{}`", mod_name),
            self.current.span
        ).with_note(format!("Checked {:?} and {:?}", path_dir, path_nested));

        self.diagnostics.emit(diag);
        Err(())
    }

    fn get_syntax_config_for_file(&self, file_path: &Path) -> SyntaxConfig {
        for (dep_name, dep_path) in &self.dependencies {
            if file_path.starts_with(dep_path) {
                let dep_manifest_path = dep_path.join("rift.toml");
                if dep_manifest_path.exists() {
                    // Parse the rift.toml and extract syntax config properly
                    return SyntaxConfig::fss();
                }
            }
        }

        self.config.clone()
    }

    fn parse_file_module(&self, path: &Path, mod_name: &str, visibility: Visibility) -> Result<Module, ()> {
        let source = fs::read_to_string(path).map_err(|_| { () })?;

        let file_config = self.get_syntax_config_for_file(path);

        let mut sub_projector = SyntaxProjector::new(&source, file_config, self.dependencies.clone())
            .with_filename(path.to_str().unwrap());

        let mut new_mod_path = self.current_module_path.clone();
        new_mod_path.push(mod_name.to_string());
        sub_projector.current_module_path = new_mod_path;

        let mut module = Module::new(mod_name);
        if let ModuleData::Shard(ref mut shard) = module.data {
            shard.visibility = visibility;
        }
        module.path = ModulePath {
            segments: sub_projector.current_module_path.iter()
                .map(|s| PathSegment::Ident(s.clone()))
                .collect()
        };

        sub_projector.parse_module_items(&mut module, Token::Eof)?;

        if sub_projector.diagnostics.has_errors() {
            sub_projector.diagnostics.emit_all();
            return Err(());
        }

        Ok(module)
    }

    fn parse_module_items(&mut self, module: &mut Module, terminator: Token) -> Result<(), ()> {
        while self.current.token != terminator && self.current.token != Token::Eof {
            if self.current.token == Token::Newline {
                self.advance();
                continue;
            }

            if self.current.token == Token::Use {
                let use_stmt = self.parse_use_statement(Visibility::Private)?;
                module.uses.push(use_stmt.clone());
                self.pending_uses.push(use_stmt);
                continue;
            }

            if self.current.token == Token::Pub && self.peek().token == Token::Use {
                self.advance();
                let use_stmt = self.parse_use_statement(Visibility::Public)?;
                module.uses.push(use_stmt.clone());
                self.pending_uses.push(use_stmt);
                continue;
            }

            if self.current.token == Token::Mod {
                let submodule = self.parse_module_declaration(Visibility::Private)?;
                module.add_child(submodule);
                continue;
            }

            if self.current.token == Token::Pub && self.peek().token == Token::Mod {
                self.advance();
                let submodule = self.parse_module_declaration(Visibility::Public)?;
                module.add_child(submodule);
                continue;
            }

            if self.current.token == Token::Function {
                let func = self.parse_function(Visibility::Private)?;
                module.add_function(func);
                continue;
            }

            if self.current.token == Token::Pub && self.peek().token == Token::Function {
                self.advance();
                let func = self.parse_function(Visibility::Public)?;
                module.add_function(func);
                continue;
            }

            if self.current.token == Token::Struct {
                let s = self.parse_struct(Visibility::Private)?;
                module.add_struct(s);
                continue;
            }

            if self.current.token == Token::Pub && self.peek().token == Token::Struct {
                self.advance();
                let s = self.parse_struct(Visibility::Public)?;
                module.add_struct(s);
                continue;
            }

            let diag = Diagnostic::error(
                format!("Unexpected token `{}` in module", self.token_to_string(&self.current.token)),
                self.current.span
            );
            self.diagnostics.emit(diag);
            self.advance();
        }

        Ok(())
    }

    fn resolve_import(&self, name: &str) -> Option<Vec<String>> {
        for use_stmt in &self.pending_uses {
            match &use_stmt.tree {
                UseTree::Simple { path, alias } => {
                    let match_name = if let Some(alias) = alias {
                        alias
                    }
                    else {
                        if let Some(PathSegment::Ident(last)) = path.segments.last() {
                            last
                        }
                        else {
                            continue;
                        }
                    };

                    if match_name == name {
                        return Some(path.segments.iter().map(|s| {
                            match s {
                                PathSegment::Ident(i) => i.clone(),
                                PathSegment::Shard => "shard".to_string(),
                                PathSegment::SelfKw => "self".to_string(),
                                PathSegment::Super => "super".to_string()
                            }
                        }).collect());
                    }
                }
                _ => {}
            }
        }
        None
    }

    fn collect_module_functions(&self, module: &Module, all_functions: &mut HashMap<String, Function>) {
        if let ModuleData::Shard(shard) = &module.data {
            for func in shard.functions.values() {
                let qualified = func.qualified_name();
                all_functions.insert(qualified, func.clone());
            }

            for (child_name, child_shard) in &shard.children {
                // Create temporary Module wrapper
                let child_module = Module {
                    name: child_name.clone(),
                    path: {
                        let mut path = module.path.clone();
                        path.segments.push(PathSegment::Ident(child_name.clone()));
                        path
                    },
                    uses: vec![],
                    external_mods: vec![],
                    active_glyphs: vec![],
                    data: ModuleData::Shard(child_shard.clone()),
                };
                self.collect_module_functions(&child_module, all_functions);
            }
        }
    }

    fn collect_module_structs(&self, module: &Module, all_structs: &mut HashMap<String, StructDef>) {
        if let ModuleData::Shard(shard) = &module.data {
            for struct_def in shard.structs.values() {
                // Assuming structs are unique
                all_structs.insert(struct_def.name.clone(), struct_def.clone());
            }

            for (child_name, child_shard) in &shard.children {
                // Create temporary Module wrapper
                let child_module = Module {
                    name: child_name.clone(),
                    path: {
                        let mut path = module.path.clone();
                        path.segments.push(PathSegment::Ident(child_name.clone()));
                        path
                    },
                    uses: vec![],
                    external_mods: vec![],
                    active_glyphs: vec![],
                    data: ModuleData::Shard(child_shard.clone()),
                };
                self.collect_module_structs(&child_module, all_structs);
            }
        }
    }
}