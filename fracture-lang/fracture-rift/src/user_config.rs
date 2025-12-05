use serde::{Serialize, Deserialize};
use std::path::{Path, PathBuf};
use std::io::{self, Write, BufRead};

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SyntaxPreference {
    Preset(String),
    Custom(CustomSyntax),
}

impl Default for SyntaxPreference {
    fn default() -> Self {
        SyntaxPreference::Preset("fss".to_string())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CustomSyntax {
    pub name: String,
    pub keywords: Keywords,
    pub tokens: TokenConfig,
    pub style: SyntaxStyle,
    #[serde(default)]
    pub typing: TypingRules,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Keywords {
    pub function: String,
    pub return_kw: String,
    pub if_kw: String,
    pub else_if_kw: String,
    pub else_kw: String,
    pub while_kw: String,
    pub for_kw: String,
    pub int_type: String,
    pub bool_type: String,
    pub string_type: String,
    pub let_kw: String,
    pub mut_kw: String,
    pub struct_kw: String,
    pub mod_kw: String,
    pub use_kw: String,
    pub pub_kw: String,
    pub as_kw: String,
    pub self_kw: String,
    pub super_kw: String,
    pub glyph_kw: String,
    pub shard_kw: String,
    pub some_kw: String,
    pub none_kw: String,
    pub ok_kw: String,
    pub err_kw: String,
    pub match_kw: String,
    pub panic_kw: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TokenConfig {
    pub arrow: String,
    pub double_equals: String,
    pub not_equals: String,
    pub less_equals: String,
    pub greater_equals: String,
    pub immutable_ref: String,
    pub mutable_ref: String,
    pub assignment: String,
    pub plus: String,
    pub minus: String,
    pub star: String,
    pub slash: String,
    pub less: String,
    pub greater: String,
    pub left_paren: String,
    pub right_paren: String,
    pub left_brace: String,
    pub right_brace: String,
    pub comma: String,
    pub semicolon: String,
    pub colon: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SyntaxStyle {
    pub block_style: BlockStyle,
    pub needs_semicolon: bool,
    pub type_annotations: TypeAnnotationStyle,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum BlockStyle {
    Braces,
    Indentation,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TypeAnnotationStyle {
    Colon,
    Arrow,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct TypingRules {
    #[serde(default = "default_true")]
    pub explicit_types: bool,
    #[serde(default = "default_true")]
    pub type_inference: bool,
}

fn default_true() -> bool { true }

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct UserConfig {
    #[serde(default)]
    pub syntax: SyntaxPreference,
    
    #[serde(default)]
    pub editor: EditorConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct EditorConfig {
    #[serde(default = "default_true")]
    pub show_type_hints: bool,
    #[serde(default)]
    pub show_borrow_hints: bool,
}

impl UserConfig {
    pub fn global_dir() -> Option<PathBuf> {
        dirs::home_dir().map(|h| h.join(".rift"))
    }
    
    pub fn global_path() -> Option<PathBuf> {
        Self::global_dir().map(|d| d.join("config.toml"))
    }
    
    pub fn local_path(project_root: &Path) -> PathBuf {
        project_root.join(".rift").join("config.toml")
    }

    pub fn load(project_root: Option<&Path>) -> Self {
        if let Some(root) = project_root {
            let local = Self::local_path(root);
            if local.exists() {
                if let Ok(config) = Self::load_from(&local) {
                    return config;
                }
            }
        }
        
        if let Some(global) = Self::global_path() {
            if global.exists() {
                if let Ok(config) = Self::load_from(&global) {
                    return config;
                }
            }
        }
        
        Self::default()
    }
    
    fn load_from(path: &Path) -> Result<Self, String> {
        let contents = std::fs::read_to_string(path)
            .map_err(|e| format!("Failed to read {}: {}", path.display(), e))?;

        toml::from_str(&contents)
            .map_err(|e| format!("Failed to parse {}: {}", path.display(), e))
    }
    
    pub fn save_global(&self) -> Result<(), String> {
        let dir = Self::global_dir()
            .ok_or("Could not determine home directory")?;
        
        std::fs::create_dir_all(&dir)
            .map_err(|e| format!("Failed to create ~/.rift/: {}", e))?;
        
        let path = dir.join("config.toml");
        let contents = self.to_toml_string()?;
        
        std::fs::write(&path, contents)
            .map_err(|e| format!("Failed to write config: {}", e))?;
        
        Ok(())
    }
    
    pub fn save_local(&self, project_root: &Path) -> Result<(), String> {        let dir = project_root.join(".rift");
        
        std::fs::create_dir_all(&dir)
            .map_err(|e| format!("Failed to create .rift/: {}", e))?;
        
        let path = dir.join("config.toml");
        let contents = self.to_toml_string()?;
        
        std::fs::write(&path, contents)
            .map_err(|e| format!("Failed to write config: {}", e))?;
        
        Ok(())
    }
    
    fn to_toml_string(&self) -> Result<String, String> {
        toml::to_string_pretty(self)
            .map_err(|e| format!("Failed to serialize config: {}", e))
    }
    
    pub fn global_exists() -> bool {
        Self::global_path().map(|p| p.exists()).unwrap_or(false)
    }
}

pub struct SetupWizard {
    stdin: io::Stdin,
}

impl SetupWizard {
    pub fn new() -> Self {
        Self { stdin: io::stdin() }
    }
    
    pub fn run(&self) -> Result<UserConfig, String> {
        println!("\n ══════════════════════════════════════════════════════════════ ");
        println!("            Welcome to Fracture / Rift Setup                    ");
        println!(" ══════════════════════════════════════════════════════════════ \n");
        
        println!("Fracture is syntax-agnostic - you define how YOU want to write code.");
        println!("This config will be saved to ~/.rift/config.toml\n");
        
        println!("How would you like to configure your syntax?\n");
        println!("  1. Use a preset (Rust, Python)");
        println!("  2. Customize from a preset");
        println!("  3. Define everything from scratch");
        println!("  4. Paste a TOML config\n");
        
        let syntax = loop {
            let choice = self.prompt("Choice [1-4]", "1")?;
        
            match choice.as_str() {
                "1" => break self.preset_only()?,
                "2" => break self.customize_preset()?,
                "3" => break self.full_custom()?,
                "4" => break self.paste_toml()?,
                _ => {
                    println!("Invalid choice '{}'. Please enter 1, 2, 3, or 4.", choice.trim());
                }
            };
        };
        
        let config = UserConfig {
            syntax,
            editor: EditorConfig::default(),
        };
        
        config.save_global()?;
        
        println!("\n✓ Config saved to ~/.rift/config.toml");
        println!("  You can edit it anytime using `rift syntax customize`, or run `rift setup` again.\n");
        
        Ok(config)
    }
    
    fn preset_only(&self) -> Result<SyntaxPreference, String> {
        // Add more later
        println!("\nAvailable presets:");
        println!("  Rust   - fn, return, i32, {{ }}, semicolons\n");
        println!("  Python - def, return, int, indentation, no semicolons\n");
        println!("  FSS (Fracture Standard Syntax)");
        
        let preset = self.prompt("Preset", "fss")?;
        Ok(SyntaxPreference::Preset(preset))
    }
    
    fn customize_preset(&self) -> Result<SyntaxPreference, String> {
        println!("\nWhich preset to start from?");
        let mut custom = loop {
            let base = self.prompt("Preset (Rust/Python)", "rust")?;
        
            match base.as_str() {
                "Python" => break Self::python_base(),
                "Rust" => break Self::rust_base(),
                "FSS" => break Self::fss_base(),
                _ => println!("'{}' is not a valid preset, please try again.", base.trim())
            };
        };
        
        println!("\nNow customize. Press Enter to keep default.\n");
        
        println!("── Keywords ──");
        custom.keywords.function = self.prompt("  Function keyword", &custom.keywords.function)?;
        custom.keywords.return_kw = self.prompt("  Return keyword", &custom.keywords.return_kw)?;
        custom.keywords.let_kw = self.prompt("  Variable declaration", &custom.keywords.let_kw)?;
        custom.keywords.if_kw = self.prompt("  If keyword", &custom.keywords.if_kw)?;
        custom.keywords.else_kw = self.prompt("  Else keyword", &custom.keywords.else_kw)?;
        custom.keywords.while_kw = self.prompt("  While keyword", &custom.keywords.while_kw)?;
        custom.keywords.for_kw = self.prompt("  For keyword", &custom.keywords.for_kw)?;
        
        println!("\n── Types ──");
        custom.keywords.int_type = self.prompt("  Integer type", &custom.keywords.int_type)?;
        custom.keywords.bool_type = self.prompt("  Boolean type", &custom.keywords.bool_type)?;
        custom.keywords.string_type = self.prompt("  String type", &custom.keywords.string_type)?;
        
        println!("\n── Style ──");
        let block = self.prompt("  Block style (braces/indentation)", 
            if matches!(custom.style.block_style, BlockStyle::Braces) { "braces" } else { "indentation" })?;
        custom.style.block_style = if block == "indentation" { BlockStyle::Indentation } else { BlockStyle::Braces };
        
        let semi = self.prompt("  Require semicolons? (yes/no)", 
            if custom.style.needs_semicolon { "yes" } else { "no" })?;
        custom.style.needs_semicolon = semi == "yes" || semi == "y";
        
        custom.name = self.prompt("\nName your syntax style", "custom")?;

        Self::validate_syntax(&custom)?;

        Ok(SyntaxPreference::Custom(custom))
    }
    
    fn full_custom(&self) -> Result<SyntaxPreference, String> {
        println!("\nDefine your complete syntax. Press Enter for suggested default.\n");
        
        let name = self.prompt("Style name", "my-style")?;
        
        println!("\n── Keywords ──");
        let keywords = Keywords {
            function: self.prompt("  Function", "fn")?,
            return_kw: self.prompt("  Return", "return")?,
            if_kw: self.prompt("  If", "if")?,
            else_if_kw: self.prompt("  Else-if", "else if")?,
            else_kw: self.prompt("  Else", "else")?,
            while_kw: self.prompt("  While", "while")?,
            for_kw: self.prompt("  For", "for")?,
            int_type: self.prompt("  Integer type", "i32")?,
            bool_type: self.prompt("  Boolean type", "bool")?,
            string_type: self.prompt("  String type", "String")?,
            let_kw: self.prompt("  Let/var", "let")?,
            mut_kw: self.prompt("  Mutable marker", "mut")?,
            struct_kw: self.prompt("  Struct", "struct")?,
            mod_kw: self.prompt("  Module", "mod")?,
            use_kw: self.prompt("  Import/use", "use")?,
            pub_kw: self.prompt("  Public", "pub")?,
            as_kw: self.prompt("  As (casting)", "as")?,
            self_kw: self.prompt("  Self reference", "self")?,
            super_kw: self.prompt("  Parent module", "super")?,
            glyph_kw: self.prompt("  Glyph import", "glyph")?,
            shard_kw: self.prompt("  Shard import", "shard")?,
            some_kw: self.prompt("  Some (option)", "Some")?,
            none_kw: self.prompt("  None (option)", "None")?,
            ok_kw: self.prompt("  Ok (result)", "Ok")?,
            err_kw: self.prompt("  Err (result)", "Err")?,
            match_kw: self.prompt("  Match", "match")?,
            panic_kw: self.prompt("  Panic", "panic")?,
        };
        
        println!("\n── Operators & Tokens ──");
        let tokens = TokenConfig {
            arrow: self.prompt("  Return type arrow", "->")?,
            double_equals: self.prompt("  Equality", "==")?,
            not_equals: self.prompt("  Not equals", "!=")?,
            less_equals: self.prompt("  Less or equal", "<=")?,
            greater_equals: self.prompt("  Greater or equal", ">=")?,
            immutable_ref: self.prompt("  Immutable reference", "&")?,
            mutable_ref: self.prompt("  Mutable reference", "&mut")?,
            assignment: self.prompt("  Assignment", "=")?,
            plus: self.prompt("  Plus", "+")?,
            minus: self.prompt("  Minus", "-")?,
            star: self.prompt("  Multiply", "*")?,
            slash: self.prompt("  Divide", "/")?,
            less: self.prompt("  Less than", "<")?,
            greater: self.prompt("  Greater than", ">")?,
            left_paren: self.prompt("  Left paren", "(")?,
            right_paren: self.prompt("  Right paren", ")")?,
            left_brace: self.prompt("  Left brace/block start", "{")?,
            right_brace: self.prompt("  Right brace/block end", "}")?,
            comma: self.prompt("  Comma", ",")?,
            semicolon: self.prompt("  Statement terminator", ";")?,
            colon: self.prompt("  Type annotation", ":")?,
        };
        
        println!("\n── Style ──");
        let block = self.prompt("  Block style (braces/indentation)", "braces")?;
        let semi = self.prompt("  Require semicolons? (yes/no)", "yes")?;
        let type_style = self.prompt("  Type annotation style (colon/arrow)", "colon")?;
        
        let style = SyntaxStyle {
            block_style: if block == "indentation" { BlockStyle::Indentation } else { BlockStyle::Braces },
            needs_semicolon: semi == "yes" || semi == "y",
            type_annotations: if type_style == "arrow" { TypeAnnotationStyle::Arrow } else { TypeAnnotationStyle::Colon },
        };

        let custom = CustomSyntax {
            name,
            keywords,
            tokens,
            style,
            typing: TypingRules::default(),
        };

        Self::validate_syntax(&custom)?;

        Ok(SyntaxPreference::Custom(custom))
    }
    
    fn paste_toml(&self) -> Result<SyntaxPreference, String> {
        println!("\nPaste your TOML config, then enter a blank line when done:\n");
        
        let mut lines = Vec::new();
        let stdin = io::stdin();
        
        for line in stdin.lock().lines() {
            let line = line.map_err(|e| format!("Read error: {}", e))?;
            if line.is_empty() {
                break;
            }
            lines.push(line);
        }
        
        let toml_str = lines.join("\n");
        
        let custom: CustomSyntax = toml::from_str(&toml_str)
            .map_err(|e| format!("Invalid TOML: {}", e))?;

        Self::validate_syntax(&custom)?;

        println!("\n✓ Parsed successfully: {}", custom.name);

        Ok(SyntaxPreference::Custom(custom))
    }
    
    fn prompt(&self, label: &str, default: &str) -> Result<String, String> {
        print!("{} [{}]: ", label, default);
        io::stdout().flush().map_err(|e| e.to_string())?;
        
        let mut input = String::new();
        self.stdin.read_line(&mut input)
            .map_err(|e| format!("Read error: {}", e))?;
        
        let input = input.trim();
        if input.is_empty() {
            Ok(default.to_string())
        } else {
            Ok(input.to_string())
        }
    }

    fn validate_syntax(custom: &CustomSyntax) -> Result<(), String> {
        use std::collections::HashMap;

        let mut keyword_map: HashMap<String, Vec<&str>> = HashMap::new();
        let kw = &custom.keywords;

        keyword_map.entry(kw.function.clone()).or_default().push("function");
        keyword_map.entry(kw.return_kw.clone()).or_default().push("return");
        keyword_map.entry(kw.if_kw.clone()).or_default().push("if");
        keyword_map.entry(kw.else_if_kw.clone()).or_default().push("else_if");
        keyword_map.entry(kw.else_kw.clone()).or_default().push("else");
        keyword_map.entry(kw.while_kw.clone()).or_default().push("while");
        keyword_map.entry(kw.for_kw.clone()).or_default().push("for");
        keyword_map.entry(kw.int_type.clone()).or_default().push("int_type");
        keyword_map.entry(kw.bool_type.clone()).or_default().push("bool_type");
        keyword_map.entry(kw.string_type.clone()).or_default().push("string_type");
        keyword_map.entry(kw.let_kw.clone()).or_default().push("let");
        keyword_map.entry(kw.mut_kw.clone()).or_default().push("mut");
        keyword_map.entry(kw.struct_kw.clone()).or_default().push("struct");
        keyword_map.entry(kw.mod_kw.clone()).or_default().push("mod");
        keyword_map.entry(kw.use_kw.clone()).or_default().push("use");
        keyword_map.entry(kw.pub_kw.clone()).or_default().push("pub");
        keyword_map.entry(kw.as_kw.clone()).or_default().push("as");
        keyword_map.entry(kw.self_kw.clone()).or_default().push("self");
        keyword_map.entry(kw.super_kw.clone()).or_default().push("super");
        keyword_map.entry(kw.glyph_kw.clone()).or_default().push("glyph");
        keyword_map.entry(kw.shard_kw.clone()).or_default().push("shard");
        keyword_map.entry(kw.some_kw.clone()).or_default().push("some");
        keyword_map.entry(kw.none_kw.clone()).or_default().push("none");
        keyword_map.entry(kw.ok_kw.clone()).or_default().push("ok");
        keyword_map.entry(kw.err_kw.clone()).or_default().push("err");
        keyword_map.entry(kw.match_kw.clone()).or_default().push("match");
        keyword_map.entry(kw.panic_kw.clone()).or_default().push("panic");

        for (value, fields) in &keyword_map {
            if fields.len() > 1 && !value.is_empty() {
                return Err(format!(
                    "Keyword conflict: '{}' used for multiple keywords: {}",
                    value,
                    fields.join(", ")
                ));
            }
        }

        let mut token_map: HashMap<String, Vec<&str>> = HashMap::new();
        let tok = &custom.tokens;

        token_map.entry(tok.arrow.clone()).or_default().push("arrow");
        token_map.entry(tok.double_equals.clone()).or_default().push("double_equals");
        token_map.entry(tok.not_equals.clone()).or_default().push("not_equals");
        token_map.entry(tok.less_equals.clone()).or_default().push("less_equals");
        token_map.entry(tok.greater_equals.clone()).or_default().push("greater_equals");
        token_map.entry(tok.immutable_ref.clone()).or_default().push("immutable_ref");
        token_map.entry(tok.mutable_ref.clone()).or_default().push("mutable_ref");
        token_map.entry(tok.assignment.clone()).or_default().push("assignment");
        token_map.entry(tok.plus.clone()).or_default().push("plus");
        token_map.entry(tok.minus.clone()).or_default().push("minus");
        token_map.entry(tok.star.clone()).or_default().push("star");
        token_map.entry(tok.slash.clone()).or_default().push("slash");
        token_map.entry(tok.less.clone()).or_default().push("less");
        token_map.entry(tok.greater.clone()).or_default().push("greater");
        token_map.entry(tok.left_paren.clone()).or_default().push("left_paren");
        token_map.entry(tok.right_paren.clone()).or_default().push("right_paren");
        token_map.entry(tok.left_brace.clone()).or_default().push("left_brace");
        token_map.entry(tok.right_brace.clone()).or_default().push("right_brace");
        token_map.entry(tok.comma.clone()).or_default().push("comma");
        token_map.entry(tok.semicolon.clone()).or_default().push("semicolon");
        token_map.entry(tok.colon.clone()).or_default().push("colon");

        for (value, fields) in &token_map {
            if fields.len() > 1 && !value.is_empty() {
                return Err(format!(
                    "Token conflict: '{}' used for multiple tokens: {}",
                    value,
                    fields.join(", ")
                ));
            }
        }

        Ok(())
    }

    fn rust_base() -> CustomSyntax {
        CustomSyntax {
            name: "custom-rust".to_string(),
            keywords: Keywords {
                function: "fn".to_string(),
                return_kw: "return".to_string(),
                if_kw: "if".to_string(),
                else_if_kw: "else if".to_string(),
                else_kw: "else".to_string(),
                while_kw: "while".to_string(),
                for_kw: "for".to_string(),
                int_type: "i32".to_string(),
                bool_type: "bool".to_string(),
                string_type: "String".to_string(),
                let_kw: "let".to_string(),
                mut_kw: "mut".to_string(),
                struct_kw: "struct".to_string(),
                mod_kw: "mod".to_string(),
                use_kw: "use".to_string(),
                pub_kw: "pub".to_string(),
                as_kw: "as".to_string(),
                self_kw: "self".to_string(),
                super_kw: "super".to_string(),
                glyph_kw: "glyph".to_string(),
                shard_kw: "shard".to_string(),
                some_kw: "Some".to_string(),
                none_kw: "None".to_string(),
                ok_kw: "Ok".to_string(),
                err_kw: "Err".to_string(),
                match_kw: "match".to_string(),
                panic_kw: "panic".to_string(),
            },
            tokens: TokenConfig {
                arrow: "->".to_string(),
                double_equals: "==".to_string(),
                not_equals: "!=".to_string(),
                less_equals: "<=".to_string(),
                greater_equals: ">=".to_string(),
                immutable_ref: "&".to_string(),
                mutable_ref: "&mut".to_string(),
                assignment: "=".to_string(),
                plus: "+".to_string(),
                minus: "-".to_string(),
                star: "*".to_string(),
                slash: "/".to_string(),
                less: "<".to_string(),
                greater: ">".to_string(),
                left_paren: "(".to_string(),
                right_paren: ")".to_string(),
                left_brace: "{".to_string(),
                right_brace: "}".to_string(),
                comma: ",".to_string(),
                semicolon: ";".to_string(),
                colon: ":".to_string(),
            },
            style: SyntaxStyle {
                block_style: BlockStyle::Braces,
                needs_semicolon: true,
                type_annotations: TypeAnnotationStyle::Colon,
            },
            typing: TypingRules::default(),
        }
    }
    
    fn python_base() -> CustomSyntax {
        CustomSyntax {
            name: "custom-python".to_string(),
            keywords: Keywords {
                function: "def".to_string(),
                return_kw: "return".to_string(),
                if_kw: "if".to_string(),
                else_if_kw: "elif".to_string(),
                else_kw: "else".to_string(),
                while_kw: "while".to_string(),
                for_kw: "for".to_string(),
                int_type: "int".to_string(),
                bool_type: "bool".to_string(),
                string_type: "str".to_string(),
                let_kw: "let".to_string(),  // Have to add semantics to actually allow python users to type python without being "checked"
                mut_kw: "mut".to_string(),
                struct_kw: "class".to_string(),
                mod_kw: "module".to_string(),
                use_kw: "import".to_string(),
                pub_kw: "pub".to_string(),
                as_kw: "as".to_string(),
                self_kw: "self".to_string(),
                super_kw: "super".to_string(),
                glyph_kw: "glyph".to_string(),
                shard_kw: "shard".to_string(),
                some_kw: "Some".to_string(),
                none_kw: "None".to_string(),
                ok_kw: "Ok".to_string(),
                err_kw: "Err".to_string(),
                match_kw: "match".to_string(),
                panic_kw: "panic".to_string(),
            },
            tokens: TokenConfig {
                arrow: "->".to_string(),
                double_equals: "==".to_string(),
                not_equals: "!=".to_string(),
                less_equals: "<=".to_string(),
                greater_equals: ">=".to_string(),
                immutable_ref: "&".to_string(),
                mutable_ref: "&mut".to_string(),
                assignment: "=".to_string(),
                plus: "+".to_string(),
                minus: "-".to_string(),
                star: "*".to_string(),
                slash: "/".to_string(),
                less: "<".to_string(),
                greater: ">".to_string(),
                left_paren: "(".to_string(),
                right_paren: ")".to_string(),
                left_brace: ":".to_string(),
                right_brace: "".to_string(),
                comma: ",".to_string(),
                semicolon: "".to_string(),
                colon: ":".to_string(),
            },
            style: SyntaxStyle {
                block_style: BlockStyle::Indentation,
                needs_semicolon: false,
                type_annotations: TypeAnnotationStyle::Colon,
            },
            typing: TypingRules::default(),
        }
    }

    fn fss_base() -> CustomSyntax {
        CustomSyntax {
            name: "custom-fss".to_string(),
            keywords: Keywords {
                function: "fn".to_string(),
                return_kw: "return".to_string(),
                if_kw: "if".to_string(),
                else_if_kw: "else if".to_string(),
                else_kw: "else".to_string(),
                while_kw: "while".to_string(),
                for_kw: "for".to_string(),
                int_type: "i32".to_string(),
                bool_type: "bool".to_string(),
                string_type: "String".to_string(),
                let_kw: "let".to_string(),
                mut_kw: "mut".to_string(),
                struct_kw: "struct".to_string(),
                mod_kw: "mod".to_string(),
                use_kw: "use".to_string(),
                pub_kw: "pub".to_string(),
                as_kw: "as".to_string(),
                self_kw: "self".to_string(),
                super_kw: "super".to_string(),
                glyph_kw: "glyph".to_string(),
                shard_kw: "shard".to_string(),
                some_kw: "Some".to_string(),
                none_kw: "None".to_string(),
                ok_kw: "Ok".to_string(),
                err_kw: "Err".to_string(),
                match_kw: "match".to_string(),
                panic_kw: "panic".to_string(),
            },
            tokens: TokenConfig {
                arrow: "->".to_string(),
                double_equals: "==".to_string(),
                not_equals: "!=".to_string(),
                less_equals: "<=".to_string(),
                greater_equals: ">=".to_string(),
                immutable_ref: "&".to_string(),
                mutable_ref: "&mut".to_string(),
                assignment: "=".to_string(),
                plus: "+".to_string(),
                minus: "-".to_string(),
                star: "*".to_string(),
                slash: "/".to_string(),
                less: "<".to_string(),
                greater: ">".to_string(),
                left_paren: "(".to_string(),
                right_paren: ")".to_string(),
                left_brace: "{".to_string(),
                right_brace: "}".to_string(),
                comma: ",".to_string(),
                semicolon: ";".to_string(),
                colon: ":".to_string(),
            },
            style: SyntaxStyle {
                block_style: BlockStyle::Braces,
                needs_semicolon: true,
                type_annotations: TypeAnnotationStyle::Colon,
            },
            typing: TypingRules::default(),
        }
    }
}