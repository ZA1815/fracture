use std::path::{Path, PathBuf};
use std::fs;
use std::collections::HashMap;
use colored::*;

use crate::manifest::Manifest;
use crate::lockfile::Lockfile;
use crate::user_config::{UserConfig, SyntaxPreference, SetupWizard};

pub type CommandResult<T> = Result<T, String>;

pub struct SetupCommand;

impl SetupCommand {
    pub fn execute(&self) -> CommandResult<()> {
        if UserConfig::global_exists() {
            println!("{} Global config already exists at ~/.rift/config.toml", "!".yellow());
            println!("  Running setup will overwrite it.\n");
            
            print!("Continue? [y/N]: ");
            std::io::Write::flush(&mut std::io::stdout()).ok();
            
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).ok();
            
            if !input.trim().eq_ignore_ascii_case("y") {
                println!("Aborted.");
                return Ok(());
            }
        }
        
        let wizard = SetupWizard::new();
        wizard.run()?;
        
        Ok(())
    }
}

pub struct InitCommand {
    pub name: String,
    
    pub style: Option<String>,
    
    pub in_place: bool,
}

impl InitCommand {
    pub fn execute(&self) -> CommandResult<()> {
        if !UserConfig::global_exists() {
            println!("{} No global config found.", "→".blue());
            println!("  Run `rift setup` to configure your syntax preferences,");
            println!("  or continue with default 'FSS (Fracture Standard Syntax)' style.\n");
        }
        
        let user_config = UserConfig::load(None);
        
        let style = self.style.clone().unwrap_or_else(|| {
            match &user_config.syntax {
                SyntaxPreference::Preset(s) => s.clone(),
                SyntaxPreference::Custom(c) => c.name.clone(),
            }
        });
        
        let project_dir = if self.in_place {
            PathBuf::from(".")
        } else {
            PathBuf::from(&self.name)
        };
        
        if project_dir.exists() && !self.in_place {
            let entries = fs::read_dir(&project_dir)
                .map_err(|e| format!("Failed to read directory: {}", e))?
                .count();
            
            if entries > 0 {
                return Err(format!(
                    "Directory '{}' already exists and is not empty. \
                     Use --force to overwrite.",
                    self.name
                ));
            }
        }
        
        let src_dir = project_dir.join("src");
        fs::create_dir_all(&src_dir)
            .map_err(|e| format!("Failed to create src directory: {}", e))?;
        
        let manifest_content = self.generate_manifest_toml(&self.name)?;
        
        fs::write(project_dir.join("rift.toml"), manifest_content)
            .map_err(|e| format!("Failed to write rift.toml: {}", e))?;
        
        let main_content = self.generate_main_file(&style);
        fs::write(src_dir.join("main.frac"), main_content)
            .map_err(|e| format!("Failed to write main.frac: {}", e))?;
        
        let gitignore = self.generate_gitignore();
        fs::write(project_dir.join(".gitignore"), gitignore)
            .map_err(|e| format!("Failed to write .gitignore: {}", e))?;
        
        println!("{} Created project `{}`", "✓".green().bold(), self.name);
        println!("  Syntax style: {}", style.cyan());
        println!();
        println!("To get started:");
        
        if !self.in_place {
            println!("  {} {}", "cd".cyan(), self.name);
        }
        
        println!("  {} build", "rift".cyan());
        println!("  {} run", "rift".cyan());
        
        Ok(())
    }
    
    fn generate_manifest_toml(&self, name: &str) -> CommandResult<String> {
        let toml = format!(
            // Might decide to remove docs thing later if I don't have the website
            // Add later: # See https://fracture-lang.org/docs/manifest for all options
r#"# Fracture Project Manifest
[package]
name = "{name}"
version = "0.1.0"
# authors = ["Your Name <you@example.com>"]
# description = "A short description of your project"
# license = "MIT"

[dependencies]
std = {{ git = "https://github.com/fracture-lang/fracture-stdlib" }}
# some-lib = "1.0"
# another-lib = {{ git = "https://github.com/user/repo" }}

# ─────────────────────────────────────────────────────────────────────────────
# SYNTAX NOTE:
# Your personal syntax preferences live in ~/.rift/config.toml (gitignored).
# This keeps your style personal until you choose to share it.
#
# To share your syntax style with collaborators:
#   rift syntax export
#
# This adds a [syntax] section here that others can use.
# ─────────────────────────────────────────────────────────────────────────────
"#,
            name = name,
        );
        
        Ok(toml)
    }
    
    // Generate this dynamically based on config
    fn generate_main_file(&self, style: &str) -> String {
        match style {
            "python" => {
                r#"import shard std::io

def main() -> int:
    message = "Hello, Fracture!"
    io::println(message)
    return 0
"#.to_string()
            }
            "rust" | _ => {
                r#"use shard std::io;

fn main() -> i32 {
    let message = "Hello, Fracture!";
    io::println(message);
    return 0;
}
"#.to_string()
            }
        }
    }
    
    fn generate_gitignore(&self) -> String {
        r#"# Fracture build artifacts
/target/
*.o
*.s
*.hsir

# Rift local cache
/.rift/

# Editor files
.vscode/
.idea/
*.swp
*~

# OS files
.DS_Store
Thumbs.db
"#.to_string()
    }
}

pub struct BuildCommand {
    pub release: bool,
    
    pub mode: Option<String>,
    
    pub target: Option<String>,
}

impl BuildCommand {
    pub fn execute(&self) -> CommandResult<()> {
        let project_root = self.find_project_root()?;
        let manifest_path = project_root.join("rift.toml");
        
        println!("{} Loading manifest...", "→".blue());
        
        let manifest = Manifest::from_file(&manifest_path)?;
        
        println!(
            "{} Building {} v{}",
            "→".blue(),
            manifest.package.name.cyan().bold(),
            manifest.package.version
        );
        
        let lockfile_path = project_root.join("rift.lock");
        let mut lockfile = Lockfile::load(&lockfile_path)?;
        
        if lockfile.is_none() && !manifest.dependencies.is_empty() {
            println!("{} No lockfile found, resolving dependencies...", "→".blue());
            
            let resolver = crate::resolver::Resolver::new();
            let graph = resolver.resolve(&manifest, &project_root)?;
            let new_lockfile = graph.to_lockfile();
            new_lockfile.save(&lockfile_path)?;

            println!("{} Generated rift.lock", "✓".green());

            lockfile = Some(new_lockfile);
        }

        if !manifest.dependencies.is_empty() && lockfile.is_none() {
            return Err("Failed to load or generate lockfile".to_string());
        }
        
        let entry_point = manifest.entry_point(&project_root)
            .ok_or_else(|| {
                "No entry point found. Create src/main.frac or specify 'entry' in rift.toml"
                    .to_string()
            })?;
        
        println!(
            "{} Compiling {}",
            "→".blue(),
            entry_point.display()
        );
        
        let compile_mode = self.mode.as_deref()
            .or(Some(&manifest.compiler.mode))
            .unwrap_or("safe");
        
        let target_dir = project_root.join("target");
        let mode_dir = if self.release { "release" } else { "debug" };
        let output_dir = target_dir.join(mode_dir);
        
        fs::create_dir_all(&output_dir)
            .map_err(|e| format!("Failed to create target directory: {}", e))?;
        
        let output_path = output_dir.join(&manifest.package.name);
        
        let syntax_config = self.load_syntax_config(&manifest, &project_root)?;

        let mut dependencies = HashMap::new();

        for (name, spec) in &manifest.dependencies {
            if let crate::manifest::DependencySpec::Detailed(d) = spec {
                if let Some(p) = &d.path {
                    let dep_path = project_root.join(p).canonicalize()
                        .map_err(|e| format!("Failed to resolve dependency path: {}", e))?;

                    dependencies.insert(name.clone(), dep_path);
                }
            }
        }
        
        self.compile(
            &entry_point,
            &output_path,
            compile_mode,
            syntax_config,
            dependencies
        )?;
        
        println!(
            "\n{} Built {} -> {}",
            "✓".green().bold(),
            manifest.package.name.cyan(),
            output_path.display()
        );
        
        Ok(())
    }
    
    fn find_project_root(&self) -> CommandResult<PathBuf> {
        let mut current = std::env::current_dir()
            .map_err(|e| format!("Failed to get current directory: {}", e))?;
        
        loop {
            if current.join("rift.toml").exists() {
                return Ok(current);
            }
            
            if !current.pop() {
                return Err(
                    "Could not find rift.toml. Are you in a Fracture project?"
                        .to_string()
                );
            }
        }
    }
    
    fn load_syntax_config(
        &self, 
        manifest: &Manifest,
        project_root: &Path
    ) -> CommandResult<fracture_ir::SyntaxConfig> {
        if manifest.syntax.source_style.is_some() || manifest.syntax.source_config.is_some() {
            if let Some(config_file) = &manifest.syntax.source_config {
                let config_path = project_root.join(config_file);
                return fracture_ir::SyntaxConfig::from_file(
                    config_path.to_str().unwrap()
                );
            }
            
            if let Some(style) = &manifest.syntax.source_style {
                return self.style_to_config(style);
            }
        }
        
        let user_config = UserConfig::load(Some(project_root));
        
        match &user_config.syntax {
            SyntaxPreference::Preset(style) => self.style_to_config(style),
            SyntaxPreference::Custom(custom) => {
                Self::custom_to_syntax_config(custom)
            }
        }
    }

    fn custom_to_syntax_config(custom: &crate::user_config::CustomSyntax) -> CommandResult<fracture_ir::SyntaxConfig> {
        let config = fracture_ir::SyntaxConfig {
            name: custom.name.clone(),
            keywords: fracture_ir::syntax_config::Keywords {
                function_kw: custom.keywords.function.clone(),
                return_kw: custom.keywords.return_kw.clone(),
                if_kw: custom.keywords.if_kw.clone(),
                else_if_kw: custom.keywords.else_if_kw.clone(),
                else_kw: custom.keywords.else_kw.clone(),
                while_kw: custom.keywords.while_kw.clone(),
                for_kw: custom.keywords.for_kw.clone(),
                int_type: custom.keywords.int_type.clone(),
                bool_type: custom.keywords.bool_type.clone(),
                string_type: custom.keywords.string_type.clone(),
                let_kw: custom.keywords.let_kw.clone(),
                mut_kw: custom.keywords.mut_kw.clone(),
                struct_kw: custom.keywords.struct_kw.clone(),
                mod_kw: custom.keywords.mod_kw.clone(),
                use_kw: custom.keywords.use_kw.clone(),
                pub_kw: custom.keywords.pub_kw.clone(),
                as_kw: custom.keywords.as_kw.clone(),
                self_kw: custom.keywords.self_kw.clone(),
                super_kw: custom.keywords.super_kw.clone(),
                glyph_kw: custom.keywords.glyph_kw.clone(),
                shard_kw: custom.keywords.shard_kw.clone(),
                some_kw: custom.keywords.some_kw.clone(),
                none_kw: custom.keywords.none_kw.clone(),
                ok_kw: custom.keywords.ok_kw.clone(),
                err_kw: custom.keywords.err_kw.clone(),
                match_kw: custom.keywords.match_kw.clone(),
                panic_kw: custom.keywords.panic_kw.clone(),
            },
            tokens: fracture_ir::syntax_config::TokenConfig {
                arrow: custom.tokens.arrow.clone(),
                double_equals: custom.tokens.double_equals.clone(),
                not_equals: custom.tokens.not_equals.clone(),
                less_equals: custom.tokens.less_equals.clone(),
                greater_equals: custom.tokens.greater_equals.clone(),
                immutable_ref: custom.tokens.immutable_ref.clone(),
                mutable_ref: custom.tokens.mutable_ref.clone(),
                assignment: custom.tokens.assignment.clone(),
                plus: custom.tokens.plus.clone(),
                minus: custom.tokens.minus.clone(),
                star: custom.tokens.star.clone(),
                slash: custom.tokens.slash.clone(),
                less: custom.tokens.less.clone(),
                greater: custom.tokens.greater.clone(),
                left_paren: custom.tokens.left_paren.clone(),
                right_paren: custom.tokens.right_paren.clone(),
                left_brace: custom.tokens.left_brace.clone(),
                right_brace: custom.tokens.right_brace.clone(),
                comma: custom.tokens.comma.clone(),
                semicolon: custom.tokens.semicolon.clone(),
                colon: custom.tokens.colon.clone(),
            },
            style: fracture_ir::syntax_config::SyntaxStyle {
                block_style: match custom.style.block_style {
                    crate::user_config::BlockStyle::Braces => fracture_ir::syntax_config::BlockStyle::Braces,
                    crate::user_config::BlockStyle::Indentation => fracture_ir::syntax_config::BlockStyle::Indentation,
                },
                needs_semicolon: custom.style.needs_semicolon,
                type_annotations: match custom.style.type_annotations {
                    crate::user_config::TypeAnnotationStyle::Colon => fracture_ir::syntax_config::TypeAnnotationStyle::Colon,
                    crate::user_config::TypeAnnotationStyle::Arrow => fracture_ir::syntax_config::TypeAnnotationStyle::CStyle,
                },
            },
            typing: fracture_ir::syntax_config::TypingRules {
                explicit_types: custom.typing.explicit_types,
                type_inference: custom.typing.type_inference,
            },
        };

        config.validate().map_err(|e| e)?;

        Ok(config)
    }
    
    fn style_to_config(&self, style: &str) -> CommandResult<fracture_ir::SyntaxConfig> {
        match style {
            "rust" | "custom-rust" => Ok(fracture_ir::SyntaxConfig::rust()),
            "python" | "custom-python" => Ok(fracture_ir::SyntaxConfig::python()),
            "fss" => Ok(fracture_ir::SyntaxConfig::fss()),
            other => Err(format!(
                "Unknown syntax style: '{}'. Run `rift setup` to configure.",
                other
            )),
        }
    }
    
    fn compile(
        &self,
        input: &Path,
        output: &Path,
        mode: &str,
        syntax_config: fracture_ir::SyntaxConfig,
        dependencies: std::collections::HashMap<String, PathBuf>
    ) -> CommandResult<()> {
        use fracture_linter::{Linter, LinterOptions};
        use fracture_compiler::{Compiler, CompilerMode, CompilerOptions, Target};
        
        let linter = Linter::new(LinterOptions {
            config: syntax_config,
            output_hsir: false,
            output_text: false,
            dependencies
        });
        
        let input_str = input.to_str()
            .ok_or_else(|| "Invalid input path".to_string())?;
        
        let program = linter.lint_file(input_str)?;
        
        let compile_mode = match mode {
            "safe" => CompilerMode::Safe,
            "unsafe" => CompilerMode::Unsafe,
            _ => return Err(format!("Invalid compiler mode: {}", mode)),
        };
        
        let output_str = output.to_str()
            .ok_or_else(|| "Invalid output path".to_string())?;
        
        let compiler = Compiler::new(CompilerOptions {
            mode: compile_mode,
            target: Target::X86_64Linux,  // Detect from manifest or system later
            optimization_level: if self.release { 2 } else { 0 },
        });
        
        compiler.compile(&program, output_str)?;
        
        let _ = std::fs::remove_file(format!("{}.s", output_str));
        let _ = std::fs::remove_file(format!("{}.o", output_str));
        
        Ok(())
    }
}

pub struct RunCommand {
    pub args: Vec<String>,
    
    pub release: bool,
}

impl RunCommand {
    pub fn execute(&self) -> CommandResult<()> {
        let build = BuildCommand {
            release: self.release,
            mode: None,
            target: None,
        };
        
        build.execute()?;
        
        let project_root = build.find_project_root()?;
        let manifest = Manifest::from_file(&project_root.join("rift.toml"))?;
        
        let mode_dir = if self.release { "release" } else { "debug" };
        let executable = project_root
            .join("target")
            .join(mode_dir)
            .join(&manifest.package.name);
        
        if !executable.exists() {
            return Err(format!(
                "Executable not found at {}",
                executable.display()
            ));
        }
        
        println!("\n{} Running {}\n", "→".blue(), manifest.package.name.cyan());
        println!("{}", "─".repeat(40).dimmed());
        
        let status = std::process::Command::new(&executable)
            .args(&self.args)
            .status()
            .map_err(|e| format!("Failed to run executable: {}", e))?;
        
        println!("{}", "─".repeat(40).dimmed());
        
        if status.success() {
            println!("\n{} Process exited successfully", "✓".green());
        } else {
            let code = status.code().unwrap_or(-1);
            println!("\n{} Process exited with code {}", "✗".red(), code);
        }
        
        Ok(())
    }
}

pub struct CheckCommand {
    pub all_warnings: bool
}

impl CheckCommand {
    pub fn execute(&self) -> CommandResult<()> {
        let project_root = BuildCommand {
            release: false,
            mode: None,
            target: None,
        }.find_project_root()?;
        
        let manifest = Manifest::from_file(&project_root.join("rift.toml"))?;
        
        println!(
            "{} Checking {} v{}",
            "→".blue(),
            manifest.package.name.cyan().bold(),
            manifest.package.version
        );
        
        let entry_point = manifest.entry_point(&project_root)
            .ok_or_else(|| "No entry point found".to_string())?;
        
        let syntax_config = self.load_syntax_config(&manifest, &project_root)?;

        let mut dependencies = HashMap::new();

        for (name, spec) in &manifest.dependencies {
            if let crate::manifest::DependencySpec::Detailed(d) = spec {
                if let Some(p) = &d.path {
                    let dep_path = project_root.join(p).canonicalize()
                        .map_err(|e| format!("Failed to resolve dependency path: {}", e))?;

                    dependencies.insert(name.clone(), dep_path);
                }
            }
        }
        
        use fracture_linter::{Linter, LinterOptions};
        
        let linter = Linter::new(LinterOptions {
            config: syntax_config,
            output_hsir: false,
            output_text: false,
            dependencies
        });
        
        let program = linter.lint_file(
            entry_point.to_str().ok_or("Invalid path")?
        )?;
        
        use fracture_compiler::passes;
        
        passes::type_check::check(&program)?;
        println!("  {} Type check", "✓".green());
        
        passes::borrow_check::check(&program)?;
        println!("  {} Borrow check", "✓".green());
        
        println!(
            "\n{} {} passed all checks",
            "✓".green().bold(),
            manifest.package.name.cyan()
        );
        
        Ok(())
    }
    
    fn load_syntax_config(
        &self,
        manifest: &Manifest,
        project_root: &Path
    ) -> CommandResult<fracture_ir::SyntaxConfig> {
        if manifest.syntax.source_style.is_some() || manifest.syntax.source_config.is_some() {
            if let Some(config_file) = &manifest.syntax.source_config {
                let config_path = project_root.join(config_file);
                return fracture_ir::SyntaxConfig::from_file(
                    config_path.to_str().unwrap()
                );
            }
            
            if let Some(style) = &manifest.syntax.source_style {
                return match style.as_str() {
                    "rust" | "custom-rust" => Ok(fracture_ir::SyntaxConfig::rust()),
                    "python" | "custom-python" => Ok(fracture_ir::SyntaxConfig::python()),
                    "fss" => Ok(fracture_ir::SyntaxConfig::fss()),
                    other => Err(format!("Unknown style: {}", other)),
                };
            }
        }
        
        let user_config = UserConfig::load(Some(project_root));
        
        match &user_config.syntax {
            SyntaxPreference::Preset(style) => match style.as_str() {
                "rust" => Ok(fracture_ir::SyntaxConfig::rust()),
                "python" => Ok(fracture_ir::SyntaxConfig::python()),
                "fss" => Ok(fracture_ir::SyntaxConfig::fss()),
                other => Err(format!("Unknown style: {}", other)),
            },
            SyntaxPreference::Custom(custom) => {
                BuildCommand::custom_to_syntax_config(custom)
            }
        }
    }
}

pub struct CleanCommand {
    pub include_lock: bool
}

impl CleanCommand {
    pub fn execute(&self) -> CommandResult<()> {
        let project_root = BuildCommand {
            release: false,
            mode: None,
            target: None,
        }.find_project_root()?;
        
        let target_dir = project_root.join("target");
        
        if target_dir.exists() {
            fs::remove_dir_all(&target_dir)
                .map_err(|e| format!("Failed to remove target/: {}", e))?;
            println!("{} Removed target/", "✓".green());
        } else {
            println!("{} target/ does not exist", "→".blue());
        }
        
        if self.include_lock {
            let lockfile = project_root.join("rift.lock");
            if lockfile.exists() {
                fs::remove_file(&lockfile)
                    .map_err(|e| format!("Failed to remove rift.lock: {}", e))?;
                println!("{} Removed rift.lock", "✓".green());
            }
        }
        
        Ok(())
    }
}

pub struct SyntaxExportCommand {
    pub inline: bool
}

impl SyntaxExportCommand {
    pub fn execute(&self) -> CommandResult<()> {
        let project_root = BuildCommand {
            release: false,
            mode: None,
            target: None,
        }.find_project_root()?;
        
        let manifest_path = project_root.join("rift.toml");
        let mut manifest_content = fs::read_to_string(&manifest_path)
            .map_err(|e| format!("Failed to read rift.toml: {}", e))?;
        
        let user_config = UserConfig::load(Some(&project_root));
        
        if manifest_content.contains("[syntax]") {
            return Err(
                "[syntax] section already exists in rift.toml.\nRemove it first if you want to re-export.".to_string()
            );
        }
        
        let (syntax_section, style_name) = match &user_config.syntax {
            SyntaxPreference::Preset(name) => {
                let section = format!(
                    "\n[syntax]\n
                     # Exported from config - shared with collaborators\n
                     source_style = \"{}\"\n",
                    name
                );
                (section, name.clone())
            }
            SyntaxPreference::Custom(custom) => {
                if self.inline {
                    let syntax_file = project_root.join("fracture-syntax.toml");
                    let syntax_toml = toml::to_string_pretty(custom)
                        .map_err(|e| format!("Failed to serialize syntax: {}", e))?;
                    
                    fs::write(&syntax_file, &syntax_toml)
                        .map_err(|e| format!("Failed to write syntax file: {}", e))?;
                    
                    let section = format!(
                        "\n[syntax]\n\
                         # Custom syntax exported from config\n\
                         source_config = \"fracture-syntax.toml\"\n"
                    );
                    (section, custom.name.clone())
                }
                else {
                    let section = format!(
                        "\n[syntax]\n\
                         # Custom syntax '{}' - collaborators need matching config\n\
                         source_style = \"{}\"\n",
                        custom.name, custom.name
                    );
                    (section, custom.name.clone())
                }
            }
        };
        
        manifest_content.push_str(&syntax_section);
        
        fs::write(&manifest_path, manifest_content)
            .map_err(|e| format!("Failed to write rift.toml: {}", e))?;
        
        println!("{} Exported syntax to rift.toml", "✓".green().bold());
        println!("  Style: {}", style_name.cyan());
        println!();
        println!("Collaborators will now use this syntax when building.");
        
        Ok(())
    }
}

pub struct SyntaxCustomizeCommand;

impl SyntaxCustomizeCommand {
    pub fn execute(&self) -> CommandResult<()> {
        use std::io::Write;
        
        let config = UserConfig::load(None);
        let editor_config = config.editor.clone();
        
        let mut custom = match config.syntax {
            SyntaxPreference::Custom(c) => c,
            SyntaxPreference::Preset(ref name) => {
                match name.as_str() {
                    "Python" => Self::python_base(),
                    _ => Self::rust_base(),
                }
            }
        };
        
        println!("\n{}", "═".repeat(60).dimmed());
        println!("{}", "  Syntax Customizer".cyan().bold());
        println!("{}", "═".repeat(60).dimmed());
        println!("\nCurrent style: {}\n", custom.name.cyan());
        
        loop {
            let items = Self::build_menu(&custom);
            
            println!("{}", "Select a setting to change (or 'q' to save & quit):".dimmed());
            println!();
            
            for (i, (category, name, value)) in items.iter().enumerate() {
                if i == 0 || items.get(i.saturating_sub(1)).map(|(c, _, _)| c) != Some(category) {
                    println!("  {}", format!("── {} ──", category).dimmed());
                }
                println!("  {:>2}. {} [{}]", i + 1, name, value.cyan());
            }
            
            println!();
            print!("Choice: ");
            std::io::stdout().flush().ok();
            
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).ok();
            let input = input.trim();
            
            if input == "q" || input == "quit" || input.is_empty() {
                break;
            }
            
            let choice: usize = match input.parse() {
                Ok(n) if n >= 1 && n <= items.len() => n,
                _ => {
                    println!("{} Invalid choice\n", "!".yellow());
                    continue;
                }
            };
            
            let (category, name, current) = &items[choice - 1];
            
            print!("New value for {} [{}]: ", name, current);
            std::io::stdout().flush().ok();
            
            let mut new_value = String::new();
            std::io::stdin().read_line(&mut new_value).ok();
            let new_value = new_value.trim();
            
            if new_value.is_empty() {
                println!("{} Keeping current value\n", "→".blue());
                continue;
            }
            
            Self::apply_change(&mut custom, choice - 1, new_value);
            println!("{} Updated {} = \"{}\"\n", "✓".green(), name, new_value);
        }
        
        if custom.name == "custom-rust" || custom.name == "custom-python" || custom.name == "rust" || custom.name == "python" {
            print!("\nName your custom style [{}]: ", custom.name);
            std::io::stdout().flush().ok();
            
            let mut name = String::new();
            std::io::stdin().read_line(&mut name).ok();
            let name = name.trim();
            
            if !name.is_empty() {
                custom.name = name.to_string();
            }
        }
        
        let new_config = UserConfig {
            syntax: SyntaxPreference::Custom(custom.clone()),
            editor: editor_config,
        };
        
        new_config.save_global()?;
        
        println!("\n{} Saved to ~/.rift/config.toml", "✓".green().bold());
        println!("  Style: {}", custom.name.cyan());
        
        Ok(())
    }
    
    fn build_menu(custom: &crate::user_config::CustomSyntax) -> Vec<(&'static str, &'static str, String)> {
        vec![
            ("Keywords", "function", custom.keywords.function.clone()),
            ("Keywords", "return", custom.keywords.return_kw.clone()),
            ("Keywords", "if", custom.keywords.if_kw.clone()),
            ("Keywords", "else if", custom.keywords.else_if_kw.clone()),
            ("Keywords", "else", custom.keywords.else_kw.clone()),
            ("Keywords", "while", custom.keywords.while_kw.clone()),
            ("Keywords", "for", custom.keywords.for_kw.clone()),
            ("Keywords", "let/var", custom.keywords.let_kw.clone()),
            ("Keywords", "mutable", custom.keywords.mut_kw.clone()),
            ("Keywords", "struct", custom.keywords.struct_kw.clone()),
            ("Keywords", "module", custom.keywords.mod_kw.clone()),
            ("Keywords", "import/use", custom.keywords.use_kw.clone()),
            ("Keywords", "public", custom.keywords.pub_kw.clone()),
            ("Keywords", "as", custom.keywords.as_kw.clone()),
            ("Keywords", "self", custom.keywords.self_kw.clone()),
            ("Keywords", "super", custom.keywords.super_kw.clone()),
            
            ("Types", "integer", custom.keywords.int_type.clone()),
            ("Types", "boolean", custom.keywords.bool_type.clone()),
            ("Types", "string", custom.keywords.string_type.clone()),
            
            ("Tokens", "arrow (->)", custom.tokens.arrow.clone()),
            ("Tokens", "equals (==)", custom.tokens.double_equals.clone()),
            ("Tokens", "not equals (!=)", custom.tokens.not_equals.clone()),
            ("Tokens", "less/equal (<=)", custom.tokens.less_equals.clone()),
            ("Tokens", "greater/equal (>=)", custom.tokens.greater_equals.clone()),
            ("Tokens", "immutable ref", custom.tokens.immutable_ref.clone()),
            ("Tokens", "mutable ref", custom.tokens.mutable_ref.clone()),
            ("Tokens", "assignment", custom.tokens.assignment.clone()),
            ("Tokens", "block start", custom.tokens.left_brace.clone()),
            ("Tokens", "block end", custom.tokens.right_brace.clone()),
            ("Tokens", "semicolon", custom.tokens.semicolon.clone()),
            ("Tokens", "type annotation", custom.tokens.colon.clone()),
            
            ("Style", "block style", match custom.style.block_style {
                crate::user_config::BlockStyle::Braces => "braces".to_string(),
                crate::user_config::BlockStyle::Indentation => "indentation".to_string(),
            }),
            ("Style", "needs semicolon", if custom.style.needs_semicolon { "yes" } else { "no" }.to_string()),
        ]
    }
    
    fn apply_change(custom: &mut crate::user_config::CustomSyntax, index: usize, value: &str) {
        match index {
            0 => custom.keywords.function = value.to_string(),
            1 => custom.keywords.return_kw = value.to_string(),
            2 => custom.keywords.if_kw = value.to_string(),
            3 => custom.keywords.else_if_kw = value.to_string(),
            4 => custom.keywords.else_kw = value.to_string(),
            5 => custom.keywords.while_kw = value.to_string(),
            6 => custom.keywords.for_kw = value.to_string(),
            7 => custom.keywords.let_kw = value.to_string(),
            8 => custom.keywords.mut_kw = value.to_string(),
            9 => custom.keywords.struct_kw = value.to_string(),
            10 => custom.keywords.mod_kw = value.to_string(),
            11 => custom.keywords.use_kw = value.to_string(),
            12 => custom.keywords.pub_kw = value.to_string(),
            13 => custom.keywords.as_kw = value.to_string(),
            14 => custom.keywords.self_kw = value.to_string(),
            15 => custom.keywords.super_kw = value.to_string(),
            
            16 => custom.keywords.int_type = value.to_string(),
            17 => custom.keywords.bool_type = value.to_string(),
            18 => custom.keywords.string_type = value.to_string(),
            
            19 => custom.tokens.arrow = value.to_string(),
            20 => custom.tokens.double_equals = value.to_string(),
            21 => custom.tokens.not_equals = value.to_string(),
            22 => custom.tokens.less_equals = value.to_string(),
            23 => custom.tokens.greater_equals = value.to_string(),
            24 => custom.tokens.immutable_ref = value.to_string(),
            25 => custom.tokens.mutable_ref = value.to_string(),
            26 => custom.tokens.assignment = value.to_string(),
            27 => custom.tokens.left_brace = value.to_string(),
            28 => custom.tokens.right_brace = value.to_string(),
            29 => custom.tokens.semicolon = value.to_string(),
            30 => custom.tokens.colon = value.to_string(),
            
            31 => custom.style.block_style = if value == "indentation" {
                crate::user_config::BlockStyle::Indentation
            } else {
                crate::user_config::BlockStyle::Braces
            },
            32 => custom.style.needs_semicolon = value == "yes" || value == "y" || value == "true",
            _ => {}
        }
    }
    
    fn rust_base() -> crate::user_config::CustomSyntax {
        crate::user_config::CustomSyntax {
            name: "custom-rust".to_string(),
            keywords: crate::user_config::Keywords {
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
            tokens: crate::user_config::TokenConfig {
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
            style: crate::user_config::SyntaxStyle {
                block_style: crate::user_config::BlockStyle::Braces,
                needs_semicolon: true,
                type_annotations: crate::user_config::TypeAnnotationStyle::Colon,
            },
            typing: crate::user_config::TypingRules::default(),
        }
    }
    
    fn python_base() -> crate::user_config::CustomSyntax {
        crate::user_config::CustomSyntax {
            name: "custom-python".to_string(),
            keywords: crate::user_config::Keywords {
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
                let_kw: "let".to_string(),
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
            tokens: crate::user_config::TokenConfig {
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
            style: crate::user_config::SyntaxStyle {
                block_style: crate::user_config::BlockStyle::Indentation,
                needs_semicolon: false,
                type_annotations: crate::user_config::TypeAnnotationStyle::Colon,
            },
            typing: crate::user_config::TypingRules::default(),
        }
    }
}

pub struct ConfigCommand {
    pub action: ConfigAction,
}

pub enum ConfigAction {
    Show,
    SetStyle(String),
    Path,
    Local
}

impl ConfigCommand {
    pub fn execute(&self) -> CommandResult<()> {
        match &self.action {
            ConfigAction::Show => {
                let global_path = UserConfig::global_path()
                    .map(|p| p.display().to_string())
                    .unwrap_or_else(|| "(not found)".to_string());
                
                let config = UserConfig::load(None);
                
                println!("{} {}", "Global config:".dimmed(), global_path);
                println!();
                
                match &config.syntax {
                    SyntaxPreference::Preset(name) => {
                        println!("syntax = \"{}\"", name.cyan());
                    }
                    SyntaxPreference::Custom(custom) => {
                        println!("[syntax]");
                        println!("name = \"{}\"", custom.name.cyan());
                        println!("# (custom syntax definition)");
                    }
                }
                
                Ok(())
            }
            
            ConfigAction::SetStyle(style) => {
                let mut config = UserConfig::load(None);
                config.syntax = SyntaxPreference::Preset(style.clone());
                config.save_global()?;
                
                println!("{} Set syntax = \"{}\"", "✓".green(), style.cyan());
                
                Ok(())
            }
            
            ConfigAction::Path => {
                if let Some(path) = UserConfig::global_path() {
                    println!("Global: {}", path.display());
                }
                
                if let Ok(cwd) = std::env::current_dir() {
                    let local = UserConfig::local_path(&cwd);
                    println!("Local:  {} {}", 
                        local.display(),
                        if local.exists() { "(exists)".green() } else { "(not set)".dimmed() }
                    );
                }
                
                Ok(())
            }
            
            ConfigAction::Local => {
                let project_root = BuildCommand {
                    release: false,
                    mode: None,
                    target: None,
                }.find_project_root()?;
                
                let config = UserConfig::load(None);
                config.save_local(&project_root)?;
                
                let local_path = UserConfig::local_path(&project_root);
                println!("{} Created {}", "✓".green(), local_path.display());
                println!("  This overrides your global config for this project only.");
                
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    
    #[test]
    fn test_generate_main_rust_style() {
        let cmd = InitCommand {
            name: "test".to_string(),
            style: Some("rust".to_string()),
            in_place: false,
        };
        
        let main = cmd.generate_main_file("rust");
        assert!(main.contains("fn main()"));
        assert!(main.contains("println"));
    }
    
    #[test]
    fn test_generate_main_python_style() {
        let cmd = InitCommand {
            name: "test".to_string(),
            style: Some("python".to_string()),
            in_place: false,
        };
        
        let main = cmd.generate_main_file("python");
        assert!(main.contains("def main()"));
    }
}