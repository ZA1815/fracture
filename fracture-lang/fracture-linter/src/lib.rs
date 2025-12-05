pub mod projector;
pub mod lexer;
pub mod errors;

pub use fracture_ir::{Program, SyntaxConfig};
pub use projector::SyntaxProjector;
pub use lexer::Lexer;

use std::path::{Path, PathBuf};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct LinterOptions {
    pub config: SyntaxConfig,
    pub output_hsir: bool,
    pub output_text: bool,
    pub dependencies: HashMap<String, PathBuf>
}

impl Default for LinterOptions {
    fn default() -> Self {
        Self {
            // Change this to be the HSIR as users should be able to type in that if they wish
            config: SyntaxConfig::rust(),
            output_hsir: true,
            output_text: false,
            dependencies: HashMap::new()
        }
    }
}

pub struct Linter {
    options: LinterOptions
}

impl Linter {
    pub fn new(options: LinterOptions) -> Self {
        Self {
            options
        }
    }

    pub fn lint_file(&self, input_path: &str) -> Result<Program, String> {
        let source = std::fs::read_to_string(input_path)
            .map_err(|e| format!("Failed to read source file: {}", e))?;

        self.lint_source(&source, input_path)
    }

    pub fn lint_source(&self, source: &str, source_name: &str) -> Result<Program, String> {
        let mut projector = SyntaxProjector::new(
            source,
            self.options.config.clone(),
            self.options.dependencies.clone()
        );

        projector = projector.with_filename(source_name);

        match projector.project_to_hsir() {
            Ok(program) => {
                println!("[Lint] {} -> HSIR ({} functions)", source_name, program.functions.len());

                Ok(program)
            }
            Err(e) => Err(format!("Linting error in {}: {}", source_name, e)) 
        }
    }

    pub fn lint_and_output(&self, input_path: &str, output_path: &str) -> Result<Program, String> {
        let program = self.lint_file(input_path)?;

        if self.options.output_hsir {
            let hsir_path = format!("{}.hsir", output_path);
            program.to_file(&hsir_path)?;
            println!("[Lint] Wrote {}", hsir_path);
        }

        if self.options.output_text {
            let text_path = format!("{}.hsir.txt", output_path);
            let text = program.to_text();
            std::fs::write(&text_path, text)
                .map_err(|e| format!("Failed to write text: {}", e))?;
            println!("[Lint] Wrote {}", text_path);
        }

        Ok(program)
    }

    pub fn load_config(config_path: Option<&str>) -> Result<SyntaxConfig, String> {
        match config_path {
            Some(path) => {
                if Path::new(path).exists() {
                    SyntaxConfig::from_file(path)
                }
                else {
                    Err(format!("Config file not found: {}", path))
                }
            }
            None => {
                if Path::new("fracture.toml").exists() {
                    SyntaxConfig::from_file("fracture.toml")
                }
                else {
                    println!("[Lint] No config found, using Rust defaults");
                    Ok(SyntaxConfig::rust())
                }
            }
        }
    }
}