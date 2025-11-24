use clap::{Parser, Subcommand};
use fracture_linter::{Linter, LinterOptions};
use fracture_compiler::{Compiler, CompilerMode, CompilerOptions, Target};
use std::process::Command as SysCommand;

#[derive(Parser, Debug)]
#[command(name = "fracture")]
#[command(version = "0.1.0")]
struct Cli {
    #[command(subcommand)]
    command: Commands
}

#[derive(Subcommand, Debug)]
enum Commands {
    Build {
        input: String,

        #[arg(short, long)]
        output: Option<String>,

        #[arg(short, long, default_value = "safe")]
        mode: String,

        #[arg(short, long)]
        config: Option<String>,

        #[arg(short, long)]
        keep: bool
    },
    Run {
        input: String,

        #[arg(short, long, default_value = "safe")]
        mode: String,

        #[arg(short, long)]
        config: Option<String>,

        #[arg(last = true)]
        args: Vec<String>
    },
    Check {
        input: String,

        #[arg(short, long)]
        config: Option<String>
    },
    Lint {
        input: String,

        #[arg(short, long)]
        output: Option<String>,

        #[arg(short, long)]
        config: Option<String>,

        #[arg(short, long)]
        text: bool
    },
    Compile {
        input: String,

        #[arg(short, long)]
        output: Option<String>,

        #[arg(short, long, default_value = "safe")]
        mode: String
    },
    New {
        name: String,

        // Just for template
        #[arg(short, long, default_value = "rust")]
        template: String
    }
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Build { input, output, mode, config, keep } => {
            cmd_build(&input, output.as_deref(), &mode, config.as_deref(), keep);
        }
        Commands::Run { input, mode, config, args } => {
            cmd_run(&input, &mode, config.as_deref(), &args);
        }
        Commands::Check { input, config } => {
            cmd_check(&input, config.as_deref());
        }
        Commands::Lint { input, output, config, text } => {
            cmd_lint(&input, output.as_deref(), config.as_deref(), text);
        }
        Commands::Compile { input, output, mode } => {
            cmd_compile(&input, output.as_deref(), &mode);
        }
        Commands::New { name, template } => {
            cmd_new(&name, &template);
        }
    }
}

fn cmd_build(input: &str, output: Option<&str>, mode: &str, config: Option<&str>, keep: bool) {
    println!("=== Building {} ===\n", input);

    let base_name = std::path::Path::new(input)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("output");

    let output_name = output.unwrap_or(base_name);
    let hsir_path = format!("{}.hsir", base_name);

    let config = Linter::load_config(config).unwrap_or_else(|e| {
        eprintln!("Error loading config: {}", e);
        std::process::exit(1);
    });

    let linter = Linter::new(LinterOptions {
        config,
        output_hsir: true,
        output_text: false
    });

    let program = linter.lint_and_output(input, base_name).unwrap_or_else(|e| {
        eprintln!("Linting failed: {}", e);
        std::process::exit(1);
    });

    // Allow for case-insensitivity later
    let compile_mode = match mode {
        "safe" => CompilerMode::Safe,
        "unsafe" => CompilerMode::Unsafe,
        _ => {
            eprintln!("Invalid mode: {}. Use 'safe' or 'unsafe'", mode);
            std::process::exit(1);
        }
    };

    // Change target later to adapt
    let compiler = Compiler::new(CompilerOptions {
        mode: compile_mode,
        target: Target::X86_64Linux,
        optimization_level: 0
    });

    compiler.compile(&program, output_name).unwrap_or_else(|e| {
        eprintln!("Compilation failed: {}", e);
        std::process::exit(1);
    });

    if !keep {
        let _ = std::fs::remove_file(&hsir_path);
        let _ = std::fs::remove_file(format!("{}.o", output_name));
        let _ = std::fs::remove_file(format!("{}.s", output_name));
    }

    println!("\n Build complete: {}", output_name);
}

fn cmd_run(input: &str, mode: &str, config: Option<&str>, args: &[String]) {
    println!("=== Running {} ===\n", input);

    let temp_exe = format!(".fracture_tmp_{}", std::process::id());
    // Allow users to keep later
    cmd_build(input, Some(&temp_exe), mode, config, false);

    println!("\n--- Output ---");
    let status = SysCommand::new(format!("./{}", temp_exe))
        .args(args)
        .status()
        .unwrap_or_else(|e| {
            eprintln!("Failed to execute: {}", e);
            std::process::exit(1);
        });
    
    let _ = std::fs::remove_file(&temp_exe);

    if !status.success() {
        std::process::exit(status.code().unwrap_or(1));
    }
}

fn cmd_check(input: &str, config: Option<&str>) {
    println!("=== Checking {} ===\n", input);

    let config = Linter::load_config(config).unwrap_or_else(|e| {
        eprintln!("Error loading config: {}", e);
        std::process::exit(1);
    });

    let linter = Linter::new(LinterOptions {
        config,
        output_hsir: false,
        output_text: false
    });

    let program = linter.lint_file(input).unwrap_or_else(|e| {
        eprintln!("Linting failed: {}", e);
        std::process::exit(1);
    });

    let compiler = Compiler::new(CompilerOptions {
        mode: CompilerMode::Safe,
        target: Target::X86_64Linux,
        optimization_level: 0
    });

    use fracture_compiler::passes;

    passes::type_check::check(&program).unwrap_or_else(|e| {
        eprintln!("Type check failed: {}", e);
        std::process::exit(1);
    });
    println!("Type check passed.");

    passes::borrow_check::check(&program).unwrap_or_else(|e| {
        eprintln!("Borrow check failed: {}", e);
        std::process::exit(1);
    });
    println!("Borrow check passed.");

    println!("\n All checks passed.");
}

fn cmd_lint(input: &str, output: Option<&str>, config: Option<&str>, text: bool) {
    let config = Linter::load_config(config).unwrap_or_else(|e| {
        eprintln!("Error loading config: {}", e);
        std::process::exit(1);
    });

    let output_path = output.unwrap_or_else(|| {
        std::path::Path::new(input)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("output")
    });

    let linter = Linter::new(LinterOptions {
        config,
        output_hsir: true,
        output_text: text
    });

    linter.lint_and_output(input, output_path).unwrap_or_else(|e| {
        eprintln!("Linting failed: {}", e);
        std::process::exit(1);
    });
}

fn cmd_compile(input: &str, output: Option<&str>, mode: &str) {
    let output_path = output.unwrap_or_else(|| {
        std::path::Path::new(input)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("output")
    });

    let compile_mode = match mode {
        "safe" => CompilerMode::Safe,
        "unsafe" => CompilerMode::Unsafe,
        _ => {
            eprintln!("Invalid mode: {}", mode);
            std::process::exit(1);
        }
    };

    let program = Compiler::load_program(input).unwrap_or_else(|e| {
        eprintln!("Failed to load HSIR: {}", e);
        std::process::exit(1);
    });

    let compiler = Compiler::new(CompilerOptions {
        mode: compile_mode,
        target: Target::X86_64Linux,
        optimization_level: 0
    });

    compiler.compile(&program, output_path).unwrap_or_else(|e| {
        eprintln!("Compilation failed: {}", e);
        std::process::exit(1);
    });
}

fn cmd_new(name: &str, template: &str) {
    println!("Creating new Fracture project: {}", name);

    std::fs::create_dir_all(name).unwrap_or_else(|e| {
        eprintln!("Failed to create directory: {}", e);
        std::process::exit(1);
    });

    let config_content = match template {
        "rust" => {
            let main_content = r#"fn main() -> i32 {
    let x = 42;
    return x;
}"#;

            std::fs::write(format!("{}/main.frac", name), main_content).unwrap();

r#"[syntax]
name = "rust"
style = "rust"

[compiler]
mode = "safe""#
        }
        "python" => {
            let main_content = r#"
def main() -> int:
    x = 42;
    return x"#;

            std::fs::write(format!("{}/main.frac", name), main_content).unwrap();

r#"[syntax]
name = "python"
style = "python"

[compiler]
mode = "unsafe""#
        }
        _ => {
            // Eventually move to using custom templates based on users config
            eprintln!("Unknown template: {}.", template);
            std::process::exit(1);
        }
    };

    std::fs::write(format!("{}/fracture.toml", name), config_content).unwrap();

    println!("Created project: {}", name);
    println!("To build:");
    println!("  cd {}", name);
    println!(" fracture build main.frac");
}