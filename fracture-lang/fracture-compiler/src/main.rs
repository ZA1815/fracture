use clap::Parser;
use fracture_compiler::{Compiler, CompilerMode, CompilerOptions, Target};

#[derive(Debug, Parser)]
#[command(name = "fracture-compile")]
#[command(name = "Fracture Compiler - Compiles HSIR to native code", long_about = None)]
struct Args {
    #[arg(value_name = "INPUT")]
    input: String,

    #[arg(short, long, value_name = "OUTPUT")]
    output: Option<String>,

    #[arg(short, long, value_name = "MODE", default_value = "safe")]
    mode: String,

    #[arg(short, long, value_name = "TARGET", default_value = "x86_64-linux")]
    target: String,

    #[arg(short = 'O', long, value_name = "LEVEL", default_value = "0")]
    opt_level: u8
}

fn main() {
    let args = Args::parse();

    let mode = match args.mode.as_str() {
        "safe" => CompilerMode::Safe,
        "unsafe" => CompilerMode::Unsafe,
        _ => {
            eprintln!("Invalid mode: {}. Use 'safe' or 'unsafe'", args.mode);
            std::process::exit(1);
        }
    };

    let target = match args.target.as_str() {
        "x86_64-linux" => Target::X86_64Linux,
        "x86_64-macos" => Target::X86_64MacOS,
        "x86_64-windows" => Target::X86_64Windows,
        _ => {
            eprintln!("Unsupported target: {}", args.target);
            std::process::exit(1);
        }
    };

    let output = args.output.unwrap_or_else(|| {
        let path = std::path::Path::new(&args.input);
        path.file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("output")
            .to_string()
    });

    println!("[compiler] Loading HSIR from {}", args.input);
    let program = match Compiler::load_program(&args.input) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Failed to load HSIR: {}", e);
            std::process::exit(1);
        }
    };

    let stats = program.stats();
    println!("[compiler] Loaded program: {}", stats);

    let options = CompilerOptions {
        mode,
        target,
        optimization_level: args.opt_level
    };

    let compiler = Compiler::new(options);

    match compiler.compile(&program, &output) {
        Ok(_) => println!("\n Compilation complete: {}", output),
        Err(e) => {
            eprintln!("\n Compilation failed: {}", e);
            std::process::exit(1);
        }
    }
}