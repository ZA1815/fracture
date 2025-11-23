use clap::Parser;
use fracture_linter::{Linter, LinterOptions, SyntaxConfig};

#[derive(Debug, Parser)]
#[command(name = "fracture-lint")]
#[command(about = "Fracture Linter - Projects source syntax to HSIR", long_about = None)]
struct Args {
    #[arg(value_name = "INPUT")]
    input: String,

    #[arg(short, long, value_name = "OUTPUT")]
    output: Option<String>,

    #[arg(short, long, value_name = "CONFIG")]
    config: Option<String>,

    #[arg(short = 't', long)]
    text: bool,

    #[arg(short, long, value_name = "STYLE")]
    style: Option<String>
}

fn main() {
    let args = Args::parse();

    let mut config = match Linter::load_config(args.config.as_deref()) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error loading config: {}", e);
            std::process::exit(1);
        }
    };

    if let Some(style) = args.style {
        config = match style.as_str() {
            // Add more styles later
            "rust" => SyntaxConfig::rust(),
            "python" => SyntaxConfig::python(),
            _ => {
                eprintln!("Unknown style: {}.", style);
                std::process::exit(1);
            }
        };
        println!("[lint] Using {} syntax style", style);
    }

    let output = args.output.unwrap_or_else(|| {
        let path = std::path::Path::new(&args.input);
        path.file_stem().and_then(|s| s.to_str()).unwrap_or("output").to_string()
    });

    let options = LinterOptions {
        config,
        output_hsir: true,
        output_text: args.text
    };

    let linter = Linter::new(options);

    match linter.lint_and_output(&args.input, &output) {
        Ok(program) => {
            let stats = program.stats();
            println!("\n Linting complete: {}", stats);
        }
        Err(e) => {
            eprintln!("\n Linting failed: {}", e);
            std::process::exit(1);
        }
    }
}