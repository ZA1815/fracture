use clap::{Parser, Subcommand};
use colored::*;
use fracture_rift::commands::*;

#[derive(Parser)]
#[command(name = "rift")]
#[command(version = "0.1.0")]
#[command(about = "The Fracture Package Manager", long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
    
    #[arg(short, long, global = true)]
    verbose: bool,
}

#[derive(Subcommand)]
enum Commands {
    Setup,
    
    #[command(alias = "new")]
    Init {
        name: String,
        
        #[arg(short, long)]
        style: Option<String>,
        
        #[arg(long)]
        in_place: bool,
    },
    
    #[command(alias = "b")]
    Build {
        #[arg(short, long)]
        release: bool,
        
        #[arg(short, long)]
        mode: Option<String>,
        
        #[arg(short, long)]
        target: Option<String>,
    },
    
    #[command(alias = "r")]
    Run {
        #[arg(short, long)]
        release: bool,
        
        #[arg(last = true)]
        args: Vec<String>,
    },
    
    #[command(alias = "c")]
    Check {
        #[arg(long)]
        all_warnings: bool,
    },
    
    Clean {
        #[arg(long)]
        include_lock: bool,
    },
    
    #[command(subcommand)]
    Syntax(SyntaxCommands),
    
    #[command(subcommand)]
    Config(ConfigCommands)
}

#[derive(Subcommand)]
enum SyntaxCommands {
    Export {
        #[arg(long)]
        inline: bool
    },
    
    Customize
}

#[derive(Subcommand)]
enum ConfigCommands {
    Show,
    
    Set {
        style: String,
    },
    
    Path,
    
    Local
}

fn main() {
    let cli = Cli::parse();
    
    if std::env::var("NO_COLOR").is_ok() {
        colored::control::set_override(false);
    }
    
    let result = match cli.command {
        Commands::Setup => {
            SetupCommand.execute()
        }
        
        Commands::Init { name, style, in_place } => {
            InitCommand { name, style, in_place }.execute()
        }
        
        Commands::Build { release, mode, target } => {
            BuildCommand { release, mode, target }.execute()
        }
        
        Commands::Run { release, args } => {
            RunCommand { release, args }.execute()
        }
        
        Commands::Check { all_warnings } => {
            CheckCommand { all_warnings }.execute()
        }
        
        Commands::Clean { include_lock } => {
            CleanCommand { include_lock }.execute()
        }
        
        Commands::Syntax(cmd) => match cmd {
            SyntaxCommands::Export { inline } => {
                SyntaxExportCommand { inline }.execute()
            }
            SyntaxCommands::Customize => {
                SyntaxCustomizeCommand.execute()
            }
        },
        
        Commands::Config(cmd) => match cmd {
            ConfigCommands::Show => {
                ConfigCommand { action: ConfigAction::Show }.execute()
            }
            ConfigCommands::Set { style } => {
                ConfigCommand { action: ConfigAction::SetStyle(style) }.execute()
            }
            ConfigCommands::Path => {
                ConfigCommand { action: ConfigAction::Path }.execute()
            }
            ConfigCommands::Local => {
                ConfigCommand { action: ConfigAction::Local }.execute()
            }
        },
    };
    
    if let Err(error) = result {
        eprintln!("\n{} {}", "error:".red().bold(), error);
        std::process::exit(1);
    }
}