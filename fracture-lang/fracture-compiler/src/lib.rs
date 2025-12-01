pub mod codegen_x86;
pub mod passes;

pub use fracture_ir::Program;
pub use codegen_x86::X86CodeGen;

use std::fs;
use std::process::Command;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompilerMode {
    Safe,
    Unsafe
}

#[derive(Debug, Clone)]
pub struct CompilerOptions {
    pub mode: CompilerMode,
    pub target: Target,
    pub optimization_level: u8
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            mode: CompilerMode::Safe,
            target: Target::X86_64Linux,
            optimization_level: 0
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Target {
    X86_64Linux,
    X86_64MacOS,
    X86_64Windows
    // More in future
}

pub struct Compiler {
    options: CompilerOptions
}

impl Compiler {
    pub fn new(options: CompilerOptions) -> Self {
        Self { options }
    }

    pub fn compile(&self, program: &Program, output_path: &str) -> Result<(), String> {
        println!("[compiler] Mode: {:?}, Target: {:?}", self.options.mode, self.options.target);

        let mut program = program.clone();

        if self.options.mode == CompilerMode::Safe {
            println!("[compiler] Running safety checks...");
            self.run_safe_passes(&mut program)?;
        }
        else {
            println!("[compiler] Skipping safety checks (unsafe mode)");
        }

        match self.options.target {
            Target::X86_64Linux | Target::X86_64MacOS | Target::X86_64Windows => {
                self.compile_x86(&mut program, output_path)
            }
        }
    }

    fn run_safe_passes(&self, program: &Program) -> Result<(), String> {
        let safe_funcs = program.functions.values()
            .filter(|f| !f.is_unsafe())
            .count();
        let unsafe_funcs = program.functions.values()
            .filter(|f| f.is_unsafe())
            .count();

        println!("  Checking {} safe function(s), skipping {} unsafe function(s)", safe_funcs, unsafe_funcs);

        passes::resolution_check::check(program)?;
        println!("  Resolution check passed.");

        passes::type_check::check(program)?;
        println!("  Type check passed.");

        passes::borrow_check::check(program)?;
        println!("  Borrow check passed.");

        // Add logic check later (more complicated)

        Ok(())
    }

    fn compile_x86(&self, program: &Program, output_path: &str) -> Result<(), String> {
        let mut codegen = X86CodeGen::new();
        let asm = codegen.compile_program(program);

        let asm_path = format!("{}.s", output_path);
        fs::write(&asm_path, asm).map_err(|e| format!("Failed to write assembly: {}", e))?;
        println!("[compiler] Generated assembly: {}", asm_path);

        let obj_path = format!("{}.o", output_path);
        // This command doesn't work on Windows, will have to change later
        let output = Command::new("as")
            .args(&[&asm_path, "-o", &obj_path])
            .output()
            .map_err(|e| format!("Failed to run assembler: {}", e))?;

        if !output.status.success() {
            return Err(format!("Assembler failed: {}", String::from_utf8_lossy(&output.stderr)));
        }

        println!("[compiler] Assembled: {}", obj_path);

        let output = Command::new("ld")
            .args(&[&obj_path, "-o", output_path])
            .output()
            .map_err(|e| format!("Failed to run linker: {}", e))?;

        if !output.status.success() {
            return Err(format!("Linker failed: {}", String::from_utf8_lossy(&output.stderr)));
        }

        println!("[compiler] Linked: {}", output_path);

        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = fs::metadata(output_path)
                .map_err(|e| e.to_string())?
                .permissions();
            perms.set_mode(0o755);
            fs::set_permissions(output_path, perms)
                .map_err(|e| e.to_string())?;
        }

        Ok(())
    }

    pub fn load_program(hsir_path: &str) -> Result<Program, String> {
        Program::from_file(hsir_path)
    }
}