use crate::hsir::*;
use crate::codegen_x86::X86CodeGen;
use std::fs;
use std::process::Command;

pub struct Compiler {
    pub program: Program
}

impl Compiler {
    pub fn new(program: Program) -> Self {
        Self { program }
    }

    pub fn compile_to_file(&self, output_path: &str) -> Result<(), String> {
        let mut codegen = X86CodeGen::new();
        let asm = codegen.compile_program(&self.program);

        let asm_path = format!("{}.s", output_path);
        fs::write(&asm_path, asm).map_err(|e| format!("Failed to write assembly: {}", e))?;

        println!("Generated assembly: {}", asm_path);

        let obj_path = format!("{}.o", output_path);
        let output = Command::new("as")
            .args(&[&asm_path, "-o", &obj_path])
            .output()
            .map_err(|e| format!("Failed to run assembler: {}", e))?;

        if !output.status.success() {
            return Err(format!("Assembler failed: {}", String::from_utf8_lossy(&output.stderr)));
        }

        println!("Created object file: {}", obj_path);

        let output = Command::new("ld")
            .args(&[&obj_path, "-o", output_path])
            .output()
            .map_err(|e| format!("Failed to run linker: {}", e))?;

        if !output.status.success() {
            return Err(format!("Linker failed: {}", String::from_utf8_lossy(&output.stderr)));
        }

        println!("Created executable: {}", output_path);

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
}