# Fracture

> **A syntax-agnostic programming language where you control both how code looks and how it behaves**

Fracture is a proof-of-concept programming language that fundamentally rethinks how we write code. Instead of forcing you into a single syntax and semantics, Fracture lets you choose - or even create - your own. Write Rust-like code, Python-style indentation, or invent something entirely new. The compiler doesn't care. It all compiles to the same high-performance native code.

## The Big Idea

Most programming languages lock you into a specific syntax and set of rules. Want optional semicolons? That's a different language. Prefer indentation over braces? Another language. Different error handling semantics? Yet another language.

**Fracture breaks this pattern.**

At its core, Fracture uses **HSIR** (High-level Syntax-agnostic Intermediate Representation) - a language-agnostic format that separates *what your code does* from *how it looks*. This unlocks two powerful features:

### Syntax Customization

Don't like the default syntax? Change it. Fracture's syntax system is completely modular. You can:

- Use the built-in Rust-like syntax
- Switch to Fracture Standard Syntax (FSS)
- Export and modify the syntax rules to create your own style
- Share syntax styles as simple configuration files

The same program can be written in multiple syntaxes - they all compile to identical code.

### Semantic Customization via Glyphs

Here's where it gets interesting. **Glyphs** are compiler extensions that add semantic rules and safety checks to your code. Want type checking? Import a glyph. Need borrow checking? There's a glyph for that. Building a domain-specific language? Write a custom glyph.

Glyphs can:
- Add new syntax constructs to the language
- Enforce safety guarantees (types, memory, errors)
- Implement custom compile-time checks
- Transform code during compilation

Think of glyphs as "compiler plugins that understand your intent."

## Architecture

Fracture is built from several components that work together:

**Rift** is the package manager and build tool - your main interface to Fracture. It handles project creation, building, dependencies, and syntax configuration.

**The IR Layer** (fracture-ir) defines HSIR and provides the syntax configuration system. This is where code becomes syntax-agnostic.

**The Linter** (fracture-linter) parses source code in any configured syntax and projects it into HSIR. Different syntax styles are just different projections onto the same representation.

**The Compiler** (fracture-compiler) takes HSIR, runs glyph passes for semantic checking, and generates native x86 assembly for Linux, macOS, and Windows.

**The Standard Library** provides reusable functionality through a system called *shards* - modular components that can be mixed and matched.

## Installation

### Quick Install

```bash
curl -fsSL https://raw.githubusercontent.com/ZA1815/fracture/main/fracture-lang/install.sh | bash
```

Or download the install script:

```bash
wget https://raw.githubusercontent.com/ZA1815/fracture/main/fracture-lang/install.sh
chmod +x install.sh
./install.sh
```

### Build from Source

**Prerequisites**: Rust nightly (1.93.0+), NASM, and a system linker (ld)

```bash
git clone https://github.com/ZA1815/fracture.git
cd fracture
cargo build --release -p fracture-rift
sudo cp target/release/rift /usr/local/bin/
```

Verify: `rift --version`

## Quick Start

```bash
# Create a new project
rift init hello_fracture
cd hello_fracture

# Check syntax and semantics
rift check

# Build the project
rift build

# Run it
rift run
```

Your `src/main.frac` might look like this:

```rust
pub fn main() -> i32 {
    println("Hello from Fracture!");
    return 0;
}
```

Or with some standard library usage:

```rust
use std::io;

pub fn fibonacci(n: i32) -> i32 {
    if n <= 1 {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

pub fn main() -> i32 {
    io::println("Fibonacci of 10:");
    let result = fibonacci(10);
    return 0;
}
```

## Shards: Modular Standard Library

The Fracture standard library is organized into **shards** - independently versioned modules that provide specific functionality:

- **io**: Input/output operations, printing
- **fs**: Filesystem operations
- **collections**: Vec, HashMap, and data structures
- **string**: String manipulation

Import what you need:

```rust
use std::collections;
use std::io;
```

Shards can be mixed and matched. Future versions will support third-party shards.

## Glyphs: Programmable Semantics

Glyphs are what make Fracture's semantics customizable. They're compiler passes that can validate, transform, or extend your code.

### Using Built-in Glyphs

```rust
use glyph std::type_check;      // Type safety
use glyph std::borrow_check;    // Memory safety
use glyph std::error_check;     // Error handling validation
use glyph std::ffi_check;       // Foreign function safety
```

When you import a glyph, the compiler runs it during compilation. If the glyph finds issues, compilation fails with helpful diagnostics.

### Why Glyphs?

Traditional languages bake semantic rules into the compiler. Fracture makes them opt-in and extensible:

- **Minimal core**: The base language is small and permissive
- **Safety when you want it**: Import type checking only where you need it
- **Custom rules**: Write glyphs to enforce your team's conventions
- **Domain-specific checking**: Build specialized validation for your domain

Want a language with Go's simplicity but Rust's safety? Import the right glyphs. Building a scripting language? Skip the heavy checks. It's your choice.

## Syntax Customization

Fracture's syntax is defined in configuration files. You can modify it, extend it, or replace it entirely.

### Export Current Syntax

```bash
rift syntax export              # Save to fracture_syntax.toml
rift syntax export --inline     # Print to stdout
```

### Customize Syntax Interactively

```bash
rift syntax customize
```

This opens an interactive editor where you can modify keywords, operators, and syntax rules.

### Switch Between Styles

```bash
rift config set rust            # Rust-like syntax
rift config set fss             # Fracture Standard Syntax
```

Or set it in your project's `rift.toml`:

```toml
[syntax]
source_style = "rust"
```

### How It Works

Syntax configurations define mappings from surface syntax to HSIR constructs. When you write `fn main() {}` in Rust-style or `def main():` in Python-style, the linter uses these mappings to produce the same HSIR function definition.

This means you can:
- Collaborate with teammates using different syntax preferences
- Migrate projects between syntax styles without changing semantics
- Experiment with new syntax ideas without forking the compiler

## Project Configuration

Projects are configured via `rift.toml`:

```toml
[package]
name = "my_project"
version = "0.1.0"
description = "A Fracture project"

[syntax]
source_style = "rust"

[dependencies]
# Future: third-party dependencies
```

## Commands

```bash
rift init <name>               # Create new project
rift check                     # Lint and run glyph checks
rift build                     # Compile to executable
rift build --release           # Optimized build
rift run                       # Build and run
rift clean                     # Remove build artifacts

rift syntax export             # Export syntax configuration
rift syntax customize          # Interactive syntax editor

rift config show               # Show configuration
rift config set <style>        # Set syntax style
rift config path               # Show config file location
```

## Target Platforms

Fracture currently supports:
- **x86_64 Linux** (fully supported)

macOS and Windows support are planned for future releases. The core architecture supports multiple targets, but the code generator currently only emits x86_64 Linux assembly.

## Current Status

**This is a proof of concept.** Fracture demonstrates a new approach to language design, but it's not production-ready. Expect:

- Rough edges and incomplete features
- API changes between versions
- Limited standard library
- Bugs and missing error messages

That said, the core ideas are solid and the architecture works. This is a foundation to build on.

## What Makes This Different?

**Syntax-agnostic**: Most "multi-syntax" languages are transpilers or preprocessors. Fracture's syntax system is part of the compiler architecture.

**Semantic customization**: Glyphs aren't macros or preprocessing - they're first-class compiler passes with access to typed IR.

**Single compilation target**: Different syntaxes and glyph combinations produce the same efficient native code. No runtime overhead.

## Future Directions

- Package registry for sharing shards and glyphs
- More target architectures (ARM, RISC-V)
- Language server protocol (LSP) for IDE support
- Standard library expansion
- Community-contributed glyphs and syntax styles

## Contributing

Fracture is experimental and evolving. Contributions, ideas, and feedback are welcome.

1. Fork the repository
2. Create a feature branch
3. Make changes and add tests
4. Run `cargo test --workspace`
5. Submit a pull request

## License

MIT License. See [LICENSE](LICENSE) for details.

## Learn More

- Read the source in `fracture-lang/`
- Explore example projects
- Check out the standard library in `fracture-stdlib/`

---
