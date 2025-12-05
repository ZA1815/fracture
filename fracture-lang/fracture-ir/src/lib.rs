pub mod hsir;
pub mod printer;
pub mod syntax_config;
pub mod glyph_loader;
pub mod syntax_merger;

pub use hsir::{
    Inst,
    Type,
    Value,
    Reg,
    Program,
    Function,
    Const,
    Module,
    ModulePath,
    PathSegment,
    UseStatement,
    UseTree,
    Visibility,
    StructDef,
    ImportType,
    ModuleData,
    Shard,
    Glyph,
    GlyphScope,
};
pub use syntax_config::SyntaxConfig;
pub use glyph_loader::{GlyphManifest, load_glyph, resolve_glyph_path};
pub use syntax_merger::{DynamicKeywords, build_keyword_registry};