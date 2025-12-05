use serde::{Serialize, Deserialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::fs;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GlyphManifest {
    pub glyph: GlyphMetadata,
    #[serde(default)]
    pub syntax: GlyphSyntax,
    #[serde(default)]
    pub dependencies: GlyphDependencies,
    #[serde(default)]
    pub validation: GlyphValidation,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GlyphMetadata {
    pub name: String,
    pub version: String,
    pub scope: String,
    pub description: String,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GlyphSyntax {
    #[serde(default)]
    pub keywords: HashMap<String, String>,
    #[serde(default)]
    pub tokens: HashMap<String, String>,
    #[serde(default)]
    pub functions: HashMap<String, String>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GlyphDependencies {
    #[serde(default)]
    pub glyphs: Vec<String>,
    #[serde(default)]
    pub suggests: Vec<String>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GlyphValidation {
    #[serde(default)]
    pub rules: Vec<String>,
}

pub fn load_glyph_manifest(glyph_dir: &Path) -> Result<GlyphManifest, String> {
    let manifest_path = glyph_dir.join("glyph.toml");

    if !manifest_path.exists() {
        return Err(format!("Glyph manifest not found: {}", manifest_path.display()));
    }

    let content = fs::read_to_string(&manifest_path)
        .map_err(|e| format!("Failed to read glyph manifest: {}", e))?;

    let manifest: GlyphManifest = toml::from_str(&content)
        .map_err(|e| format!("Failed to parse glyph manifest: {}", e))?;

    Ok(manifest)
}

pub fn resolve_glyph_path(import_path: &str, stdlib_path: &Path) -> Result<PathBuf, String> {
    // For now, we only support std:: glyphs
    let parts: Vec<&str> = import_path.split("::").collect();

    if parts.len() != 2 {
        return Err(format!("Invalid glyph path: {}. Expected format: std::glyph_name", import_path));
    }

    let (namespace, glyph_name) = (parts[0], parts[1]);

    if namespace != "std" {
        return Err(format!("Unknown glyph namespace: {}. Currently only 'std' is supported", namespace));
    }

    let glyph_dir = stdlib_path.join("glyphs").join(glyph_name);

    if !glyph_dir.exists() {
        return Err(format!("Glyph not found: {} (looked in {})", import_path, glyph_dir.display()));
    }

    Ok(glyph_dir)
}

pub fn load_glyph(import_path: &str, stdlib_path: &Path) -> Result<GlyphManifest, String> {
    let glyph_dir = resolve_glyph_path(import_path, stdlib_path)?;
    load_glyph_manifest(&glyph_dir)
}