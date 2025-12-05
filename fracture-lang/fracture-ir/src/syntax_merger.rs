use crate::glyph_loader::GlyphManifest;
use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct DynamicKeywords {
    keyword_to_type: HashMap<String, String>,
    type_to_keyword: HashMap<String, String>,
}

impl DynamicKeywords {
    pub fn new() -> Self {
        Self {
            keyword_to_type: HashMap::new(),
            type_to_keyword: HashMap::new(),
        }
    }

    pub fn add_from_glyph(&mut self, manifest: &GlyphManifest) {
        for (semantic_type, keyword_string) in &manifest.syntax.keywords {
            self.keyword_to_type.insert(keyword_string.clone(), semantic_type.clone());
            self.type_to_keyword.insert(semantic_type.clone(), keyword_string.clone());
        }
    }

    pub fn get_type(&self, keyword: &str) -> Option<&String> {
        self.keyword_to_type.get(keyword)
    }

    pub fn get_keyword(&self, semantic_type: &str) -> Option<&String> {
        self.type_to_keyword.get(semantic_type)
    }

    pub fn has_type(&self, semantic_type: &str) -> bool {
        self.type_to_keyword.contains_key(semantic_type)
    }

    pub fn keywords(&self) -> impl Iterator<Item = &String> {
        self.keyword_to_type.keys()
    }

    pub fn types(&self) -> impl Iterator<Item = &String> {
        self.type_to_keyword.keys()
    }
}

pub fn build_keyword_registry(manifests: &[GlyphManifest]) -> DynamicKeywords {
    let mut registry = DynamicKeywords::new();

    for manifest in manifests {
        registry.add_from_glyph(manifest);
    }

    registry
}
