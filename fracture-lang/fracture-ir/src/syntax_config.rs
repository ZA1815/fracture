use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SyntaxConfig {
    pub name: String,
    pub keywords: Keywords,
    pub style: SyntaxStyle,
    pub typing: TypingRules
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Keywords {
    pub function: String,
    pub return_kw: String,
    pub if_kw: String,
    pub else_if_kw: String,
    pub else_kw: String,
    pub while_kw: String,
    pub for_kw: String,

    pub int_type: String,
    pub bool_type: String,
    pub string_type: String,

    pub let_kw: String,
    pub mut_kw: String
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SyntaxStyle {
    pub block_style: BlockStyle,
    pub needs_semicolon: bool,
    pub type_annotations: TypeAnnotationStyle
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BlockStyle {
    Braces,
    Indentation,
    Keywords
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypeAnnotationStyle {
    Colon,
    CStyle,
    None
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypingRules {
    pub explicit_types: bool,
    pub type_inference: bool
}

impl SyntaxConfig {
    pub fn python() -> Self {
        Self {
            name: "python".to_string(),
            keywords: Keywords {
                function: "def".to_string(),
                return_kw: "return".to_string(),
                if_kw: "if".to_string(),
                else_if_kw: "elif".to_string(),
                else_kw: "else".to_string(),
                while_kw: "while".to_string(),
                for_kw: "for".to_string(),
                int_type: "int".to_string(),
                bool_type: "bool".to_string(),
                string_type: "str".to_string(),
                let_kw: "".to_string(),
                mut_kw: "".to_string()
            },
            style: SyntaxStyle {
                block_style: BlockStyle::Indentation,
                needs_semicolon: false,
                type_annotations: TypeAnnotationStyle::Colon
            },
            typing: TypingRules {
                explicit_types: false,
                type_inference: true
            }
        }
    }

    pub fn rust() -> Self {
        Self {
            name: "rust".to_string(),
            keywords: Keywords {
                function: "fn".to_string(),
                return_kw: "return".to_string(),
                if_kw: "if".to_string(),
                else_if_kw: "else if".to_string(),
                else_kw: "else".to_string(),
                while_kw: "while".to_string(),
                for_kw: "for".to_string(),
                int_type: "i32".to_string(),
                bool_type: "bool".to_string(),
                string_type: "String".to_string(),
                let_kw: "let".to_string(),
                mut_kw: "mut".to_string()
            },
            style: SyntaxStyle {
                block_style: BlockStyle::Braces,
                needs_semicolon: true,
                type_annotations: TypeAnnotationStyle::Colon
            },
            typing: TypingRules {
                explicit_types: true,
                type_inference: true
            }
        }
    }
}