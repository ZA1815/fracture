use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SyntaxConfig {
    pub name: String,
    pub keywords: Keywords,
    pub tokens: TokenConfig,
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
    pub mut_kw: String,

    pub struct_kw: String
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TokenConfig {
    pub arrow: String,
    pub double_equals: String,
    pub not_equals: String,
    pub less_equals: String,
    pub greater_equals: String,

    pub immutable_ref: String,
    pub mutable_ref: String,

    pub assignment: String,
    pub plus: String,
    pub minus: String,
    pub star: String,
    pub slash: String,
    pub less: String,
    pub greater: String,

    pub left_paren: String,
    pub right_paren: String,
    pub left_brace: String,
    pub right_brace: String,
    pub comma: String,
    pub semicolon: String,
    pub colon: String
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
                mut_kw: "".to_string(),
                struct_kw: "class".to_string()
            },
            tokens: TokenConfig {
                arrow: "->".to_string(),
                double_equals: "==".to_string(),
                not_equals: "!=".to_string(),
                less_equals: "<=".to_string(),
                greater_equals: ">=".to_string(),
                immutable_ref: "".to_string(),
                mutable_ref: "".to_string(),
                assignment: "=".to_string(),
                plus: "+".to_string(),
                minus: "-".to_string(),
                star: "*".to_string(),
                slash: "/".to_string(),
                less: "<".to_string(),
                greater: ">".to_string(),
                left_paren: "(".to_string(),
                right_paren: ")".to_string(),
                left_brace: "{".to_string(),
                right_brace: "}".to_string(),
                comma: ",".to_string(),
                semicolon: ";".to_string(),
                colon: ":".to_string(),
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
                mut_kw: "mut".to_string(),
                struct_kw: "struct".to_string()
            },
            tokens: TokenConfig {
                arrow: "->".to_string(),
                double_equals: "==".to_string(),
                not_equals: "!=".to_string(),
                less_equals: "<=".to_string(),
                greater_equals: ">=".to_string(),
                immutable_ref: "&".to_string(),
                mutable_ref: "&mut".to_string(),
                assignment: "=".to_string(),
                plus: "+".to_string(),
                minus: "-".to_string(),
                star: "*".to_string(),
                slash: "/".to_string(),
                less: "<".to_string(),
                greater: ">".to_string(),
                left_paren: "(".to_string(),
                right_paren: ")".to_string(),
                left_brace: "{".to_string(),
                right_brace: "}".to_string(),
                comma: ",".to_string(),
                semicolon: ";".to_string(),
                colon: ":".to_string()
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

    pub fn validate(&self) -> Result<(), String> {
        let mut all_tokens = Vec::new();

        // Need to create a default so that it doesn't crash
        let tc = &self.tokens;
        if !tc.arrow.is_empty() { all_tokens.push(("arrow", &tc.arrow)); }
        if !tc.double_equals.is_empty() { all_tokens.push(("double_equals", &tc.double_equals)); }
        if !tc.not_equals.is_empty() { all_tokens.push(("not_equals", &tc.not_equals)); }
        if !tc.less_equals.is_empty() { all_tokens.push(("less_equal", &tc.less_equals)); }
        if !tc.greater_equals.is_empty() { all_tokens.push(("greater_equal", &tc.greater_equals)); }
        if !tc.immutable_ref.is_empty() { all_tokens.push(("immutable_ref", &tc.immutable_ref)); }
        if !tc.mutable_ref.is_empty() { all_tokens.push(("mutable_ref", &tc.mutable_ref)); }
        if !tc.assignment.is_empty() { all_tokens.push(("assignment", &tc.assignment)); }
        if !tc.plus.is_empty() { all_tokens.push(("plus", &tc.plus)); }
        if !tc.minus.is_empty() { all_tokens.push(("minus", &tc.minus)); }
        if !tc.star.is_empty() { all_tokens.push(("star", &tc.star)); }
        if !tc.slash.is_empty() { all_tokens.push(("slash", &tc.slash)); }
        if !tc.less.is_empty() { all_tokens.push(("less", &tc.less)); }
        if !tc.greater.is_empty() { all_tokens.push(("greater", &tc.greater)); }

        for i in 0..all_tokens.len() {
            for j in (i + 1)..all_tokens.len() {
                if all_tokens[i].1 == all_tokens[j].1 {
                    return Err(format!(
                        "Token conflict: '{}' and '{}' both map to '{}'",
                        all_tokens[i].0, all_tokens[j].0, all_tokens[i].1
                    ))
                }
            }
        }

        for (name, token) in &all_tokens {
            if token.contains(char::is_whitespace) {
                return Err(format!(
                    "Token '{}' contains whitespace: '{}'",
                    name, token
                ));
            }
        }

        Ok(())
    }

    pub fn from_file(path: &str) -> Result<Self, String> {
        let contents = std::fs::read_to_string(path)
            .map_err(|e| format!("Failed to read config: {}", e))?;

        let config: Self = toml::from_str(&contents)
            .map_err(|e| format!("Failed to parse config: {}", e))?;

        config.validate()?;

        Ok(config)
    }
}