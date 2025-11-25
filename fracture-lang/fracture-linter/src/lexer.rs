use fracture_ir::{SyntaxConfig, syntax_config::BlockStyle};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Function,
    Return,
    If,
    ElseIf,
    Else,
    While,
    For,
    Let,
    Mut,
    Struct,

    Ident(String),
    Number(i64),
    Float(f64),
    String(String),
    Bool(bool),

    Plus,
    Minus,
    Star,
    Slash,

    Assignment,
    ObjectAccess,
    DoubleEquals,
    NotEquals,
    
    Less,
    Greater,
    LessEquals,
    GreaterEquals,

    ImmutableRef,
    MutableRef,

    LeftParentheses,
    RightParentheses,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Colon,
    Semicolon,
    Comma,
    Arrow,
    Hash,
    Dot,
    SliceDot,

    Newline,
    Indent,
    Dedent,
    Eof
}

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
    config: SyntaxConfig,
    indent_stack: Vec<usize>,
    at_line_start: bool,
    token_map: Vec<(String, Token)>
}

impl Lexer {
    pub fn new(input: &str, config: SyntaxConfig) -> Self {
        let token_map = Self::build_token_map(&config);

        Self {
            input: input.chars().collect(),
            pos: 0,
            config,
            indent_stack: vec![0],
            at_line_start: true,
            token_map
        }
    }

    fn build_token_map(config: &SyntaxConfig) -> Vec<(String, Token)> {
        let tc = &config.tokens;
        let mut list = Vec::new();

        if !tc.mutable_ref.is_empty() { 
            list.push((tc.mutable_ref.clone(), Token::MutableRef)); 
        }
        if !tc.immutable_ref.is_empty() { 
            list.push((tc.immutable_ref.clone(), Token::ImmutableRef)); 
        }
        if !tc.arrow.is_empty() { 
            list.push((tc.arrow.clone(), Token::Arrow)); 
        }
        if !tc.double_equals.is_empty() { 
            list.push((tc.double_equals.clone(), Token::DoubleEquals)); 
        }
        if !tc.not_equals.is_empty() { 
            list.push((tc.not_equals.clone(), Token::NotEquals)); 
        }
        if !tc.less_equals.is_empty() { 
            list.push((tc.less_equals.clone(), Token::LessEquals)); 
        }
        if !tc.greater_equals.is_empty() { 
            list.push((tc.greater_equals.clone(), Token::GreaterEquals)); 
        }
        if !tc.assignment.is_empty() { 
            list.push((tc.assignment.clone(), Token::Assignment)); 
        }
        if !tc.plus.is_empty() { 
            list.push((tc.plus.clone(), Token::Plus)); 
        }
        if !tc.minus.is_empty() { 
            list.push((tc.minus.clone(), Token::Minus)); 
        }
        if !tc.star.is_empty() { 
            list.push((tc.star.clone(), Token::Star)); 
        }
        if !tc.slash.is_empty() { 
            list.push((tc.slash.clone(), Token::Slash)); 
        }
        if !tc.less.is_empty() { 
            list.push((tc.less.clone(), Token::Less)); 
        }
        if !tc.greater.is_empty() { 
            list.push((tc.greater.clone(), Token::Greater)); 
        }
        if !tc.left_paren.is_empty() { 
            list.push((tc.left_paren.clone(), Token::LeftParentheses)); 
        }
        if !tc.right_paren.is_empty() { 
            list.push((tc.right_paren.clone(), Token::RightParentheses)); 
        }
        if !tc.left_brace.is_empty() { 
            list.push((tc.left_brace.clone(), Token::LeftBrace)); 
        }
        if !tc.right_brace.is_empty() { 
            list.push((tc.right_brace.clone(), Token::RightBrace)); 
        }
        if !tc.comma.is_empty() { 
            list.push((tc.comma.clone(), Token::Comma)); 
        }
        if !tc.semicolon.is_empty() { 
            list.push((tc.semicolon.clone(), Token::Semicolon)); 
        }
        if !tc.colon.is_empty() { 
            list.push((tc.colon.clone(), Token::Colon)); 
        }
        // Always the same (essentially reserved)
        list.push(("[".to_string(), Token::LeftBracket));
        list.push(("]".to_string(), Token::RightBracket));
        list.push(("#".to_string(), Token::Hash));
        list.push(("..".to_string(), Token::SliceDot));
        list.push((".".to_string(), Token::Dot));

        list.sort_by(|a, b| b.0.len().cmp(&a.0.len()));
        
        list
    }

    pub fn next_token(&mut self) -> Token {
        if self.at_line_start && matches!(self.config.style.block_style, BlockStyle::Indentation) {
            self.at_line_start = false;
            let indent = self.count_indent();
            let current = *self.indent_stack.last().unwrap();

            if indent > current {
                self.indent_stack.push(indent);
                return Token::Indent;
            }
            else if indent < current {
                self.indent_stack.pop();
                return Token::Dedent;
            }
        }

        self.skip_whitespace();

        if self.pos >= self.input.len() {
            return Token::Eof;
        }

        for (token_str, token) in &self.token_map {
            if self.matches_at_pos(token_str) {
                self.pos += token_str.len();
                return token.clone();
            }
        }

        // Add customization for this later
        if self.input[self.pos] == '\n' {
            self.pos += 1;
            self.at_line_start = true;
            return Token::Newline;
        }

        // Add customization for this later
        if self.input[self.pos] == '"' {
            return self.lex_string();
        }

        if self.input[self.pos].is_ascii_digit() {
            return self.lex_number();
        }

        // Maybe add customization?
        if self.input[self.pos].is_ascii_alphabetic() || self.input[self.pos] == '_' {
            return self.lex_ident_or_keyword();
        }

        // Likely return error later instead of just skipping
        self.pos += 1;
        self.next_token()
    }

    fn matches_at_pos(&self, s: &str) -> bool {
        let chars: Vec<char> = s.chars().collect();
        if self.pos + chars.len() > self.input.len() {
            return false;
        }

        for (i, ch) in chars.iter().enumerate() {
            if self.input[self.pos + i] != *ch {
                return false;
            }
        }

        true
    }

    fn lex_ident_or_keyword(&mut self) -> Token {
        let start = self.pos;
        while self.pos < self.input.len() && (self.input[self.pos].is_ascii_alphanumeric() || self.input[self.pos] == '_') {
            self.pos += 1;
        }

        let text: String = self.input[start..self.pos].iter().collect();

        if text == self.config.keywords.function {
            Token::Function
        }
        else if text == self.config.keywords.return_kw {
            Token::Return
        }
        else if text == self.config.keywords.if_kw {
            Token::If
        }
        else if text == self.config.keywords.else_if_kw {
            Token::ElseIf
        }
        else if text == self.config.keywords.else_kw {
            Token::Else
        }
        else if text == self.config.keywords.while_kw {
            Token::While
        }
        else if text == self.config.keywords.for_kw {
            Token::For
        }
        else if text == self.config.keywords.let_kw {
            Token::Let
        }
        else if text == self.config.keywords.mut_kw {
            Token::Mut
        }
        else if text == self.config.keywords.struct_kw {
            Token::Struct
        }
        else if text == "true" {
            Token::Bool(true)
        }
        else if text == "false" {
            Token::Bool(false)
        }
        else {
            Token::Ident(text)
        }
    }

    fn lex_number(&mut self) -> Token {
        let start = self.pos;
        while self.pos < self.input.len() && self.input[self.pos].is_ascii_digit() {
            self.pos += 1;
        }
        
        // If we ever make slice op customizable, this has to change
        let is_dot = self.pos < self.input.len() && self.input[self.pos] == '.';
        let is_range_start = is_dot && self.pos + 1 < self.input.len() && self.input[self.pos + 1] == '.';
        
        if is_dot && !is_range_start {
            self.pos += 1;
            while self.pos < self.input.len() && self.input[self.pos].is_ascii_digit() {
                self.pos += 1;
            }
            let text: String = self.input[start..self.pos].iter().collect();
            Token::Float(text.parse().unwrap())
        }
        else {
            let text: String = self.input[start..self.pos].iter().collect();
            Token::Number(text.parse().unwrap())
        }
    }

    fn lex_string(&mut self) -> Token {
        self.pos += 1;
        let start = self.pos;

        while self.pos < self.input.len() && self.input[self.pos] != '"' {
            self.pos += 1;
        }

        let text: String = self.input[start..self.pos].iter().collect();
        self.pos += 1;
        Token::String(text)
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.input.len() {
            match self.input[self.pos] {
                ' ' | '\t' | '\r' => self.pos += 1,
                _ => break
            }
        }
    }

    fn count_indent(&mut self) -> usize {
        let mut count = 0;
        while self.pos < self.input.len() && self.input[self.pos] == ' ' {
            count += 1;
            self.pos += 1;
        }

        count
    }

    pub fn peek_next(&self) -> Option<char> {
        if self.pos + 1 < self.input.len() {
            Some(self.input[self.pos + 1])
        }
        else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_greedy_matching() {
        let config = SyntaxConfig::rust();
        let mut lexer = Lexer::new("x == y", config);
        
        assert_eq!(lexer.next_token(), Token::Ident("x".to_string()));
        assert_eq!(lexer.next_token(), Token::DoubleEquals);
        assert_eq!(lexer.next_token(), Token::Ident("y".to_string()));
    }

    #[test]
    fn test_mutable_ref() {
        let config = SyntaxConfig::rust();
        let mut lexer = Lexer::new("&mut x", config);
        
        assert_eq!(lexer.next_token(), Token::MutableRef);
        assert_eq!(lexer.next_token(), Token::Ident("x".to_string()));
    }

    #[test]
    fn test_arrow() {
        let config = SyntaxConfig::rust();
        let mut lexer = Lexer::new("x -> y", config);
        
        assert_eq!(lexer.next_token(), Token::Ident("x".to_string()));
        assert_eq!(lexer.next_token(), Token::Arrow);
        assert_eq!(lexer.next_token(), Token::Ident("y".to_string()));
    }

    #[test]
    fn test_dot() {
        let config = SyntaxConfig::rust();
        let mut lexer = Lexer::new("user.age", config);
        
        assert_eq!(lexer.next_token(), Token::Ident("user".to_string()));
        assert_eq!(lexer.next_token(), Token::Dot);
        assert_eq!(lexer.next_token(), Token::Ident("age".to_string()));
    }
}