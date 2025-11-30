use fracture_ir::{SyntaxConfig, syntax_config::BlockStyle};
use crate::errors::{Span, Position};

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
    // Have to change later but its complicated
    DoubleColon,
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
    Eof,

    Mod,
    Use,
    Pub,
    As,
    SelfKw,
    Super
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

impl SpannedToken {
    pub fn new(token: Token, span: Span) -> Self {
        Self { token, span }
    }
}

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
    line: usize,
    column: usize,
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
            line: 1,
            column: 1,
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
        // Have to change later but its complicated
        list.push(("::".to_string(), Token::DoubleColon));

        list.sort_by(|a, b| b.0.len().cmp(&a.0.len()));
        
        list
    }

    pub fn next_token(&mut self) -> SpannedToken {
        if self.at_line_start && matches!(self.config.style.block_style, BlockStyle::Indentation) {
            self.at_line_start = false;
            let start = self.current_position();
            let indent = self.count_indent();
            let current = *self.indent_stack.last().unwrap();

            if indent > current {
                self.indent_stack.push(indent);
                return SpannedToken::new(Token::Indent, Span::new(start, self.current_position()));
            }
            else if indent < current {
                self.indent_stack.pop();
                return SpannedToken::new(Token::Dedent, Span::new(start, self.current_position()));
            }
        }

        self.skip_whitespace();

        let start= self.current_position();

        if self.pos >= self.input.len() {
            return SpannedToken::new(Token::Eof, Span::new(start, start));
        }

        let token_map = self.token_map.clone();

        for (token_str, token) in &token_map {
            if self.matches_at_pos(token_str) {
                self.advance_by(token_str.len());
                let end = self.current_position();
                return SpannedToken::new(token.clone(), Span::new(start, end));
            }
        }

        // Add customization for this later
        if self.input[self.pos] == '\n' {
            self.advance_char();
            self.at_line_start = true;
            let end = self.current_position();
            return SpannedToken::new(Token::Newline, Span::new(start, end));
        }

        // Add customization for this later
        if self.input[self.pos] == '"' {
            return self.lex_string(start);
        }

        if self.input[self.pos].is_ascii_digit() {
            return self.lex_number(start);
        }

        // Maybe add customization?
        if self.input[self.pos].is_ascii_alphabetic() || self.input[self.pos] == '_' {
            return self.lex_ident_or_keyword(start);
        }

        // Likely return error later instead of just skipping
        self.advance_char();
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

    fn lex_ident_or_keyword(&mut self, start: Position) -> SpannedToken {
        let ident_start = self.pos;
        while self.pos < self.input.len() && (self.input[self.pos].is_ascii_alphanumeric() || self.input[self.pos] == '_') {
            self.advance_char();
        }

        let text: String = self.input[ident_start..self.pos].iter().collect();
        let end=  self.current_position();
        let span = Span::new(start, end);

        let token = if text == self.config.keywords.function {
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
        else if !self.config.keywords.mod_kw.is_empty() && text == self.config.keywords.mod_kw {
            Token::Mod
        }
        else if !self.config.keywords.use_kw.is_empty() && text == self.config.keywords.use_kw {
            Token::Use
        }
        else if !self.config.keywords.pub_kw.is_empty() && text == self.config.keywords.pub_kw {
            Token::Pub
        }
        else if !self.config.keywords.as_kw.is_empty() && text == self.config.keywords.as_kw {
            Token::As
        }
        else if !self.config.keywords.self_kw.is_empty() && text == self.config.keywords.self_kw {
            Token::SelfKw
        }
        else if !self.config.keywords.super_kw.is_empty() && text == self.config.keywords.super_kw {
            Token::Super
        }
        else if text == "true" {
            Token::Bool(true)
        }
        else if text == "false" {
            Token::Bool(false)
        }
        else {
            Token::Ident(text)
        };

        SpannedToken::new(token, span)
    }

    fn lex_number(&mut self, start: Position) -> SpannedToken {
        let num_start = self.pos;
        while self.pos < self.input.len() && self.input[self.pos].is_ascii_digit() {
            self.pos += 1;
        }
        
        // If we ever make slice op customizable, this has to change
        let is_dot = self.pos < self.input.len() && self.input[self.pos] == '.';
        let is_range_start = is_dot && self.pos + 1 < self.input.len() && self.input[self.pos + 1] == '.';
        
        if is_dot && !is_range_start {
            self.advance_char();
            while self.pos < self.input.len() && self.input[self.pos].is_ascii_digit() {
                self.pos += 1;
            }
            let text: String = self.input[num_start..self.pos].iter().collect();
            let end = self.current_position();
            SpannedToken::new(Token::Float(text.parse().unwrap()), Span::new(start, end))
        }
        else {
            let text: String = self.input[num_start..self.pos].iter().collect();
            let end=  self.current_position();
            SpannedToken::new(Token::Number(text.parse().unwrap()), Span::new(start, end))
        }
    }

    fn lex_string(&mut self, start: Position) -> SpannedToken {
        self.advance_char();
        let str_start = self.pos;

        while self.pos < self.input.len() && self.input[self.pos] != '"' {
            self.advance_char();
        }

        let text: String = self.input[str_start..self.pos].iter().collect();
        if self.pos < self.input.len() {
            // Emit error here if string wasn't closed
            self.advance_char();
        }

        let end = self.current_position();
        
        SpannedToken::new(Token::String(text), Span::new(start, end))
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.input.len() {
            match self.input[self.pos] {
                ' ' | '\t' | '\r' => self.advance_char(),
                _ => break
            }
        }
    }

    fn count_indent(&mut self) -> usize {
        let mut count = 0;
        while self.pos < self.input.len() && self.input[self.pos] == ' ' {
            count += 1;
            self.advance_char();
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

    fn current_position(&self) -> Position {
        Position::new(self.pos, self.line, self.column)
    }

    fn advance_char(&mut self) {
        if self.pos < self.input.len() {
            if self.input[self.pos] == '\n' {
                self.line += 1;
                self.column = 1;
            }
            else {
                self.column += 1;
            }

            self.pos += 1;
        }
    }

    fn advance_by(&mut self, n: usize) {
        for _ in 0..n {
            self.advance_char();
        }
    }

    fn current_line(&self) -> usize {
        self.line
    }

    fn current_column(&self) -> usize {
        self.column
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position_tracking() {
        let config = SyntaxConfig::rust();
        let mut lexer = Lexer::new("let x = 42;", config);
        
        // 'let' should be at line 1, column 1
        let tok1 = lexer.next_token();
        assert_eq!(tok1.token, Token::Let);
        assert_eq!(tok1.span.start.line, 1);
        assert_eq!(tok1.span.start.column, 1);
        
        // 'x' should be at line 1, column 5
        let tok2 = lexer.next_token();
        assert_eq!(tok2.token, Token::Ident("x".to_string()));
        assert_eq!(tok2.span.start.line, 1);
        assert_eq!(tok2.span.start.column, 5);
    }
    
    #[test]
    fn test_multiline_tracking() {
        let config = SyntaxConfig::rust();
        let source = "let x = 1;\nlet y = 2;";
        let mut lexer = Lexer::new(source, config);
        
        // Skip to second line
        for _ in 0..6 { lexer.next_token(); } // let, x, =, 1, ;, newline
        
        // 'let' on line 2
        let tok = lexer.next_token();
        assert_eq!(tok.token, Token::Let);
        assert_eq!(tok.span.start.line, 2);
        assert_eq!(tok.span.start.column, 1);
    }

    #[test]
    fn test_mod_keyword() {
        let config = SyntaxConfig::rust();
        let mut lexer = Lexer::new("mod foo;", config);
        
        let tok = lexer.next_token();
        assert_eq!(tok.token, Token::Mod);
    }

    #[test]
    fn test_double_colon() {
        let config = SyntaxConfig::rust();
        let mut lexer = Lexer::new("std::io", config);
        
        assert_eq!(lexer.next_token().token, Token::Ident("std".to_string()));
        assert_eq!(lexer.next_token().token, Token::DoubleColon);
        assert_eq!(lexer.next_token().token, Token::Ident("io".to_string()));
    }
}