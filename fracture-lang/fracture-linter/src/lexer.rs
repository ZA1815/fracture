use crate::syntax_config::*;

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

    Ident(String),
    Number(i64),
    Float(f64),
    String(String),
    Bool(bool),

    Plus,
    Minus,
    Star,
    Slash,
    Equals,
    DoubleEquals,
    Less,
    Greater,

    LeftParentheses,
    RightParentheses,
    LeftBrace,
    RightBrace,
    Colon,
    Semicolon,
    Comma,
    Arrow,

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
    at_line_start: bool
}

impl Lexer {
    pub fn new(input: &str, config: SyntaxConfig) -> Self {
        Self {
            input: input.chars().collect(),
            pos: 0,
            config,
            indent_stack: vec![0],
            at_line_start: true
        }
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

        let ch = self.input[self.pos];

        let token = match ch {
            '+' => { self.pos += 1; Token::Plus }
            '-' => {
                if self.peek_next() == Some('>') {
                    self.pos += 2;
                    Token::Arrow
                }
                else {
                    self.pos += 1;
                    Token::Minus
                }
            }
            '*' => { self.pos += 1; Token::Star }
            '/' => { self.pos += 1; Token::Slash }
            '(' => { self.pos += 1; Token::LeftParentheses }
            ')' => { self.pos += 1; Token::RightParentheses }
            '{' => { self.pos += 1; Token::LeftBrace }
            '}' => { self.pos += 1; Token::RightBrace }
            ':' => { self.pos += 1; Token::Colon }
            ';' => { self.pos += 1; Token::Semicolon }
            ',' => { self.pos += 1; Token::Comma },
            '=' => {
                if self.peek_next() == Some('=') {
                    self.pos += 2;
                    Token::DoubleEquals
                }
                else {
                    self.pos += 1;
                    Token::Equals
                }
            }
            '<' => { self.pos += 1; Token::Less }
            '>' => { self.pos += 1; Token::Greater }
            '\n' => {
                self.pos += 1;
                self.at_line_start = true;
                Token::Newline
            }
            '"' => self.lex_string(),
            _ if ch.is_ascii_digit() => self.lex_number(),
            _ if ch.is_ascii_alphabetic() || ch == '_' => self.lex_ident_or_keyword(),
            _ => {
                self.pos += 1;
                self.next_token()
            }
        };

        token
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

        if self.pos < self.input.len() && self.input[self.pos] == '.' {
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

    fn peek_next(&self) -> Option<char> {
        if self.pos + 1 < self.input.len() {
            Some(self.input[self.pos + 1])
        }
        else {
            None
        }
    }
}