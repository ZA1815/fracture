use colored::*;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub column: usize
}

impl Position {
    pub fn new(offset: usize, line: usize, column: usize) -> Self {
        Self { offset, line, column }
    }

    pub fn start() -> Self {
        Self { offset: 0, line: 1, column: 1 }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub start: Position,
    pub end: Position
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn from_offsets(start: usize, end: usize) -> Self {
        Self { start: Position::new(start, 0, 0), end: Position::new(end, 0, 0) }
    }

    pub fn merge(&self, other: &Span) -> Span {
        Span {
            start: if self.start.offset < other.start.offset { self.start } else { other.start },
            end: if self.end.offset < other.end.offset { self.end } else { other.end } }
    }

    pub fn dummy() -> Self {
        Self::default()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Note,
    Help
}

impl Severity {
    pub fn prefix(&self) -> ColoredString {
        match self {
            Severity::Error => "error".red().bold(),
            Severity::Warning => "warning".yellow().bold(),
            Severity::Note => "note".blue().bold(),
            Severity::Help => "help".green().bold()
        }
    }
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: Span,
    pub notes: Vec<String>,
    pub suggestions: Vec<Suggestion>
}

#[derive(Debug, Clone)]
pub struct Suggestion {
    pub message: String,
    pub replacement: Option<String>
}

impl Diagnostic {
    pub fn error(message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Error,
            message: message.into(),
            span,
            notes: Vec::new(),
            suggestions: Vec::new()
        }
    }

    pub fn warning(message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Warning,
            message: message.into(),
            span,
            notes: Vec::new(),
            suggestions: Vec::new(),
        }
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn with_suggestion(mut self, message: impl Into<String>, replacement: impl Into<String>) -> Self {
        self.suggestions.push(Suggestion {
            message: message.into(),
            replacement: Some(replacement.into())
        });

        self
    }

    pub fn with_help(mut self, message: impl Into<String>) -> Self {
        self.suggestions.push(Suggestion {
            message: message.into(),
            replacement: None
        });

        self
    }

    pub fn format(&self, source: &str, filename: &str) -> String {
        let mut output = String::new();

        output.push_str(&format!("{}: {}\n", self.severity.prefix(), self.message.bold()));

        output.push_str(&format!("  {} {}:{}:{}\n", "-->".blue().bold(), filename, self.span.start.line, self.span.start.column));

        if let Some(source_line) = get_source_line(source, self.span.start.line) {
            let line_num = self.span.start.line;
            let line_num_width = line_num.to_string().len();

            output.push_str(&format!("{:width$} {}\n", "", "|".blue().bold(), width = line_num_width + 1));

            output.push_str(&format!(
                "{:width$} {} {}\n",
                line_num.to_string().blue().bold(),
                "|".blue().bold(), source_line,
                width = line_num_width
            ));

            let caret_start = self.span.start.column.saturating_sub(1);
            let caret_len = if self.span.start.line == self.span.end.line {
                (self.span.end.column - self.span.start.column).max(1)
            }
            else {
                source_line.len().saturating_sub(caret_start).max(1)
            };

            let underline = format!(
                "{:width$} {} {:padding$}{}",
                "",
                "|".blue().bold(),
                "",
                "^".repeat(caret_len).red().bold(),
                width = line_num_width,
                padding = caret_start
            );
            output.push_str(&underline);
            output.push('\n');
        }

        for note in &self.notes {
            output.push_str(&format!(
                "  {} {}: {}\n",
                "=".blue().bold(),
                "note".blue().bold(),
                note
            ));
        }

        for suggestion in &self.suggestions {
            output.push_str(&format!(
                "   {} {}: {}\n",
                "=".blue().bold(),
                "help".green().bold(),
                suggestion.message
            ));

            if let Some(replacement) = &suggestion.replacement {
                output.push_str(&format!("      {}\n", replacement.green()));
            }
        }

        output
    }
}

fn get_source_line(source: &str, line: usize) -> Option<&str> {
    source.lines().nth(line.saturating_sub(1))
}

#[derive(Debug, Default)]
pub struct DiagnosticCollector {
    diagnostics: Vec<Diagnostic>,
    source: String,
    filename: String
}

impl DiagnosticCollector {
    pub fn new(source: String, filename: String) -> Self {
        Self {
            diagnostics: Vec::new(),
            source,
            filename
        }
    }

    pub fn emit(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn error(&mut self, message: impl Into<String>, span: Span) {
        self.emit(Diagnostic::error(message, span));
    }

    pub fn warning(&mut self, message: impl Into<String>,span: Span) {
        self.emit(Diagnostic::warning(message, span));
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| d.severity == Severity::Error)
    }

    pub fn error_count(&self) -> usize {
        self.diagnostics.iter().filter(|d| d.severity == Severity::Error).count()
    }

    pub fn warning_count(&self) -> usize {
        self.diagnostics.iter().filter(|d| d.severity == Severity::Warning).count()
    }

    pub fn emit_all(&self) {
        for diagnostic in &self.diagnostics {
            eprintln!("{}", diagnostic.format(&self.source, &self.filename));
        }

        let errors = self.error_count();
        let warnings = self.warning_count();

        if errors > 0 || warnings > 0 {
            let mut summary = String::new();
            if errors > 0 {
                summary.push_str(&format!("{}", format!("{}, error(s)", errors).red().bold()));
            }
            if warnings > 0 {
                if !summary.is_empty() {
                    summary.push_str(", ");
                }

                summary.push_str(&format!("{}", format!("{} warning(s)", warnings).yellow().bold()));
            }

            eprintln!("\n{}: {}", "Summary".bold(), summary)
        }
    }

    pub fn to_result<T>(&self, value: T) -> Result<T, String> {
        if self.has_errors() {
            let formatted: Vec<String> = self.diagnostics
                .iter()
                .filter(|d| d.severity == Severity::Error)
                .map(|d| d.format(&self.source, &self.filename))
                .collect();
            Err(formatted.join("\n"))
        }
        else {
            Ok(value)
        }
    }
}

pub fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let s1_chars: Vec<char> = s1.chars().collect();
    let s2_chars: Vec<char> = s2.chars().collect();

    let m = s1_chars.len();
    let n = s2_chars.len();

    if m == 0 { return n; }
    if n == 0 { return m; }

    let mut prev_row: Vec<usize> = (0..=n).collect();
    let mut curr_row: Vec<usize> = vec![0; n + 1];

    for i in 1..=m {
        curr_row[0] = i;

        for j in 1..=n {
            let cost = if s1_chars[i - 1] == s2_chars[j - 1] { 0 } else { 1 };

            curr_row[j] = std::cmp::min(std::cmp::min(curr_row[j - 1] + 1, prev_row[j] + 1), prev_row[j - 1] + cost);
        }

        std::mem::swap(&mut prev_row, &mut curr_row);
    }

    prev_row[n]
}

pub fn find_similar<'a>(target: &str, candidates: &[&'a str]) -> Option<&'a str> {
    let max_distance = match target.len() {
        0..=3 => 1,
        4..=6 => 2,
        _ => 3
    };

    let mut best_match: Option<&str> = None;
    let mut best_distance = usize::MAX;

    for &candidate in candidates {
        let distance = levenshtein_distance(target, candidate);

        if distance < best_distance && distance <= max_distance {
            best_distance = distance;
            best_match = Some(candidate);
        }
    }

    best_match
}

pub fn suggest_identifier(typo: &str, known_names: &[&str]) -> Option<String> {
    find_similar(typo, known_names).map(|s| s.to_string())
}   

impl Diagnostic {
    pub fn unexpected_token(found: &str, expected: &str, span: Span) -> Self {
        Self::error(
            format!("Expected {}, found `{}`", expected, found),
            span
        ).with_help(format!("Try adding {} here", expected))
    }

    pub fn undefined_variable(name: &str, span: Span, known_vars: &[&str]) -> Self {
        let mut diag = Self::error(format!(
            "Cannot find value `{}` in this scope", name),
            span
        );

        if let Some(suggestion) = find_similar(name, known_vars) {
            diag = diag.with_suggestion(
                format!("A local variable with a similar name exists"),
                suggestion.to_string()
            );
        }

        diag
    }

    pub fn undefined_function(name: &str, span: Span, known_funcs: &[&str]) -> Self {
        let mut diag = Self::error(
            format!("Cannot find function `{}` in this scope", name),
            span
        );

        if let Some(suggestion) = find_similar(name, known_funcs) {
            diag = diag.with_suggestion(
                format!("A function with a similar name exists"),
                suggestion.to_string()
            );
        }

        diag
    }

    pub fn type_mismatch(expected: &str, found: &str, span: Span) -> Self {
        Self::error(
            format!("Mismatched types: Expected `{}`, found `{}`", expected, found),
            span
        )
    }

    pub fn missing_semicolon(span: Span) -> Self {
        Self::error("Expected `;`", span).with_help("Add a semicolon at the end of the statement")
    }

    pub fn undisclosed_delimiter(opener: &str, span: Span) -> Self {
        Self::error(
            format!("Undisclosed delimiter `{}`", opener),
            span
        ).with_note(format!("This `{}` was never closed", opener))
    }

    pub fn unused_variable(name: &str, span: Span) -> Self {
        Self::warning(
            format!("Unused variable: `{}`", name),
            span
        ).with_help(format!("If this is intentional, prefix it with an underscore: `_{}`", name))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_levenshtein_identical() {
        assert_eq!(levenshtein_distance("hello", "hello"), 0);
    }
    
    #[test]
    fn test_levenshtein_one_edit() {
        assert_eq!(levenshtein_distance("cat", "bat"), 1);
        assert_eq!(levenshtein_distance("cat", "cart"), 1);
        assert_eq!(levenshtein_distance("cart", "cat"), 1);
    }
    
    #[test]
    fn test_levenshtein_multiple_edits() {
        assert_eq!(levenshtein_distance("kitten", "sitting"), 3);
    }
    
    #[test]
    fn test_find_similar() {
        let candidates = vec!["function", "return", "let", "mut"];
        
        assert_eq!(find_similar("functon", &candidates), Some("function"));
        
        assert_eq!(find_similar("retrun", &candidates), Some("return"));
        
        assert_eq!(find_similar("xyz", &candidates), None);
    }
    
    #[test]
    fn test_diagnostic_format() {
        let source = "fn main() {\n    let x = 42\n}";
        let span = Span {
            start: Position { offset: 23, line: 2, column: 15 },
            end: Position { offset: 24, line: 2, column: 16 },
        };
        
        let diag = Diagnostic::error("expected `;`", span)
            .with_help("add a semicolon at the end");
        
        let formatted = diag.format(source, "main.frac");
        
        assert!(formatted.contains("error"));
        assert!(formatted.contains("expected `;`"));
        assert!(formatted.contains("main.frac:2:15"));
    }
}