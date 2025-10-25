use crate::diagnostics::{RatError, RatResult, Severity};
use crate::source::SourceFile;
use crate::tokens::{Keyword, Symbol, Token, TokenKind};

#[derive(Debug, Clone)]
struct Cursor<'a> {
    source: &'a str,
    offset: usize,
}

impl<'a> Cursor<'a> {
    fn new(source: &'a str) -> Self {
        Self { source, offset: 0 }
    }

    #[inline]
    fn peek_char(&self) -> Option<char> {
        self.source[self.offset..].chars().next()
    }

    #[inline]
    fn peek_is(&self, ch: char) -> bool {
        matches!(self.peek_char(), Some(c) if c == ch)
    }

    #[inline]
    fn peek_n(&self, n: usize) -> Option<char> {
        self.source[self.offset..].chars().nth(n)
    }

    #[inline]
    fn advance(&mut self) -> Option<(usize, char)> {
        if self.offset >= self.source.len() {
            return None;
        }
        let mut iter = self.source[self.offset..].char_indices();
        let (_, ch) = iter.next()?;
        let next_offset = if let Some((idx, _)) = iter.next() {
            self.offset + idx
        } else {
            self.source.len()
        };
        let current_offset = self.offset;
        self.offset = next_offset;
        Some((current_offset, ch))
    }

    #[inline]
    fn slice_from(&self, start: usize) -> &str {
        &self.source[start..self.offset]
    }

    fn take_while<F>(&mut self, mut pred: F)
    where
        F: FnMut(char) -> bool,
    {
        while let Some(ch) = self.peek_char() {
            if pred(ch) {
                self.advance();
            } else {
                break;
            }
        }
    }
}

#[derive(Default, Debug)]
struct IndentStack {
    stack: Vec<usize>,
}

impl IndentStack {
    fn new() -> Self {
        Self { stack: vec![0] }
    }

    fn current(&self) -> usize {
        *self.stack.last().unwrap()
    }

    fn push(&mut self, indent: usize) {
        self.stack.push(indent);
    }

    fn pop(&mut self) -> Option<usize> {
        if self.stack.len() > 1 {
            self.stack.pop()
        } else {
            None
        }
    }

    fn depth(&self) -> usize {
        self.stack.len()
    }
}

/// Lex a Ratlang source file into a sequence of tokens (including indentation
/// markers).
pub fn lex(file: &SourceFile) -> RatResult<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut indent_stack = IndentStack::new();
    let mut nesting_level = 0usize;
    let mut line_start_offset = 0usize;
    let mut cursor = Cursor::new(&file.text);

    let mut current_line = 1u32;
    let bytes = file.text.as_bytes();

    while cursor.offset < file.text.len() {
        let line_start = cursor.offset;
        // Determine indent by counting spaces and tabs until newline or non-space
        let mut indent_width = 0usize;
        let mut idx = cursor.offset;
        while idx < bytes.len() {
            match bytes[idx] {
                b' ' => {
                    indent_width += 1;
                    idx += 1;
                }
                b'\t' => {
                    return Err(RatError::diagnostic(
                        Severity::Error,
                        format!(
                            "Tab indentation is not supported (line {current_line}). Use spaces instead."
                        ),
                    ));
                }
                b'\r' => {
                    idx += 1;
                }
                b'\n' => {
                    // Blank line
                    idx += 1;
                    cursor.offset = idx;
                    current_line += 1;
                    indent_width = 0;
                    break;
                }
                b'#' => {
                    if idx + 1 < bytes.len() && bytes[idx + 1] == b'#' {
                        break;
                    }
                    while idx < bytes.len() && bytes[idx] != b'\n' {
                        idx += 1;
                    }
                    if idx < bytes.len() {
                        idx += 1;
                    }
                    cursor.offset = idx;
                    current_line += 1;
                    indent_width = 0;
                    break;
                }
                _ => {
                    break;
                }
            }
        }

        if cursor.offset >= file.text.len() {
            break;
        }
        cursor.offset = idx;
        line_start_offset = line_start;

        // Determine content of line (excluding indentation)
        let line_slice = &file.text[line_start..];
        let mut end_idx = 0usize;
        while line_start + end_idx < file.text.len() {
            let byte = file.text.as_bytes()[line_start + end_idx];
            if byte == b'\n' {
                break;
            }
            end_idx += 1;
        }
        let line_content = &line_slice[..end_idx];
        if line_content.trim().is_empty() {
            cursor.offset = line_start + end_idx + 1;
            current_line += 1;
            continue;
        }

        // Manage indentation when not inside open constructs
        if nesting_level == 0 {
            let current = indent_stack.current();
            if indent_width > current {
                indent_stack.push(indent_width);
                let span = file.span(line_start, line_start + indent_width);
                tokens.push(Token::new(TokenKind::Indent, span));
            } else if indent_width < current {
                while indent_width < indent_stack.current() {
                    indent_stack
                        .pop()
                        .ok_or_else(|| RatError::error("Negative indentation"))?;
                    let span = file.span(line_start, line_start + indent_width);
                    tokens.push(Token::new(TokenKind::Dedent, span));
                }
                if indent_stack.current() != indent_width {
                    return Err(RatError::diagnostic(
                        Severity::Error,
                        format!("Inconsistent indentation on line {current_line}"),
                    ));
                }
            }
        }

        let mut line_cursor = Cursor {
            source: line_content,
            offset: indent_width.min(line_content.len()),
        };

        let line_indent_offset = line_start_offset;

        while let Some((col_offset, ch)) = line_cursor.advance() {
            let absolute_start = line_indent_offset + col_offset;
            if ch.is_whitespace() {
                continue;
            }
            let token = match ch {
                '0'..='9' => {
                    let mut is_float = false;
                    let mut has_exponent = false;
                    line_cursor.take_while(|c| c.is_ascii_digit() || c == '_');
                    loop {
                        if line_cursor.peek_is('.') && !is_float && !has_exponent {
                            if matches!(line_cursor.peek_n(1), Some('.')) {
                                break;
                            }
                            is_float = true;
                            line_cursor.advance();
                            line_cursor.take_while(|c| c.is_ascii_digit() || c == '_');
                            continue;
                        }
                        if matches!(line_cursor.peek_char(), Some('e' | 'E')) && !has_exponent {
                            has_exponent = true;
                            is_float = true;
                            line_cursor.advance();
                            if matches!(line_cursor.peek_char(), Some('+' | '-')) {
                                line_cursor.advance();
                            }
                            line_cursor.take_while(|c| c.is_ascii_digit() || c == '_');
                            continue;
                        }
                        break;
                    }
                    let literal = line_cursor.slice_from(col_offset);
                    if is_float {
                        let sanitized = literal.replace('_', "");
                        let value: f64 = sanitized.parse().map_err(|_| {
                            RatError::error(format!("Invalid float literal `{literal}`"))
                        })?;
                        TokenKind::FloatLiteral(value)
                    } else {
                        let sanitized = literal.replace('_', "");
                        let value: i128 = sanitized.parse().map_err(|_| {
                            RatError::error(format!("Invalid integer literal `{literal}`"))
                        })?;
                        TokenKind::IntLiteral(value)
                    }
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    line_cursor.take_while(|c| c.is_ascii_alphanumeric() || c == '_' || c == '?');
                    let ident = line_cursor.slice_from(col_offset);
                    if let Some(keyword) = Keyword::from_ident(ident) {
                        match keyword {
                            Keyword::True => TokenKind::BoolLiteral(true),
                            Keyword::False => TokenKind::BoolLiteral(false),
                            Keyword::None => TokenKind::Keyword(Keyword::None),
                            _ => TokenKind::Keyword(keyword),
                        }
                    } else {
                        TokenKind::Identifier(ident.into())
                    }
                }
                '"' => {
                    let mut escaped = false;
                    let mut value = String::new();
                    while let Some((_, ch)) = line_cursor.advance() {
                        if escaped {
                            let actual = match ch {
                                'n' => '\n',
                                'r' => '\r',
                                't' => '\t',
                                '\\' => '\\',
                                '"' => '"',
                                other => other,
                            };
                            value.push(actual);
                            escaped = false;
                        } else if ch == '\\' {
                            escaped = true;
                        } else if ch == '"' {
                            break;
                        } else {
                            value.push(ch);
                        }
                    }
                    TokenKind::StringLiteral(value)
                }
                '#' => {
                    // doc comment or ordinary comment
                    if let Some((_, '#')) = line_cursor.advance() {
                        let doc = line_cursor.source[line_cursor.offset..].trim().to_string();
                        line_cursor.offset = line_cursor.source.len();
                        TokenKind::DocComment(doc)
                    } else {
                        // Ordinary comment: skip rest of line
                        break;
                    }
                }
                '(' => {
                    nesting_level += 1;
                    TokenKind::Symbol(Symbol::LParen)
                }
                ')' => {
                    if nesting_level > 0 {
                        nesting_level -= 1;
                    }
                    TokenKind::Symbol(Symbol::RParen)
                }
                '[' => {
                    nesting_level += 1;
                    TokenKind::Symbol(Symbol::LBracket)
                }
                ']' => {
                    if nesting_level > 0 {
                        nesting_level -= 1;
                    }
                    TokenKind::Symbol(Symbol::RBracket)
                }
                '{' => {
                    nesting_level += 1;
                    TokenKind::Symbol(Symbol::LBrace)
                }
                '}' => {
                    if nesting_level > 0 {
                        nesting_level -= 1;
                    }
                    TokenKind::Symbol(Symbol::RBrace)
                }
                '.' => {
                    if line_cursor.peek_is('.') {
                        line_cursor.advance();
                        if line_cursor.peek_is('=') {
                            line_cursor.advance();
                            TokenKind::Symbol(Symbol::RangeInclusive)
                        } else if line_cursor.peek_is('.') {
                            line_cursor.advance();
                            TokenKind::Symbol(Symbol::Ellipsis)
                        } else {
                            TokenKind::Symbol(Symbol::Range)
                        }
                    } else {
                        TokenKind::Symbol(Symbol::Dot)
                    }
                }
                ':' => {
                    if line_cursor.peek_is(':') {
                        line_cursor.advance();
                        TokenKind::Symbol(Symbol::DoubleColon)
                    } else {
                        TokenKind::Symbol(Symbol::Colon)
                    }
                }
                ',' => TokenKind::Symbol(Symbol::Comma),
                '|' => {
                    if line_cursor.peek_is('>') {
                        line_cursor.advance();
                        TokenKind::Symbol(Symbol::PipeGreater)
                    } else if line_cursor.peek_is('|') {
                        line_cursor.advance();
                        TokenKind::Symbol(Symbol::PipePipe)
                    } else {
                        TokenKind::Symbol(Symbol::Pipe)
                    }
                }
                '&' => {
                    if line_cursor.peek_is('&') {
                        line_cursor.advance();
                        TokenKind::Symbol(Symbol::AmpAmp)
                    } else if line_cursor.peek_is('=') {
                        line_cursor.advance();
                        TokenKind::Symbol(Symbol::AmpEquals)
                    } else {
                        TokenKind::Symbol(Symbol::Amp)
                    }
                }
                '!' => {
                    if line_cursor.peek_is('=') {
                        line_cursor.advance();
                        TokenKind::Symbol(Symbol::NotEq)
                    } else {
                        TokenKind::Symbol(Symbol::Bang)
                    }
                }
                '=' => {
                    if line_cursor.peek_is('>') {
                        line_cursor.advance();
                        TokenKind::Symbol(Symbol::ThickArrow)
                    } else if line_cursor.peek_is('=') {
                        line_cursor.advance();
                        TokenKind::Symbol(Symbol::EqEq)
                    } else {
                        TokenKind::Symbol(Symbol::Equals)
                    }
                }
                '-' => {
                    if line_cursor.peek_is('>') {
                        line_cursor.advance();
                        TokenKind::Symbol(Symbol::Arrow)
                    } else if line_cursor.peek_is('=') {
                        line_cursor.advance();
                        TokenKind::Symbol(Symbol::MinusEquals)
                    } else {
                        TokenKind::Symbol(Symbol::Minus)
                    }
                }
                '+' => {
                    if line_cursor.peek_is('=') {
                        line_cursor.advance();
                        TokenKind::Symbol(Symbol::PlusEquals)
                    } else {
                        TokenKind::Symbol(Symbol::Plus)
                    }
                }
                '*' => {
                    if line_cursor.peek_is('=') {
                        line_cursor.advance();
                        TokenKind::Symbol(Symbol::StarEquals)
                    } else {
                        TokenKind::Symbol(Symbol::Star)
                    }
                }
                '/' => {
                    if line_cursor.peek_is('=') {
                        line_cursor.advance();
                        TokenKind::Symbol(Symbol::SlashEquals)
                    } else {
                        TokenKind::Symbol(Symbol::Slash)
                    }
                }
                '%' => {
                    if line_cursor.peek_is('=') {
                        line_cursor.advance();
                        TokenKind::Symbol(Symbol::PercentEquals)
                    } else {
                        TokenKind::Symbol(Symbol::Percent)
                    }
                }
                '<' => {
                    if line_cursor.peek_is('=') {
                        line_cursor.advance();
                        TokenKind::Symbol(Symbol::Le)
                    } else {
                        TokenKind::Symbol(Symbol::Lt)
                    }
                }
                '>' => {
                    if line_cursor.peek_is('=') {
                        line_cursor.advance();
                        TokenKind::Symbol(Symbol::Ge)
                    } else {
                        TokenKind::Symbol(Symbol::Gt)
                    }
                }
                '^' => {
                    if line_cursor.peek_is('=') {
                        line_cursor.advance();
                        TokenKind::Symbol(Symbol::CaretEquals)
                    } else {
                        TokenKind::Symbol(Symbol::Caret)
                    }
                }
                '?' => {
                    if line_cursor.peek_is('?') {
                        line_cursor.advance();
                        TokenKind::Symbol(Symbol::QuestionQuestion)
                    } else {
                        TokenKind::Symbol(Symbol::Question)
                    }
                }
                '@' => TokenKind::Symbol(Symbol::At),
                '$' => TokenKind::Symbol(Symbol::Dollar),
                _ => {
                    return Err(RatError::error(format!(
                        "Unexpected character `{ch}` on line {current_line}"
                    )))
                }
            };

            let token_end = line_cursor.offset;
            let span = file.span(absolute_start, line_indent_offset + token_end);
            tokens.push(Token::new(token, span));
        }

        if nesting_level == 0 {
            let newline_start = line_start + end_idx;
            let newline_end = (newline_start + 1).min(file.text.len());
            let newline_span = file.span(newline_start, newline_end);
            tokens.push(Token::new(TokenKind::Newline, newline_span));
        }

        cursor.offset = line_start + end_idx;
        if cursor.offset < file.text.len() && bytes[cursor.offset] == b'\n' {
            cursor.offset += 1;
        }
        current_line += 1;
    }

    while indent_stack.depth() > 1 {
        indent_stack.pop();
        let span = file.span(file.text.len(), file.text.len());
        tokens.push(Token::new(TokenKind::Dedent, span));
    }

    let eof_span = file.span(file.text.len(), file.text.len());
    tokens.push(Token::new(TokenKind::Eof, eof_span));

    Ok(tokens)
}
