use crate::position::Span;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Keyword {
    Module,
    Import,
    From,
    As,
    Pub,
    Let,
    Var,
    Const,
    Share,
    Borrow,
    Fn,
    Async,
    Await,
    Class,
    Struct,
    Enum,
    Trait,
    Impl,
    For,
    In,
    While,
    If,
    Elif,
    Else,
    Match,
    Return,
    Break,
    Continue,
    Defer,
    Try,
    Except,
    Raise,
    Spawn,
    Select,
    Channel,
    Test,
    Init,
    Extends,
    True,
    False,
    None,
}

impl Keyword {
    pub fn from_ident(ident: &str) -> Option<Self> {
        let kw = match ident {
            "module" => Keyword::Module,
            "import" => Keyword::Import,
            "from" => Keyword::From,
            "as" => Keyword::As,
            "pub" => Keyword::Pub,
            "let" => Keyword::Let,
            "var" => Keyword::Var,
            "const" => Keyword::Const,
            "share" => Keyword::Share,
            "borrow" => Keyword::Borrow,
            "fn" => Keyword::Fn,
            "async" => Keyword::Async,
            "await" => Keyword::Await,
            "class" => Keyword::Class,
            "struct" => Keyword::Struct,
            "enum" => Keyword::Enum,
            "trait" => Keyword::Trait,
            "impl" => Keyword::Impl,
            "for" => Keyword::For,
            "in" => Keyword::In,
            "while" => Keyword::While,
            "if" => Keyword::If,
            "elif" => Keyword::Elif,
            "else" => Keyword::Else,
            "match" => Keyword::Match,
            "return" => Keyword::Return,
            "break" => Keyword::Break,
            "continue" => Keyword::Continue,
            "defer" => Keyword::Defer,
            "try" => Keyword::Try,
            "except" => Keyword::Except,
            "raise" => Keyword::Raise,
            "spawn" => Keyword::Spawn,
            "select" => Keyword::Select,
            "channel" => Keyword::Channel,
            "test" => Keyword::Test,
            "init" => Keyword::Init,
            "extends" => Keyword::Extends,
            "true" => Keyword::True,
            "false" => Keyword::False,
            "none" => Keyword::None,
            _ => return None,
        };
        Some(kw)
    }

    pub fn as_str(&self) -> &'static str {
        use Keyword::*;
        match self {
            Module => "module",
            Import => "import",
            From => "from",
            As => "as",
            Pub => "pub",
            Let => "let",
            Var => "var",
            Const => "const",
            Share => "share",
            Borrow => "borrow",
            Fn => "fn",
            Async => "async",
            Await => "await",
            Class => "class",
            Struct => "struct",
            Enum => "enum",
            Trait => "trait",
            Impl => "impl",
            For => "for",
            In => "in",
            While => "while",
            If => "if",
            Elif => "elif",
            Else => "else",
            Match => "match",
            Return => "return",
            Break => "break",
            Continue => "continue",
            Defer => "defer",
            Try => "try",
            Except => "except",
            Raise => "raise",
            Spawn => "spawn",
            Select => "select",
            Channel => "channel",
            Test => "test",
            Init => "init",
            Extends => "extends",
            True => "true",
            False => "false",
            None => "none",
        }
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Symbol {
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Dot,
    Colon,
    DoubleColon,
    Arrow,
    ThickArrow,
    FatArrow,
    Equals,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Pipe,
    PipePipe,
    Amp,
    AmpAmp,
    Caret,
    Bang,
    Question,
    QuestionQuestion,
    Lt,
    Le,
    Gt,
    Ge,
    EqEq,
    NotEq,
    Ellipsis,
    Range,
    RangeInclusive,
    At,
    Dollar,
    PipeGreater,
    PlusEquals,
    MinusEquals,
    StarEquals,
    SlashEquals,
    PercentEquals,
    AmpEquals,
    PipeEquals,
    CaretEquals,
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Symbol::*;
        let s = match self {
            LParen => "(",
            RParen => ")",
            LBrace => "{",
            RBrace => "}",
            LBracket => "[",
            RBracket => "]",
            Comma => ",",
            Dot => ".",
            Colon => ":",
            DoubleColon => "::",
            Arrow => "->",
            ThickArrow => "=>",
            FatArrow => "=>",
            Equals => "=",
            Plus => "+",
            Minus => "-",
            Star => "*",
            Slash => "/",
            Percent => "%",
            Pipe => "|",
            PipePipe => "||",
            Amp => "&",
            AmpAmp => "&&",
            Caret => "^",
            Bang => "!",
            Question => "?",
            QuestionQuestion => "??",
            Lt => "<",
            Le => "<=",
            Gt => ">",
            Ge => ">=",
            EqEq => "==",
            NotEq => "!=",
            Ellipsis => "...",
            Range => "..",
            RangeInclusive => "..=",
            At => "@",
            Dollar => "$",
            PipeGreater => "|>",
            PlusEquals => "+=",
            MinusEquals => "-=",
            StarEquals => "*=",
            SlashEquals => "/=",
            PercentEquals => "%=",
            AmpEquals => "&=",
            PipeEquals => "|=",
            CaretEquals => "^=",
        };
        write!(f, "{s}")
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum TokenKind {
    Identifier(SmolStr),
    IntLiteral(i128),
    FloatLiteral(f64),
    BoolLiteral(bool),
    StringLiteral(String),
    DocComment(String),
    Keyword(Keyword),
    Symbol(Symbol),
    Newline,
    Indent,
    Dedent,
    Eof,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}
