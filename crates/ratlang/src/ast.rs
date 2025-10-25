use crate::position::Span;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Program {
    pub module: Option<ModuleDecl>,
    pub imports: Vec<ImportDecl>,
    pub items: Vec<Item>,
}

impl Program {
    pub fn new(module: Option<ModuleDecl>, imports: Vec<ImportDecl>, items: Vec<Item>) -> Self {
        Self { module, imports, items }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleDecl {
    pub name: Vec<SmolStr>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImportDecl {
    pub path: Vec<SmolStr>,
    pub alias: Option<SmolStr>,
    pub items: Vec<ImportItem>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ImportItem {
    Wildcard(Span),
    Item { name: SmolStr, alias: Option<SmolStr>, span: Span },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Item {
    Function(FunctionDecl),
    Struct(StructDecl),
    Class(ClassDecl),
    Enum(EnumDecl),
    Trait(TraitDecl),
    Impl(ImplDecl),
    Constant(ConstDecl),
    Statement(Stmt),
    Test(TestCase),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstDecl {
    pub name: SmolStr,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionDecl {
    pub name: SmolStr,
    pub generics: Vec<TypeParameter>,
    pub params: Vec<Parameter>,
    pub return_type: Option<TypeExpr>,
    pub body: Block,
    pub asyncness: Asyncness,
    pub visibility: Visibility,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructDecl {
    pub name: SmolStr,
    pub fields: Vec<StructField>,
    pub visibility: Visibility,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructField {
    pub name: SmolStr,
    pub ty: TypeExpr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClassDecl {
    pub name: SmolStr,
    pub fields: Vec<StructField>,
    pub methods: Vec<FunctionDecl>,
    pub visibility: Visibility,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnumDecl {
    pub name: SmolStr,
    pub variants: Vec<EnumVariant>,
    pub visibility: Visibility,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnumVariant {
    pub name: SmolStr,
    pub fields: EnumVariantKind,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EnumVariantKind {
    Unit,
    Tuple(Vec<TypeExpr>),
    Struct(Vec<StructField>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraitDecl {
    pub name: SmolStr,
    pub methods: Vec<FunctionSignature>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionSignature {
    pub name: SmolStr,
    pub generics: Vec<TypeParameter>,
    pub params: Vec<Parameter>,
    pub return_type: Option<TypeExpr>,
    pub asyncness: Asyncness,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImplDecl {
    pub target: TypeExpr,
    pub trait_ref: Option<TypeExpr>,
    pub methods: Vec<FunctionDecl>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestCase {
    pub name: SmolStr,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeParameter {
    pub name: SmolStr,
    pub bounds: Vec<TypeExpr>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Parameter {
    pub name: SmolStr,
    pub ty: Option<TypeExpr>,
    pub default: Option<Expr>,
    pub span: Span,
    pub pass_by: PassBy,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PassBy {
    Value,
    Shared,
    Borrowed,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub span: Span,
}

impl Block {
    pub fn new(statements: Vec<Stmt>, span: Span) -> Self {
        Self { statements, span }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Stmt {
    Let(LetStmt),
    Var(VarStmt),
    Assign(AssignStmt),
    Expr(Expr),
    Return(ReturnStmt),
    Break(Span),
    Continue(Span),
    Defer(Expr, Span),
    While(WhileStmt),
    For(ForStmt),
    Try(TryStmt),
    WithDefer(Block),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LetStmt {
    pub name: Pattern,
    pub ty: Option<TypeExpr>,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VarStmt {
    pub name: Pattern,
    pub ty: Option<TypeExpr>,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AssignStmt {
    pub target: Expr,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForStmt {
    pub binder: Pattern,
    pub iterable: Expr,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TryStmt {
    pub body: Block,
    pub handlers: Vec<CatchBlock>,
    pub finally: Option<Block>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CatchBlock {
    pub pattern: Option<Pattern>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Pattern {
    Identifier(SmolStr, Span),
    Tuple(Vec<Pattern>, Span),
    Struct { path: Vec<SmolStr>, fields: Vec<(SmolStr, Pattern)>, span: Span },
    Enum { path: Vec<SmolStr>, args: Vec<Pattern>, span: Span },
    Wildcard(Span),
    Literal(Literal, Span),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Literal {
    Int(i128),
    Float(f64),
    Bool(bool),
    String(String),
    None,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expr {
    Literal(Literal, Span),
    Identifier(SmolStr, Span),
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
    Call(Box<CallExpr>),
    Field(Box<FieldExpr>),
    Index(Box<IndexExpr>),
    If(Box<IfExpr>),
    Match(Box<MatchExpr>),
    List(ListExpr),
    Tuple(TupleExpr),
    Set(SetExpr),
    Dict(DictExpr),
    Lambda(LambdaExpr),
    Await(Box<AwaitExpr>),
    Spawn(Box<SpawnExpr>),
    AsyncBlock(Box<BlockExpr>),
    Block(Block),
    Assign(Box<AssignExpr>),
    Pipe(Box<PipeExpr>),
    Range(Box<RangeExpr>),
    Result(ResultExpr),
    Option(OptionExpr),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub left: Expr,
    pub right: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CallExpr {
    pub function: Expr,
    pub args: Vec<Expr>,
    pub kwargs: Vec<(SmolStr, Expr)>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldExpr {
    pub target: Expr,
    pub field: SmolStr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IndexExpr {
    pub target: Expr,
    pub index: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IfExpr {
    pub condition: Expr,
    pub then_branch: Block,
    pub else_branch: Option<Block>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MatchExpr {
    pub scrutinee: Expr,
    pub arms: Vec<MatchArm>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListExpr {
    pub elements: Vec<Expr>,
    pub spans: Vec<Span>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TupleExpr {
    pub elements: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SetExpr {
    pub elements: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DictExpr {
    pub entries: Vec<(Expr, Expr)>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LambdaExpr {
    pub params: Vec<Parameter>,
    pub body: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AwaitExpr {
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpawnExpr {
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlockExpr {
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AssignExpr {
    pub target: Expr,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PipeExpr {
    pub input: Expr,
    pub function: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RangeExpr {
    pub start: Expr,
    pub end: Expr,
    pub inclusive: bool,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResultExpr {
    pub ok: Option<Box<Expr>>,
    pub err: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OptionExpr {
    pub some: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AwaitableType;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Eq,
    NotEq,
    Lt,
    Lte,
    Gt,
    Gte,
    Pipe,
    NullCoalesce,
    Range,
    RangeInclusive,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
    Neg,
    Not,
    BitNot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Asyncness {
    Sync,
    Async,
}

impl Asyncness {
    pub fn is_async(&self) -> bool {
        matches!(self, Asyncness::Async)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Visibility {
    Public,
    Module,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypeExpr {
    Simple(Vec<SmolStr>, Span),
    Optional(Box<TypeExpr>, Span),
    Tuple(Vec<TypeExpr>, Span),
    List(Box<TypeExpr>, Span),
    Set(Box<TypeExpr>, Span),
    Dict(Box<TypeExpr>, Box<TypeExpr>, Span),
    Function { params: Vec<TypeExpr>, ret: Box<TypeExpr>, span: Span },
    Generic { base: Vec<SmolStr>, args: Vec<TypeExpr>, span: Span },
    SelfType(Span),
}

impl TypeExpr {
    pub fn span(&self) -> Span {
        match self {
            TypeExpr::Simple(_, span)
            | TypeExpr::Optional(_, span)
            | TypeExpr::Tuple(_, span)
            | TypeExpr::List(_, span)
            | TypeExpr::Set(_, span)
            | TypeExpr::Dict(_, _, span)
            | TypeExpr::Function { span, .. }
            | TypeExpr::Generic { span, .. }
            | TypeExpr::SelfType(span) => *span,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeAnnotation {
    pub expr: TypeExpr,
    pub span: Span,
}
