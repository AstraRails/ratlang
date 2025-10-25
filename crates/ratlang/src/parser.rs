use crate::ast::*;
use crate::diagnostics::{RatError, RatResult};
use crate::position::{DUMMY_SPAN, Span};
use crate::tokens::{Keyword, Symbol, Token, TokenKind};
use smol_str::SmolStr;

pub struct Parser<'a> {
    tokens: &'a [Token],
    index: usize,
    doc_buffer: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            index: 0,
            doc_buffer: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> RatResult<Program> {
        let mut module = None;
        let mut imports = Vec::new();
        let mut items = Vec::new();

        self.skip_newlines();

        if self.peek_keyword(Keyword::Module) {
            module = Some(self.parse_module_decl()?);
        }

        while !self.is_eof() {
            self.skip_newlines();
            if self.is_eof() {
                break;
            }

            if self.peek_token(|t| matches!(t.kind, TokenKind::DocComment(_))) {
                if let TokenKind::DocComment(text) = self.current().kind.clone() {
                    self.doc_buffer.push(text.trim().to_string());
                }
                self.advance();
                self.eat_newlines();
                continue;
            }

            if self.peek_keyword(Keyword::Import) || self.peek_keyword(Keyword::From) {
                let import = self.parse_import_decl()?;
                imports.push(import);
                self.skip_newlines();
                continue;
            }

            let item = if self.peek_keyword(Keyword::Const) {
                Item::Constant(self.parse_const_decl()?)
            } else if self.peek_keyword(Keyword::Test) {
                Item::Test(self.parse_test_case()?)
            } else {
                self.parse_item()?
            };
            items.push(item);
            self.skip_newlines();
        }

        Ok(Program::new(module, imports, items))
    }

    fn parse_module_decl(&mut self) -> RatResult<ModuleDecl> {
        let module_token = self.expect_keyword(Keyword::Module)?;
        let mut components = Vec::new();
        let start_span = module_token.span;
        loop {
            let ident = self.expect_identifier("module path segment")?;
            components.push(ident);
            if self.eat_symbol(Symbol::Dot) {
                continue;
            }
            break;
        }
        self.expect_newline("expected newline after module declaration")?;
        Ok(ModuleDecl {
            name: components,
            span: start_span,
        })
    }

    fn parse_import_decl(&mut self) -> RatResult<ImportDecl> {
        if self.eat_keyword(Keyword::Import) {
            let path = self.parse_path()?;
            let alias = if self.eat_keyword(Keyword::As) {
                Some(self.expect_identifier("import alias")?)
            } else {
                None
            };
            self.expect_newline("expected newline after import")?;
            return Ok(ImportDecl {
                path,
                alias,
                items: Vec::new(),
                span: self.prev_span(),
            });
        }

        self.expect_keyword(Keyword::From)?;
        let module_path = self.parse_path()?;
        self.expect_keyword(Keyword::Import)?;
        let mut items = Vec::new();
        if self.eat_symbol(Symbol::Star) {
            items.push(ImportItem::Wildcard(self.prev_span()));
        } else {
            loop {
                let name = self.expect_identifier("import item")?;
                let alias = if self.eat_keyword(Keyword::As) {
                    Some(self.expect_identifier("import alias")?)
                } else {
                    None
                };
                items.push(ImportItem::Item {
                    name,
                    alias,
                    span: self.prev_span(),
                });
                if !self.eat_symbol(Symbol::Comma) {
                    break;
                }
            }
        }
        self.expect_newline("expected newline after import statement")?;
        Ok(ImportDecl {
            path: module_path,
            alias: None,
            items,
            span: self.prev_span(),
        })
    }

    fn parse_const_decl(&mut self) -> RatResult<ConstDecl> {
        let start = self.expect_keyword(Keyword::Const)?.span;
        let name = self.expect_identifier("constant name")?;
        self.expect_symbol(Symbol::Equals, "expected '=' in const declaration")?;
        let value = self.parse_expression()?;
        self.expect_newline("expected newline after const declaration")?;
        Ok(ConstDecl {
            name,
            value,
            span: start,
        })
    }

    fn parse_test_case(&mut self) -> RatResult<TestCase> {
        let start = self.expect_keyword(Keyword::Test)?.span;
        let name_lit = self.expect_string_literal("test name must be a string literal")?;
        let body = self.parse_block()?;
        Ok(TestCase {
            name: SmolStr::from(name_lit),
            body,
            span: start,
        })
    }

    fn parse_item(&mut self) -> RatResult<Item> {
        let visibility = if self.eat_keyword(Keyword::Pub) {
            Visibility::Public
        } else {
            Visibility::Module
        };

        if self.peek_keyword(Keyword::Async) && self.peek_keyword_n(Keyword::Fn, 1) {
            self.expect_keyword(Keyword::Async)?;
            let mut func = self.parse_function_decl(Some(Asyncness::Async))?;
            func.visibility = visibility;
            return Ok(Item::Function(func));
        }

        if self.peek_keyword(Keyword::Fn) {
            let mut func = self.parse_function_decl(None)?;
            func.visibility = visibility;
            return Ok(Item::Function(func));
        }

        // Fallback: parse statement as top-level item
        let stmt = self.parse_statement()?;
        Ok(Item::Statement(stmt))
    }

    fn parse_function_decl(&mut self, asyncness: Option<Asyncness>) -> RatResult<FunctionDecl> {
        let fn_span = self.expect_keyword(Keyword::Fn)?.span.clone();
        let name = self.expect_identifier("function name")?;
        let generics = Vec::new();
        self.expect_symbol(Symbol::LParen, "expected '(' after function name")?;
        let params = self.parse_parameter_list()?;
        self.expect_symbol(Symbol::RParen, "expected ')' to close parameter list")?;
        let return_type = if self.eat_symbol(Symbol::Arrow) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };
        self.expect_newline("expected newline after function signature")?;
        let body = self.parse_block()?;
        Ok(FunctionDecl {
            name,
            generics,
            params,
            return_type,
            body,
            asyncness: asyncness.unwrap_or(Asyncness::Sync),
            visibility: Visibility::Module,
            span: fn_span,
        })
    }

    fn parse_parameter_list(&mut self) -> RatResult<Vec<Parameter>> {
        let mut params = Vec::new();
        if self.peek_symbol(Symbol::RParen) {
            return Ok(params);
        }
        loop {
            let name = self.expect_identifier("parameter name")?;
            let ty = if self.eat_symbol(Symbol::Colon) {
                Some(self.parse_type_expr()?)
            } else {
                None
            };
            let default = if self.eat_symbol(Symbol::Equals) {
                Some(self.parse_expression()?)
            } else {
                None
            };
            let span = self.prev_span();
            params.push(Parameter {
                name,
                ty,
                default,
                span,
                pass_by: PassBy::Value,
            });
            if !self.eat_symbol(Symbol::Comma) {
                break;
            }
        }
        Ok(params)
    }

    fn parse_block(&mut self) -> RatResult<Block> {
        if self.peek_token(|t| !matches!(t.kind, TokenKind::Indent)) {
            self.expect_newline("expected newline before block")?;
        }
        self.expect_token(|t| matches!(t.kind, TokenKind::Indent), "expected indent to start block")?;
        let indent_span = self.prev_span();
        let mut statements = Vec::new();
        self.skip_newlines();
        while !self.is_eof() && !self.peek_token(|t| matches!(t.kind, TokenKind::Dedent)) {
            if self.peek_token(|t| matches!(t.kind, TokenKind::DocComment(_))) {
                if let TokenKind::DocComment(text) = self.current().kind.clone() {
                    self.doc_buffer.push(text.trim().to_string());
                }
                self.advance();
                self.skip_newlines();
                continue;
            }
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.skip_newlines();
        }
        self.expect_token(|t| matches!(t.kind, TokenKind::Dedent), "expected dedent to close block")?;
        Ok(Block::new(statements, indent_span))
    }

    fn parse_statement(&mut self) -> RatResult<Stmt> {
        if self.peek_keyword(Keyword::Let) {
            return self.parse_let_statement();
        }
        if self.peek_keyword(Keyword::Var) {
            return self.parse_var_statement();
        }
        if self.peek_keyword(Keyword::Return) {
            return self.parse_return_statement();
        }
        if self.peek_keyword(Keyword::Break) {
            let span = self.expect_keyword(Keyword::Break)?.span.clone();
            self.expect_newline("expected newline after break")?;
            return Ok(Stmt::Break(span));
        }
        if self.peek_keyword(Keyword::Continue) {
            let span = self.expect_keyword(Keyword::Continue)?.span.clone();
            self.expect_newline("expected newline after continue")?;
            return Ok(Stmt::Continue(span));
        }
        if self.peek_keyword(Keyword::Defer) {
            let span = self.expect_keyword(Keyword::Defer)?.span.clone();
            let expr = self.parse_expression()?;
            self.expect_newline("expected newline after defer expression")?;
            return Ok(Stmt::Defer(expr, span));
        }
        if self.peek_keyword(Keyword::While) {
            return self.parse_while_statement();
        }
        if self.peek_keyword(Keyword::For) {
            return self.parse_for_statement();
        }

        let expr = self.parse_expression()?;
        if matches!(expr, Expr::Block(_) | Expr::If(_) | Expr::Match(_)) {
            return Ok(Stmt::Expr(expr));
        }
        self.expect_newline("expected newline after expression statement")?;
        Ok(Stmt::Expr(expr))
    }

    fn parse_let_statement(&mut self) -> RatResult<Stmt> {
        let start = self.expect_keyword(Keyword::Let)?.span.clone();
        let pattern = Pattern::Identifier(self.expect_identifier("binding name")?, start);
        let ty = if self.eat_symbol(Symbol::Colon) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };
        self.expect_symbol(Symbol::Equals, "expected '=' in let binding")?;
        let value = self.parse_expression()?;
        self.expect_newline("expected newline after let binding")?;
        Ok(Stmt::Let(LetStmt {
            name: pattern,
            ty,
            value,
            span: start,
        }))
    }

    fn parse_var_statement(&mut self) -> RatResult<Stmt> {
        let start = self.expect_keyword(Keyword::Var)?.span.clone();
        let pattern = Pattern::Identifier(self.expect_identifier("binding name")?, start);
        let ty = if self.eat_symbol(Symbol::Colon) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };
        self.expect_symbol(Symbol::Equals, "expected '=' in var binding")?;
        let value = self.parse_expression()?;
        self.expect_newline("expected newline after var binding")?;
        Ok(Stmt::Var(VarStmt {
            name: pattern,
            ty,
            value,
            span: start,
        }))
    }

    fn parse_return_statement(&mut self) -> RatResult<Stmt> {
        let start = self.expect_keyword(Keyword::Return)?.span.clone();
        if self.peek_token(|t| matches!(t.kind, TokenKind::Newline | TokenKind::Dedent)) {
            self.expect_newline("expected newline after return")?;
            return Ok(Stmt::Return(ReturnStmt { value: None, span: start }));
        }
        let value = self.parse_expression()?;
        self.expect_newline("expected newline after return expression")?;
        Ok(Stmt::Return(ReturnStmt {
            value: Some(value),
            span: start,
        }))
    }

    fn parse_while_statement(&mut self) -> RatResult<Stmt> {
        let start = self.expect_keyword(Keyword::While)?.span.clone();
        let condition = self.parse_expression()?;
        let body = self.parse_block()?;
        Ok(Stmt::While(WhileStmt {
            condition,
            body,
            span: start,
        }))
    }

    fn parse_for_statement(&mut self) -> RatResult<Stmt> {
        let start = self.expect_keyword(Keyword::For)?.span.clone();
        let binder = Pattern::Identifier(self.expect_identifier("loop variable")?, start);
        self.expect_keyword(Keyword::In)?;
        let iterable = self.parse_expression()?;
        let body = self.parse_block()?;
        Ok(Stmt::For(ForStmt {
            binder,
            iterable,
            body,
            span: start,
        }))
    }

    fn parse_expression(&mut self) -> RatResult<Expr> {
        self.parse_binary_expr(0)
    }

    fn parse_binary_expr(&mut self, min_prec: u8) -> RatResult<Expr> {
        let mut left = self.parse_unary_expr()?;

        loop {
            let (op, prec, right_assoc) = match self.current().kind {
                TokenKind::Symbol(Symbol::PipePipe) => (BinaryOp::Or, 1, false),
                TokenKind::Symbol(Symbol::AmpAmp) => (BinaryOp::And, 2, false),
                TokenKind::Symbol(Symbol::EqEq) => (BinaryOp::Eq, 3, false),
                TokenKind::Symbol(Symbol::NotEq) => (BinaryOp::NotEq, 3, false),
                TokenKind::Symbol(Symbol::Lt) => (BinaryOp::Lt, 4, false),
                TokenKind::Symbol(Symbol::Le) => (BinaryOp::Lte, 4, false),
                TokenKind::Symbol(Symbol::Gt) => (BinaryOp::Gt, 4, false),
                TokenKind::Symbol(Symbol::Ge) => (BinaryOp::Gte, 4, false),
                TokenKind::Symbol(Symbol::Plus) => (BinaryOp::Add, 5, false),
                TokenKind::Symbol(Symbol::Minus) => (BinaryOp::Sub, 5, false),
                TokenKind::Symbol(Symbol::Star) => (BinaryOp::Mul, 6, false),
                TokenKind::Symbol(Symbol::Slash) => (BinaryOp::Div, 6, false),
                TokenKind::Symbol(Symbol::Percent) => (BinaryOp::Mod, 6, false),
                TokenKind::Symbol(Symbol::PipeGreater) => (BinaryOp::Pipe, 7, false),
                TokenKind::Symbol(Symbol::QuestionQuestion) => (BinaryOp::NullCoalesce, 1, true),
                TokenKind::Symbol(Symbol::Range) => (BinaryOp::Range, 6, false),
                TokenKind::Symbol(Symbol::RangeInclusive) => (BinaryOp::RangeInclusive, 6, false),
                _ => break,
            };

            if prec < min_prec {
                break;
            }

            self.advance();
            let next_min_prec = if right_assoc { prec } else { prec + 1 };
            let right = self.parse_binary_expr(next_min_prec)?;
            let span = self.prev_span();
            left = Expr::Binary(Box::new(BinaryExpr {
                op,
                left,
                right,
                span,
            }));
        }

        Ok(left)
    }

    fn parse_unary_expr(&mut self) -> RatResult<Expr> {
        if self.eat_symbol(Symbol::Minus) {
            let span = self.prev_span();
            let expr = self.parse_unary_expr()?;
            return Ok(Expr::Unary(Box::new(UnaryExpr {
                op: UnaryOp::Neg,
                expr,
                span,
            })));
        }
        if self.eat_symbol(Symbol::Bang) {
            let span = self.prev_span();
            let expr = self.parse_unary_expr()?;
            return Ok(Expr::Unary(Box::new(UnaryExpr {
                op: UnaryOp::Not,
                expr,
                span,
            })));
        }
        if self.peek_keyword(Keyword::Await) {
            let span = self.expect_keyword(Keyword::Await)?.span;
            let expr = self.parse_unary_expr()?;
            return Ok(Expr::Await(Box::new(AwaitExpr { expr, span })));
        }
        if self.peek_keyword(Keyword::Spawn) {
            let span = self.expect_keyword(Keyword::Spawn)?.span;
            let expr = self.parse_unary_expr()?;
            return Ok(Expr::Spawn(Box::new(SpawnExpr { expr, span })));
        }
        self.parse_postfix_expr()
    }

    fn parse_postfix_expr(&mut self) -> RatResult<Expr> {
        let mut expr = self.parse_primary_expr()?;
        loop {
            if self.eat_symbol(Symbol::LParen) {
                let args = self.parse_argument_list()?;
                self.expect_symbol(Symbol::RParen, "expected ')' after arguments")?;
                let span = self.prev_span();
                expr = Expr::Call(Box::new(CallExpr {
                    function: expr,
                    args,
                    kwargs: Vec::new(),
                    span,
                }));
                continue;
            }
            if self.eat_symbol(Symbol::Dot) {
                let field = self.expect_identifier("field name")?;
                let span = self.prev_span();
                expr = Expr::Field(Box::new(FieldExpr { target: expr, field, span }));
                continue;
            }
            if self.eat_symbol(Symbol::LBracket) {
                let index = self.parse_expression()?;
                self.expect_symbol(Symbol::RBracket, "expected ']' after index expression")?;
                let span = self.prev_span();
                expr = Expr::Index(Box::new(IndexExpr { target: expr, index, span }));
                continue;
            }
            break;
        }
        Ok(expr)
    }

    fn parse_primary_expr(&mut self) -> RatResult<Expr> {
        let token = self.current().clone();
        match &token.kind {
            TokenKind::BoolLiteral(value) => {
                self.advance();
                Ok(Expr::Literal(Literal::Bool(*value), token.span))
            }
            TokenKind::IntLiteral(value) => {
                self.advance();
                Ok(Expr::Literal(Literal::Int(*value), token.span))
            }
            TokenKind::FloatLiteral(value) => {
                self.advance();
                Ok(Expr::Literal(Literal::Float(*value), token.span))
            }
            TokenKind::StringLiteral(value) => {
                self.advance();
                Ok(Expr::Literal(Literal::String(value.clone()), token.span))
            }
            TokenKind::Keyword(Keyword::None) => {
                self.advance();
                Ok(Expr::Literal(Literal::None, token.span))
            }
            TokenKind::Identifier(name) => {
                self.advance();
                Ok(Expr::Identifier(name.clone(), token.span))
            }
            TokenKind::Symbol(Symbol::LParen) => {
                self.advance();
                if self.eat_symbol(Symbol::RParen) {
                    return Ok(Expr::Tuple(TupleExpr {
                        elements: Vec::new(),
                        span: token.span,
                    }));
                }
                let mut elements = Vec::new();
                elements.push(self.parse_expression()?);
                if self.eat_symbol(Symbol::Comma) {
                    while !self.peek_symbol(Symbol::RParen) {
                        elements.push(self.parse_expression()?);
                        if !self.eat_symbol(Symbol::Comma) {
                            break;
                        }
                    }
                }
                self.expect_symbol(Symbol::RParen, "expected ')' to close tuple")?;
                Ok(if elements.len() == 1 {
                    elements.into_iter().next().unwrap()
                } else {
                    Expr::Tuple(TupleExpr {
                        elements,
                        span: token.span,
                    })
                })
            }
            TokenKind::Symbol(Symbol::LBracket) => {
                self.advance();
                let mut elements = Vec::new();
                if !self.peek_symbol(Symbol::RBracket) {
                    loop {
                        elements.push(self.parse_expression()?);
                        if !self.eat_symbol(Symbol::Comma) {
                            break;
                        }
                    }
                }
                self.expect_symbol(Symbol::RBracket, "expected ']' to close list literal")?;
                Ok(Expr::List(ListExpr {
                    elements,
                    spans: Vec::new(),
                    span: token.span,
                }))
            }
            TokenKind::Symbol(Symbol::LBrace) => {
                self.advance();
                let mut entries = Vec::new();
                if !self.peek_symbol(Symbol::RBrace) {
                    loop {
                        let key = self.parse_expression()?;
                        self.expect_symbol(Symbol::Colon, "expected ':' between dict key and value")?;
                        let value = self.parse_expression()?;
                        entries.push((key, value));
                        if !self.eat_symbol(Symbol::Comma) {
                            break;
                        }
                    }
                }
                self.expect_symbol(Symbol::RBrace, "expected '}' to close dict literal")?;
                Ok(Expr::Dict(DictExpr { entries, span: token.span }))
            }
            TokenKind::Symbol(Symbol::Pipe) => self.parse_lambda_expr(),
            TokenKind::Keyword(Keyword::If) => self.parse_if_expression(),
            TokenKind::Keyword(Keyword::Match) => self.parse_match_expression(),
            _ => Err(self.error_here("unexpected token in expression")),
        }
    }

    fn parse_lambda_expr(&mut self) -> RatResult<Expr> {
        let pipe_span = self.expect_symbol(Symbol::Pipe, "expected '|' to start lambda")?.span;
        let mut params = Vec::new();
        if !self.peek_symbol(Symbol::Pipe) {
            loop {
                let name = self.expect_identifier("lambda parameter")?;
                params.push(Parameter {
                    name,
                    ty: None,
                    default: None,
                    span: pipe_span,
                    pass_by: PassBy::Value,
                });
                if !self.eat_symbol(Symbol::Comma) {
                    break;
                }
            }
        }
        self.expect_symbol(Symbol::Pipe, "expected closing '|' in lambda")?;
        let body = self.parse_expression()?;
        Ok(Expr::Lambda(LambdaExpr {
            params,
            body: Box::new(body),
            span: pipe_span,
        }))
    }

    fn parse_if_expression(&mut self) -> RatResult<Expr> {
        let start = self.expect_keyword(Keyword::If)?.span;
        let condition = self.parse_expression()?;
        let then_block = self.parse_block()?;
        let mut else_block = None;
        if self.peek_keyword(Keyword::Elif) {
            let elif_expr = self.parse_if_expression()?;
            else_block = Some(Block::new(vec![Stmt::Expr(elif_expr)], start));
        } else if self.peek_keyword(Keyword::Else) {
            self.expect_keyword(Keyword::Else)?;
            else_block = Some(self.parse_block()?);
        }
        Ok(Expr::If(Box::new(IfExpr {
            condition,
            then_branch: then_block,
            else_branch: else_block,
            span: start,
        })))
    }

    fn parse_match_expression(&mut self) -> RatResult<Expr> {
        let start = self.expect_keyword(Keyword::Match)?.span;
        let scrutinee = self.parse_expression()?;
        self.expect_newline("expected newline after match scrutinee")?;
        self.expect_token(|t| matches!(t.kind, TokenKind::Indent), "expected indent after match")?;
        let mut arms = Vec::new();
        self.skip_newlines();
        while !self.is_eof() && !self.peek_token(|t| matches!(t.kind, TokenKind::Dedent)) {
            let pattern = self.parse_pattern()?;
            let guard = if self.peek_keyword(Keyword::If) {
                self.expect_keyword(Keyword::If)?;
                Some(self.parse_expression()?)
            } else {
                None
            };
            self.expect_symbol(Symbol::Colon, "expected ':' after match arm")?;
            let body = self.parse_block()?;
            arms.push(MatchArm {
                pattern,
                guard,
                body,
                span: start,
            });
            self.skip_newlines();
        }
        self.expect_token(|t| matches!(t.kind, TokenKind::Dedent), "expected dedent after match arms")?;
        Ok(Expr::Match(Box::new(MatchExpr {
            scrutinee,
            arms,
            span: start,
        })))
    }

    fn parse_pattern(&mut self) -> RatResult<Pattern> {
        if let TokenKind::Identifier(name) = self.current().kind.clone() {
            self.advance();
            if name.as_str() == "_" {
                return Ok(Pattern::Wildcard(self.prev_span()));
            }
            return Ok(Pattern::Identifier(name, self.prev_span()));
        }
        Err(self.error_here("expected pattern"))
    }

    fn parse_argument_list(&mut self) -> RatResult<Vec<Expr>> {
        let mut args = Vec::new();
        if self.peek_symbol(Symbol::RParen) {
            return Ok(args);
        }
        loop {
            args.push(self.parse_expression()?);
            if !self.eat_symbol(Symbol::Comma) {
                break;
            }
        }
        Ok(args)
    }

    fn parse_type_expr(&mut self) -> RatResult<TypeExpr> {
        let span = self.current().span;
        let mut path = Vec::new();
        let ident = self.expect_identifier("type name")?;
        path.push(ident);
        while self.eat_symbol(Symbol::DoubleColon) {
            let ident = self.expect_identifier("type path segment")?;
            path.push(ident);
        }
        let mut ty = TypeExpr::Simple(path, span);
        if self.eat_symbol(Symbol::LBracket) {
            let mut args = Vec::new();
            if !self.peek_symbol(Symbol::RBracket) {
                loop {
                    args.push(self.parse_type_expr()?);
                    if !self.eat_symbol(Symbol::Comma) {
                        break;
                    }
                }
            }
            self.expect_symbol(Symbol::RBracket, "expected ']' after type arguments")?;
            let base = match ty {
                TypeExpr::Simple(path, _span) => path,
                _ => Vec::new(),
            };
            ty = TypeExpr::Generic {
                base,
                args,
                span,
            };
        }
        if self.eat_symbol(Symbol::Question) {
            ty = TypeExpr::Optional(Box::new(ty), span);
        }
        Ok(ty)
    }

    fn parse_path(&mut self) -> RatResult<Vec<SmolStr>> {
        let mut parts = Vec::new();
        parts.push(self.expect_identifier("identifier")?);
        while self.eat_symbol(Symbol::Dot) {
            parts.push(self.expect_identifier("identifier")?);
        }
        Ok(parts)
    }

    fn peek_token<F>(&self, predicate: F) -> bool
    where
        F: FnOnce(&Token) -> bool,
    {
        if self.index >= self.tokens.len() {
            return false;
        }
        predicate(&self.tokens[self.index])
    }

    fn peek_keyword(&self, keyword: Keyword) -> bool {
        self.peek_token(|t| matches!(t.kind, TokenKind::Keyword(k) if k == keyword))
    }

    fn peek_keyword_n(&self, keyword: Keyword, n: usize) -> bool {
        self.tokens
            .get(self.index + n)
            .map_or(false, |t| matches!(t.kind, TokenKind::Keyword(k) if k == keyword))
    }

    fn peek_symbol(&self, symbol: Symbol) -> bool {
        self.peek_token(|t| matches!(t.kind, TokenKind::Symbol(s) if s == symbol))
    }

    fn expect_keyword(&mut self, keyword: Keyword) -> RatResult<&Token> {
        if self.peek_keyword(keyword) {
            Ok(self.advance())
        } else {
            Err(self.error_here(format!("expected keyword `{}`", keyword)))
        }
    }

    fn eat_keyword(&mut self, keyword: Keyword) -> bool {
        if self.peek_keyword(keyword) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect_symbol(&mut self, symbol: Symbol, message: &str) -> RatResult<&Token> {
        if self.peek_symbol(symbol) {
            Ok(self.advance())
        } else {
            Err(self.error_here(message))
        }
    }

    fn eat_symbol(&mut self, symbol: Symbol) -> bool {
        if self.peek_symbol(symbol) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect_identifier(&mut self, what: &str) -> RatResult<SmolStr> {
        if let Some(token) = self.tokens.get(self.index) {
            if let TokenKind::Identifier(name) = &token.kind {
                self.advance();
                return Ok(name.clone());
            }
        }
        Err(self.error_here(format!("expected {what}")))
    }

    fn expect_string_literal(&mut self, message: &str) -> RatResult<String> {
        if let Some(token) = self.tokens.get(self.index) {
            if let TokenKind::StringLiteral(value) = &token.kind {
                self.advance();
                return Ok(value.clone());
            }
        }
        Err(self.error_here(message))
    }

    fn expect_token<F>(&mut self, predicate: F, message: &str) -> RatResult<&Token>
    where
        F: FnOnce(&Token) -> bool,
    {
        if self.peek_token(predicate) {
            Ok(self.advance())
        } else {
            Err(self.error_here(message))
        }
    }

    fn expect_newline(&mut self, message: &str) -> RatResult<()> {
        if self.eat_newlines() {
            return Ok(());
        }
        Err(self.error_here(message))
    }

    fn eat_newlines(&mut self) -> bool {
        let mut consumed = false;
        while self.peek_token(|t| matches!(t.kind, TokenKind::Newline)) {
            self.advance();
            consumed = true;
        }
        consumed
    }

    fn skip_newlines(&mut self) {
        while self.peek_token(|t| matches!(t.kind, TokenKind::Newline)) {
            self.advance();
        }
    }


    fn is_eof(&self) -> bool {
        matches!(self.current().kind, TokenKind::Eof)
    }

    fn current(&self) -> &Token {
        &self.tokens[self.index]
    }

    fn advance(&mut self) -> &Token {
        let token = &self.tokens[self.index];
        if self.index + 1 < self.tokens.len() {
            self.index += 1;
        }
        token
    }

    fn prev_span(&self) -> Span {
        if self.index == 0 {
            DUMMY_SPAN
        } else {
            self.tokens[self.index - 1].span
        }
    }

    fn error_here(&self, message: impl Into<String>) -> RatError {
        let token = self.current();
        RatError::Diagnostic(
            crate::diagnostics::RatDiagnostic::error(message)
                .with_primary(token.span, Some("here".into())),
        )
    }
}

pub fn parse(tokens: &[Token]) -> RatResult<Program> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}
