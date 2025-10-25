use crate::ast::*;
use crate::diagnostics::{RatError, RatResult};
use crate::position::Span;
use crate::types::{resolve_type_expr, Type, TypeEnvironment, TypeInfo};
use smol_str::SmolStr;

pub struct TypeChecker {
    env: TypeEnvironment,
    info: TypeInfo,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            env: TypeEnvironment::new(),
            info: TypeInfo::default(),
        }
    }

    pub fn check_program(mut self, program: &Program) -> RatResult<TypeInfo> {
        for import in &program.imports {
            for item in &import.items {
                if let ImportItem::Item { name, .. } = item {
                    self.env.insert(name.clone(), Type::Any);
                }
            }
        }
        for item in &program.items {
            self.check_item(item)?;
        }
        Ok(self.info)
    }

    fn check_item(&mut self, item: &Item) -> RatResult<()> {
        match item {
            Item::Constant(const_decl) => {
                let value_ty = self.check_expr(&const_decl.value)?;
                self.env.insert(const_decl.name.clone(), value_ty);
                Ok(())
            }
            Item::Function(func) => {
                let func_type = self.function_type(func);
                self.env.insert(func.name.clone(), func_type.clone());
                self.check_function(func)
            }
            Item::Statement(stmt) => {
                self.check_statement(stmt)
            }
            Item::Struct(_) | Item::Class(_) | Item::Enum(_) | Item::Trait(_) | Item::Impl(_) => {
                // Future work: structural typing. For now, treat as dynamically typed.
                Ok(())
            }
            Item::Test(test_case) => {
                self.env.push_scope();
                self.check_block(&test_case.body)?;
                self.env.pop_scope();
                Ok(())
            }
        }
    }

    fn function_type(&self, func: &FunctionDecl) -> Type {
        let params = func
            .params
            .iter()
            .map(|param| param.ty.as_ref().map(resolve_type_expr).unwrap_or(Type::Any))
            .collect::<Vec<_>>();
        let ret = func
            .return_type
            .as_ref()
            .map(resolve_type_expr)
            .unwrap_or(Type::Any);
        Type::Function(params, Box::new(ret), func.asyncness.clone())
    }

    fn check_function(&mut self, func: &FunctionDecl) -> RatResult<()> {
        self.env.push_scope();
        for param in &func.params {
            let ty = param.ty.as_ref().map(resolve_type_expr).unwrap_or(Type::Any);
            self.env.insert(param.name.clone(), ty);
        }
        self.check_block(&func.body)?;
        self.env.pop_scope();
        Ok(())
    }

    fn check_block(&mut self, block: &Block) -> RatResult<Type> {
        self.env.push_scope();
        let mut last_type = Type::None;
        for stmt in &block.statements {
            let ty = self.check_statement(stmt)?;
            if let Some(ty) = ty {
                last_type = ty;
            }
        }
        self.env.pop_scope();
        Ok(last_type)
    }

    fn check_statement(&mut self, stmt: &Stmt) -> RatResult<Option<Type>> {
        match stmt {
            Stmt::Let(let_stmt) => {
                let value_ty = self.check_expr(&let_stmt.value)?;
                if let Some(type_expr) = &let_stmt.ty {
                    let annotated = resolve_type_expr(type_expr);
                    if !annotated.is_assignable_from(&value_ty) {
                        return Err(RatError::error(format!(
                            "type mismatch: expected {:?}, found {:?}",
                            annotated, value_ty
                        )));
                    }
                    if let Some(name) = self.pattern_name(&let_stmt.name) {
                        self.env.insert(name, annotated);
                    }
                } else if let Some(name) = self.pattern_name(&let_stmt.name) {
                    self.env.insert(name, value_ty.clone());
                }
                Ok(None)
            }
            Stmt::Var(var_stmt) => {
                let value_ty = self.check_expr(&var_stmt.value)?;
                if let Some(type_expr) = &var_stmt.ty {
                    let annotated = resolve_type_expr(type_expr);
                    if !annotated.is_assignable_from(&value_ty) {
                        return Err(RatError::error(format!(
                            "type mismatch: expected {:?}, found {:?}",
                            annotated, value_ty
                        )));
                    }
                    if let Some(name) = self.pattern_name(&var_stmt.name) {
                        self.env.insert(name, annotated);
                    }
                } else if let Some(name) = self.pattern_name(&var_stmt.name) {
                    self.env.insert(name, value_ty.clone());
                }
                Ok(None)
            }
            Stmt::Assign(assign_stmt) => {
                let value_ty = self.check_expr(&assign_stmt.value)?;
                if let Expr::Identifier(name, _) = &assign_stmt.target {
                    self.env.assign(name, value_ty.clone());
                }
                Ok(None)
            }
            Stmt::Expr(expr) => {
                let ty = self.check_expr(expr)?;
                Ok(Some(ty))
            }
            Stmt::Return(return_stmt) => {
                if let Some(expr) = &return_stmt.value {
                    let ty = self.check_expr(expr)?;
                    Ok(Some(ty))
                } else {
                    Ok(Some(Type::None))
                }
            }
            Stmt::Break(_) | Stmt::Continue(_) => Ok(None),
            Stmt::Defer(expr, _) => {
                self.check_expr(expr)?;
                Ok(None)
            }
            Stmt::While(while_stmt) => {
                self.check_expr(&while_stmt.condition)?;
                self.check_block(&while_stmt.body)?;
                Ok(None)
            }
            Stmt::For(for_stmt) => {
                self.check_expr(&for_stmt.iterable)?;
                self.env.push_scope();
                if let Some(name) = self.pattern_name(&for_stmt.binder) {
                    self.env.insert(name, Type::Any);
                }
                self.check_block(&for_stmt.body)?;
                self.env.pop_scope();
                Ok(None)
            }
            Stmt::Try(try_stmt) => {
                self.env.push_scope();
                self.check_block(&try_stmt.body)?;
                for handler in &try_stmt.handlers {
                    self.env.push_scope();
                    if let Some(pattern) = &handler.pattern {
                        if let Some(name) = self.pattern_name(pattern) {
                            self.env.insert(name, Type::Any);
                        }
                    }
                    self.check_block(&handler.body)?;
                    self.env.pop_scope();
                }
                if let Some(finally) = &try_stmt.finally {
                    self.check_block(finally)?;
                }
                self.env.pop_scope();
                Ok(None)
            }
            Stmt::WithDefer(block) => {
                self.check_block(block)?;
                Ok(None)
            }
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> RatResult<Type> {
        let ty = match expr {
            Expr::Literal(lit, span) => {
                let lit_ty = match lit {
                    Literal::Int(_) => Type::Int,
                    Literal::Float(_) => Type::Float,
                    Literal::Bool(_) => Type::Bool,
                    Literal::String(_) => Type::String,
                    Literal::None => Type::None,
                };
                self.info.record(*span, lit_ty.clone());
                return Ok(lit_ty);
            }
            Expr::Identifier(name, span) => {
                let ty = self.env.get(name).unwrap_or(Type::Any);
                self.info.record(*span, ty.clone());
                return Ok(ty);
            }
            Expr::Binary(binary) => {
                let left_ty = self.check_expr(&binary.left)?;
                let right_ty = self.check_expr(&binary.right)?;
                match binary.op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                        if matches!(left_ty, Type::Float) || matches!(right_ty, Type::Float) {
                            Type::Float
                        } else {
                            Type::Int
                        }
                    }
                    BinaryOp::And | BinaryOp::Or => Type::Bool,
                    BinaryOp::Eq
                    | BinaryOp::NotEq
                    | BinaryOp::Lt
                    | BinaryOp::Lte
                    | BinaryOp::Gt
                    | BinaryOp::Gte => Type::Bool,
                    BinaryOp::Pipe => right_ty,
                    BinaryOp::NullCoalesce => left_ty,
                    BinaryOp::Range | BinaryOp::RangeInclusive => Type::List(Box::new(Type::Int)),
                    BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor => Type::Int,
                }
            }
            Expr::Unary(unary) => {
                let inner_ty = self.check_expr(&unary.expr)?;
                match unary.op {
                    UnaryOp::Neg => inner_ty,
                    UnaryOp::Not | UnaryOp::BitNot => Type::Bool,
                }
            }
            Expr::Call(call) => {
                self.check_expr(&call.function)?;
                for arg in &call.args {
                    self.check_expr(arg)?;
                }
                Type::Any
            }
            Expr::Field(field) => {
                self.check_expr(&field.target)?;
                Type::Any
            }
            Expr::Index(index) => {
                self.check_expr(&index.target)?;
                self.check_expr(&index.index)?;
                Type::Any
            }
            Expr::If(if_expr) => {
                self.check_expr(&if_expr.condition)?;
                let then_ty = self.check_block(&if_expr.then_branch)?;
                if let Some(else_branch) = &if_expr.else_branch {
                    let else_ty = self.check_block(else_branch)?;
                    if then_ty.is_assignable_from(&else_ty) {
                        then_ty
                    } else {
                        Type::Any
                    }
                } else {
                    Type::Option(Box::new(then_ty))
                }
            }
            Expr::Match(match_expr) => {
                self.check_expr(&match_expr.scrutinee)?;
                for arm in &match_expr.arms {
                    self.check_block(&arm.body)?;
                }
                Type::Any
            }
            Expr::List(list) => {
                if let Some(first) = list.elements.first() {
                    let item_ty = self.check_expr(first)?;
                    for element in &list.elements[1..] {
                        self.check_expr(element)?;
                    }
                    Type::List(Box::new(item_ty))
                } else {
                    Type::List(Box::new(Type::Any))
                }
            }
            Expr::Tuple(tuple) => Type::Tuple(
                tuple
                    .elements
                    .iter()
                    .map(|expr| self.check_expr(expr))
                    .collect::<RatResult<Vec<_>>>()?,
            ),
            Expr::Set(set) => {
                for element in &set.elements {
                    self.check_expr(element)?;
                }
                Type::Set(Box::new(Type::Any))
            }
            Expr::Dict(dict) => {
                for (key, value) in &dict.entries {
                    self.check_expr(key)?;
                    self.check_expr(value)?;
                }
                Type::Dict(Box::new(Type::Any), Box::new(Type::Any))
            }
            Expr::Lambda(lambda) => {
                let params = lambda
                    .params
                    .iter()
                    .map(|param| param.ty.as_ref().map(resolve_type_expr).unwrap_or(Type::Any))
                    .collect::<Vec<_>>();
                let body_ty = self.check_expr(&lambda.body)?;
                Type::Function(params, Box::new(body_ty), Asyncness::Sync)
            }
            Expr::Await(await_expr) => self.check_expr(&await_expr.expr)?,
            Expr::Spawn(spawn_expr) => self.check_expr(&spawn_expr.expr)?,
            Expr::AsyncBlock(block_expr) => self.check_block(&block_expr.block)?,
            Expr::Block(block) => self.check_block(block)?,
            Expr::Assign(assign_expr) => {
                let value_ty = self.check_expr(&assign_expr.value)?;
                if let Expr::Identifier(name, _) = &assign_expr.target {
                    self.env.assign(name, value_ty.clone());
                }
                value_ty
            }
            Expr::Pipe(pipe_expr) => {
                self.check_expr(&pipe_expr.input)?;
                self.check_expr(&pipe_expr.function)?;
                Type::Any
            }
            Expr::Range(range_expr) => {
                self.check_expr(&range_expr.start)?;
                self.check_expr(&range_expr.end)?;
                Type::List(Box::new(Type::Int))
            }
            Expr::Result(result_expr) => {
                match (&result_expr.ok, &result_expr.err) {
                    (Some(ok), Some(err)) => {
                        self.check_expr(ok)?;
                        self.check_expr(err)?;
                        Type::Any
                    }
                    (Some(ok), None) => {
                        let ok_ty = self.check_expr(ok)?;
                        Type::Struct(SmolStr::from(format!("Result[{:?}]", ok_ty)))
                    }
                    _ => Type::Any,
                }
            }
            Expr::Option(option_expr) => {
                if let Some(expr) = &option_expr.some {
                    let inner = self.check_expr(expr)?;
                    Type::Option(Box::new(inner))
                } else {
                    Type::Option(Box::new(Type::None))
                }
            }
        };
        if let Some(span) = expr_span(expr) {
            self.info.record(span, ty.clone());
        }
        Ok(ty)
    }

    fn pattern_name(&self, pattern: &Pattern) -> Option<SmolStr> {
        match pattern {
            Pattern::Identifier(name, _) if name.as_str() != "_" => Some(name.clone()),
            _ => None,
        }
    }
}

fn expr_span(expr: &Expr) -> Option<Span> {
    match expr {
        Expr::Literal(_, span)
        | Expr::Identifier(_, span)
        | Expr::List(ListExpr { span, .. })
        | Expr::Tuple(TupleExpr { span, .. })
        | Expr::Set(SetExpr { span, .. })
        | Expr::Dict(DictExpr { span, .. })
        | Expr::Lambda(LambdaExpr { span, .. })
        | Expr::Await(AwaitExpr { span, .. })
        | Expr::Spawn(SpawnExpr { span, .. })
        | Expr::AsyncBlock(BlockExpr { span, .. })
        | Expr::Assign(AssignExpr { span, .. })
        | Expr::Pipe(PipeExpr { span, .. })
        | Expr::Range(RangeExpr { span, .. })
        | Expr::Result(ResultExpr { span, .. })
        | Expr::Option(OptionExpr { span, .. }) => Some(*span),
        Expr::Binary(binary) => Some(binary.span),
        Expr::Unary(unary) => Some(unary.span),
        Expr::Call(call) => Some(call.span),
        Expr::Field(field) => Some(field.span),
        Expr::Index(index) => Some(index.span),
        Expr::If(if_expr) => Some(if_expr.span),
        Expr::Match(match_expr) => Some(match_expr.span),
        Expr::Block(block) => Some(block.span),
    }
}

pub fn check(program: &Program) -> RatResult<TypeInfo> {
    TypeChecker::new().check_program(program)
}
