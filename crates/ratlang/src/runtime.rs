use crate::ast::*;
use crate::compiler::Compilation;
use crate::diagnostics::{RatError, RatResult};
use smol_str::SmolStr;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Value {
    Int(i128),
    Float(f64),
    Bool(bool),
    String(String),
    None,
    List(Vec<Value>),
    Dict(HashMap<String, Value>),
    Function(Function),
}

#[derive(Clone, Debug)]
pub enum Function {
    User(UserFunction),
    Native(NativeFunction),
}

pub type NativeFunction = fn(&mut Interpreter, Vec<Value>) -> RatResult<Value>;

#[derive(Clone, Debug)]
pub struct UserFunction {
    pub name: SmolStr,
    pub params: Vec<Parameter>,
    pub asyncness: Asyncness,
    pub body: FunctionBody,
    pub env: Environment,
}

#[derive(Clone, Debug)]
pub enum FunctionBody {
    Block(Block),
    Expr(Expr),
}

#[derive(Clone, Debug, Default)]
pub struct Environment {
    frames: Vec<HashMap<SmolStr, Value>>,
}

impl Environment {
    pub fn new() -> Self {
        let mut env = Self { frames: Vec::new() };
        env.push();
        env
    }

    pub fn push(&mut self) {
        self.frames.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.frames.pop();
    }

    pub fn insert(&mut self, name: SmolStr, value: Value) {
        if let Some(frame) = self.frames.last_mut() {
            frame.insert(name, value);
        }
    }

    pub fn assign(&mut self, name: &SmolStr, value: Value) -> bool {
        for frame in self.frames.iter_mut().rev() {
            if frame.contains_key(name) {
                frame.insert(name.clone(), value);
                return true;
            }
        }
        false
    }

    pub fn get(&self, name: &SmolStr) -> Option<Value> {
        for frame in self.frames.iter().rev() {
            if let Some(value) = frame.get(name) {
                return Some(value.clone());
            }
        }
        None
    }
}

#[derive(Clone, Debug)]
pub struct ExecutionConfig {
    pub entry_point: SmolStr,
    pub args: Vec<String>,
}

impl Default for ExecutionConfig {
    fn default() -> Self {
        Self {
            entry_point: SmolStr::new_inline("main"),
            args: Vec::new(),
        }
    }
}

#[derive(Debug, Default)]
pub struct Vm;

impl Vm {
    pub fn new() -> Self {
        Self
    }

    pub fn execute(&mut self, compilation: &Compilation, config: ExecutionConfig) -> RatResult<Value> {
        let mut interpreter = Interpreter::new(compilation);
        interpreter.initialize_builtins();
        interpreter.eval_program()?;
        let entry_value = interpreter
            .env
            .get(&config.entry_point)
            .ok_or_else(|| RatError::error(format!("entry point `{}` not found", config.entry_point)))?;
        let args = Value::List(config.args.into_iter().map(Value::String).collect());
        interpreter.call_value(entry_value, vec![args])
    }
}

struct Interpreter<'a> {
    env: Environment,
    compilation: &'a Compilation,
}

impl<'a> Interpreter<'a> {
    fn new(compilation: &'a Compilation) -> Self {
        Self {
            env: Environment::new(),
            compilation,
        }
    }

    fn with_env(&self, env: Environment) -> Interpreter<'a> {
        Interpreter {
            env,
            compilation: self.compilation,
        }
    }

    fn initialize_builtins(&mut self) {
        self.env.insert(SmolStr::new_inline("print"), Value::Function(Function::Native(native_print)));
        self.env.insert(SmolStr::new_inline("len"), Value::Function(Function::Native(native_len)));
    }

    fn eval_program(&mut self) -> RatResult<()> {
        for item in &self.compilation.program.items {
            match item {
                Item::Constant(const_decl) => {
                    let value = self.eval_expr(&const_decl.value)?;
                    self.env.insert(const_decl.name.clone(), value);
                }
                Item::Function(func_decl) => {
                    let func = UserFunction {
                        name: func_decl.name.clone(),
                        params: func_decl.params.clone(),
                        asyncness: func_decl.asyncness.clone(),
                        body: FunctionBody::Block(func_decl.body.clone()),
                        env: self.env.clone(),
                    };
                    self.env.insert(func_decl.name.clone(), Value::Function(Function::User(func)));
                }
                Item::Statement(stmt) => {
                    self.eval_stmt(stmt)?.into_value();
                }
                Item::Struct(_) | Item::Class(_) | Item::Enum(_) | Item::Trait(_) | Item::Impl(_) => {
                    // Not yet executable at runtime; skip.
                }
                Item::Test(_) => {
                    // Tests executed via harness; ignore during normal execution.
                }
            }
        }
        Ok(())
    }

    fn eval_block(&mut self, block: &Block) -> RatResult<ExecState> {
        self.env.push();
        let mut last = ExecState::Value(Value::None);
        for stmt in &block.statements {
            let state = self.eval_stmt(stmt)?;
            match state {
                ExecState::Value(_) => {
                    last = state;
                }
                ExecState::Return(_) | ExecState::Break | ExecState::Continue => {
                    self.env.pop();
                    return Ok(state);
                }
            }
        }
        self.env.pop();
        Ok(last)
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> RatResult<ExecState> {
        match stmt {
            Stmt::Let(let_stmt) => {
                let value = self.eval_expr(&let_stmt.value)?;
                if let Some(name) = self.pattern_name(&let_stmt.name) {
                    self.env.insert(name, value);
                }
                Ok(ExecState::Value(Value::None))
            }
            Stmt::Var(var_stmt) => {
                let value = self.eval_expr(&var_stmt.value)?;
                if let Some(name) = self.pattern_name(&var_stmt.name) {
                    self.env.insert(name, value);
                }
                Ok(ExecState::Value(Value::None))
            }
            Stmt::Assign(assign_stmt) => {
                let value = self.eval_expr(&assign_stmt.value)?;
                if let Expr::Identifier(name, _) = &assign_stmt.target {
                    if !self.env.assign(name, value.clone()) {
                        self.env.insert(name.clone(), value.clone());
                    }
                }
                Ok(ExecState::Value(value))
            }
            Stmt::Expr(expr) => {
                let value = self.eval_expr(expr)?;
                Ok(ExecState::Value(value))
            }
            Stmt::Return(return_stmt) => {
                let value = if let Some(expr) = &return_stmt.value {
                    self.eval_expr(expr)?
                } else {
                    Value::None
                };
                Ok(ExecState::Return(value))
            }
            Stmt::Break(_) => Ok(ExecState::Break),
            Stmt::Continue(_) => Ok(ExecState::Continue),
            Stmt::Defer(_, _) => {
                // Not yet implemented; ignore.
                Ok(ExecState::Value(Value::None))
            }
            Stmt::While(while_stmt) => {
                while self.eval_expr(&while_stmt.condition)?.is_truthy() {
                    match self.eval_block(&while_stmt.body)? {
                        ExecState::Value(_) => {}
                        ExecState::Break => break,
                        ExecState::Continue => continue,
                        ExecState::Return(value) => return Ok(ExecState::Return(value)),
                    }
                }
                Ok(ExecState::Value(Value::None))
            }
            Stmt::For(for_stmt) => {
                let iterable = self.eval_expr(&for_stmt.iterable)?;
                for value in iterable.into_iterable() {
                    self.env.push();
                    if let Some(name) = self.pattern_name(&for_stmt.binder) {
                        self.env.insert(name, value);
                    }
                    match self.eval_block(&for_stmt.body)? {
                        ExecState::Value(_) => {}
                        ExecState::Break => {
                            self.env.pop();
                            break;
                        }
                        ExecState::Continue => {
                            self.env.pop();
                            continue;
                        }
                        ExecState::Return(value) => {
                            self.env.pop();
                            return Ok(ExecState::Return(value));
                        }
                    }
                    self.env.pop();
                }
                Ok(ExecState::Value(Value::None))
            }
            Stmt::Try(try_stmt) => {
                // Execute body; ignore exception semantics for now.
                self.eval_block(&try_stmt.body)?;
                if let Some(finally) = &try_stmt.finally {
                    self.eval_block(finally)?;
                }
                Ok(ExecState::Value(Value::None))
            }
            Stmt::WithDefer(block) => self.eval_block(block),
        }
    }

    fn eval_expr(&mut self, expr: &Expr) -> RatResult<Value> {
        match expr {
            Expr::Literal(lit, _) => Ok(match lit {
                Literal::Int(v) => Value::Int(*v),
                Literal::Float(v) => Value::Float(*v),
                Literal::Bool(v) => Value::Bool(*v),
                Literal::String(v) => Value::String(v.clone()),
                Literal::None => Value::None,
            }),
            Expr::Identifier(name, _) => self
                .env
                .get(name)
                .ok_or_else(|| RatError::error(format!("undefined identifier `{}`", name))),
            Expr::Binary(binary) => {
                if matches!(binary.op, BinaryOp::Pipe) {
                    let value = self.eval_expr(&binary.left)?;
                    let func = self.eval_expr(&binary.right)?;
                    self.call_value(func, vec![value])
                } else {
                    let left = self.eval_expr(&binary.left)?;
                    let right = self.eval_expr(&binary.right)?;
                    self.eval_binary(binary.op, left, right)
                }
            }
            Expr::Unary(unary) => {
                let value = self.eval_expr(&unary.expr)?;
                self.eval_unary(unary.op, value)
            }
            Expr::Call(call) => {
                let callee = self.eval_expr(&call.function)?;
                let mut args = Vec::new();
                for arg in &call.args {
                    args.push(self.eval_expr(arg)?);
                }
                self.call_value(callee, args)
            }
            Expr::Field(field) => {
                let target = self.eval_expr(&field.target)?;
                self.eval_field(target, &field.field)
            }
            Expr::Index(index) => {
                let target = self.eval_expr(&index.target)?;
                let idx = self.eval_expr(&index.index)?;
                self.eval_index(target, idx)
            }
            Expr::If(if_expr) => {
                if self.eval_expr(&if_expr.condition)?.is_truthy() {
                    match self.eval_block(&if_expr.then_branch)? {
                        ExecState::Return(value) => Ok(value),
                        ExecState::Value(value) => Ok(value),
                        ExecState::Break | ExecState::Continue => Ok(Value::None),
                    }
                } else if let Some(else_block) = &if_expr.else_branch {
                    match self.eval_block(else_block)? {
                        ExecState::Return(value) => Ok(value),
                        ExecState::Value(value) => Ok(value),
                        ExecState::Break | ExecState::Continue => Ok(Value::None),
                    }
                } else {
                    Ok(Value::None)
                }
            }
            Expr::Match(match_expr) => {
                let scrutinee = self.eval_expr(&match_expr.scrutinee)?;
                for arm in &match_expr.arms {
                    if self.pattern_matches(&arm.pattern, &scrutinee) {
                        if let Some(guard) = &arm.guard {
                            if !self.eval_expr(guard)?.is_truthy() {
                                continue;
                            }
                        }
                        match self.eval_block(&arm.body)? {
                            ExecState::Return(value) => return Ok(value),
                            ExecState::Value(value) => return Ok(value),
                            ExecState::Break | ExecState::Continue => return Ok(Value::None),
                        }
                    }
                }
                Ok(Value::None)
            }
            Expr::List(list) => {
                let mut items = Vec::new();
                for element in &list.elements {
                    items.push(self.eval_expr(element)?);
                }
                Ok(Value::List(items))
            }
            Expr::Tuple(tuple) => {
                let mut items = Vec::new();
                for element in &tuple.elements {
                    items.push(self.eval_expr(element)?);
                }
                Ok(Value::List(items))
            }
            Expr::Set(set) => {
                let mut items = Vec::new();
                for element in &set.elements {
                    items.push(self.eval_expr(element)?);
                }
                Ok(Value::List(items))
            }
            Expr::Dict(dict) => {
                let mut map = HashMap::new();
                for (key, value) in &dict.entries {
                    let key_value = self.eval_expr(key)?;
                    let value_value = self.eval_expr(value)?;
                    map.insert(key_value.to_string(), value_value);
                }
                Ok(Value::Dict(map))
            }
            Expr::Lambda(lambda) => {
                let func = UserFunction {
                    name: SmolStr::new_inline("<lambda>"),
                    params: lambda.params.clone(),
                    asyncness: Asyncness::Sync,
                    body: FunctionBody::Expr(*lambda.body.clone()),
                    env: self.env.clone(),
                };
                Ok(Value::Function(Function::User(func)))
            }
            Expr::Await(await_expr) => self.eval_expr(&await_expr.expr),
            Expr::Spawn(spawn_expr) => self.eval_expr(&spawn_expr.expr),
            Expr::AsyncBlock(block_expr) => {
                match self.eval_block(&block_expr.block)? {
                    ExecState::Return(value) | ExecState::Value(value) => Ok(value),
                    ExecState::Break | ExecState::Continue => Ok(Value::None),
                }
            }
            Expr::Block(block) => match self.eval_block(block)? {
                ExecState::Return(value) | ExecState::Value(value) => Ok(value),
                ExecState::Break | ExecState::Continue => Ok(Value::None),
            },
            Expr::Assign(assign_expr) => {
                let value = self.eval_expr(&assign_expr.value)?;
                if let Expr::Identifier(name, _) = &assign_expr.target {
                    if !self.env.assign(name, value.clone()) {
                        self.env.insert(name.clone(), value.clone());
                    }
                }
                Ok(value)
            }
            Expr::Pipe(pipe_expr) => {
                let value = self.eval_expr(&pipe_expr.input)?;
                let func = self.eval_expr(&pipe_expr.function)?;
                self.call_value(func, vec![value])
            }
            Expr::Range(range_expr) => {
                let start = self.eval_expr(&range_expr.start)?.as_int()?;
                let end = self.eval_expr(&range_expr.end)?.as_int()?;
                let mut values = Vec::new();
                if range_expr.inclusive {
                    for i in start..=end {
                        values.push(Value::Int(i));
                    }
                } else {
                    for i in start..end {
                        values.push(Value::Int(i));
                    }
                }
                Ok(Value::List(values))
            }
            Expr::Result(result_expr) => {
                if let Some(ok) = &result_expr.ok {
                    Ok(self.eval_expr(ok.as_ref())?)
                } else if let Some(err) = &result_expr.err {
                    Ok(self.eval_expr(err.as_ref())?)
                } else {
                    Ok(Value::None)
                }
            }
            Expr::Option(option_expr) => {
                if let Some(expr) = &option_expr.some {
                    Ok(self.eval_expr(expr.as_ref())?)
                } else {
                    Ok(Value::None)
                }
            }
        }
    }

    fn eval_binary(&self, op: BinaryOp, left: Value, right: Value) -> RatResult<Value> {
        use BinaryOp::*;
        match op {
            Add => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 + b)),
                (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + b as f64)),
                (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b)),
                _ => Ok(Value::None),
            },
            Sub => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 - b)),
                (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - b as f64)),
                _ => Ok(Value::None),
            },
            Mul => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 * b)),
                (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * b as f64)),
                _ => Ok(Value::None),
            },
            Div => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Float(a as f64 / b as f64)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
                (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 / b)),
                (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a / b as f64)),
                _ => Ok(Value::None),
            },
            Mod => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a % b)),
                _ => Ok(Value::None),
            },
            And => Ok(Value::Bool(left.is_truthy() && right.is_truthy())),
            Or => Ok(Value::Bool(left.is_truthy() || right.is_truthy())),
            BitAnd => Ok(Value::Int(left.as_int()? & right.as_int()?)),
            BitOr => Ok(Value::Int(left.as_int()? | right.as_int()?)),
            BitXor => Ok(Value::Int(left.as_int()? ^ right.as_int()?)),
            Eq => Ok(Value::Bool(left == right)),
            NotEq => Ok(Value::Bool(left != right)),
            Lt => Ok(Value::Bool(left.partial_cmp(&right).map_or(false, |ord| ord == std::cmp::Ordering::Less))),
            Lte => Ok(Value::Bool(left.partial_cmp(&right).map_or(false, |ord| ord != std::cmp::Ordering::Greater))),
            Gt => Ok(Value::Bool(left.partial_cmp(&right).map_or(false, |ord| ord == std::cmp::Ordering::Greater))),
            Gte => Ok(Value::Bool(left.partial_cmp(&right).map_or(false, |ord| ord != std::cmp::Ordering::Less))),
            Pipe => Ok(right),
            NullCoalesce => Ok(if !left.is_none() { left } else { right }),
            Range | RangeInclusive => Ok(Value::None),
        }
    }

    fn eval_unary(&self, op: UnaryOp, value: Value) -> RatResult<Value> {
        Ok(match op {
            UnaryOp::Neg => match value {
                Value::Int(v) => Value::Int(-v),
                Value::Float(v) => Value::Float(-v),
                other => other,
            },
            UnaryOp::Not => Value::Bool(!value.is_truthy()),
            UnaryOp::BitNot => Value::Int(!value.as_int()?),
        })
    }

    fn call_value(&mut self, callee: Value, args: Vec<Value>) -> RatResult<Value> {
        match callee {
            Value::Function(Function::User(func)) => self.call_user_function(func, args),
            Value::Function(Function::Native(native)) => native(self, args),
            _ => Err(RatError::error("attempted to call a non-function value")),
        }
    }

    fn call_user_function(&mut self, func: UserFunction, args: Vec<Value>) -> RatResult<Value> {
        if args.len() != func.params.len() {
            return Err(RatError::error(format!(
                "function `{}` expected {} arguments, got {}",
                func.name,
                func.params.len(),
                args.len()
            )));
        }
        let mut env = func.env.clone();
        // Ensure user-defined functions can recurse by making the function value visible inside its own closure env.
        env.insert(func.name.clone(), Value::Function(Function::User(func.clone())));
        env.push();
        for (param, arg) in func.params.iter().zip(args.into_iter()) {
            env.insert(param.name.clone(), arg);
        }
        let mut child = self.with_env(env);
        let result = match func.body {
            FunctionBody::Block(block) => child.eval_block(&block)?,
            FunctionBody::Expr(expr) => {
                let value = child.eval_expr(&expr)?;
                ExecState::Value(value)
            }
        };
        match result {
            ExecState::Return(value) | ExecState::Value(value) => Ok(value),
            ExecState::Break | ExecState::Continue => Ok(Value::None),
        }
    }

    fn eval_field(&mut self, target: Value, field: &SmolStr) -> RatResult<Value> {
        match target {
            Value::Dict(map) => map
                .get(field.as_str())
                .cloned()
                .ok_or_else(|| RatError::error(format!("field `{}` not found", field))),
            _ => Err(RatError::error("field access on non-dict value")),
        }
    }

    fn eval_index(&mut self, target: Value, index: Value) -> RatResult<Value> {
        match (target, index) {
            (Value::List(list), Value::Int(i)) => {
                list.get(i as usize).cloned().ok_or_else(|| RatError::error("index out of bounds"))
            }
            (Value::String(s), Value::Int(i)) => s
                .chars()
                .nth(i as usize)
                .map(|c| Value::String(c.to_string()))
                .ok_or_else(|| RatError::error("index out of bounds")),
            _ => Err(RatError::error("unsupported index operation")),
        }
    }

    fn pattern_matches(&mut self, pattern: &Pattern, value: &Value) -> bool {
        match pattern {
            Pattern::Identifier(name, _) => {
                if name.as_str() != "_" {
                    self.env.insert(name.clone(), value.clone());
                }
                true
            }
            Pattern::Literal(lit, _) => match (lit, value) {
                (Literal::Int(a), Value::Int(b)) => a == b,
                (Literal::Float(a), Value::Float(b)) => a == b,
                (Literal::Bool(a), Value::Bool(b)) => a == b,
                (Literal::String(a), Value::String(b)) => a == b,
                (Literal::None, Value::None) => true,
                _ => false,
            },
            Pattern::Wildcard(_) => true,
            _ => false,
        }
    }

    fn pattern_name(&self, pattern: &Pattern) -> Option<SmolStr> {
        if let Pattern::Identifier(name, _) = pattern {
            Some(name.clone())
        } else {
            None
        }
    }
}

#[derive(Clone)]
enum ExecState {
    Value(Value),
    Return(Value),
    Break,
    Continue,
}

impl ExecState {
    fn into_value(self) -> Option<Value> {
        match self {
            ExecState::Value(value) => Some(value),
            ExecState::Return(value) => Some(value),
            ExecState::Break | ExecState::Continue => None,
        }
    }
}

impl Value {
    fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(v) => *v,
            Value::None => false,
            Value::Int(v) => *v != 0,
            Value::Float(v) => *v != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::List(list) => !list.is_empty(),
            Value::Dict(map) => !map.is_empty(),
            Value::Function(_) => true,
        }
    }

    fn is_none(&self) -> bool {
        matches!(self, Value::None)
    }

    fn as_int(&self) -> RatResult<i128> {
        match self {
            Value::Int(v) => Ok(*v),
            _ => Err(RatError::error("expected integer value")),
        }
    }

    fn into_iterable(self) -> Vec<Value> {
        match self {
            Value::List(values) => values,
            Value::String(s) => s.chars().map(|c| Value::String(c.to_string())).collect(),
            _ => vec![],
        }
    }

    fn to_string(&self) -> String {
        match self {
            Value::Int(v) => v.to_string(),
            Value::Float(v) => v.to_string(),
            Value::Bool(v) => v.to_string(),
            Value::String(v) => v.clone(),
            Value::None => "none".to_string(),
            Value::List(list) => {
                let items: Vec<String> = list.iter().map(|v| v.to_string()).collect();
                format!("[{}]", items.join(", "))
            }
            Value::Dict(map) => {
                let mut entries: Vec<String> = map.iter().map(|(k, v)| format!("{}: {}", k, v.to_string())).collect();
                entries.sort();
                format!("{{{}}}", entries.join(", "))
            }
            Value::Function(_) => "<function>".to_string(),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::None, Value::None) => true,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Dict(a), Value::Dict(b)) => a == b,
            _ => false,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a.partial_cmp(b),
            (Value::Float(a), Value::Float(b)) => a.partial_cmp(b),
            (Value::Int(a), Value::Float(b)) => (*a as f64).partial_cmp(b),
            (Value::Float(a), Value::Int(b)) => a.partial_cmp(&(*b as f64)),
            (Value::String(a), Value::String(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

fn native_print(_interp: &mut Interpreter, args: Vec<Value>) -> RatResult<Value> {
    let parts: Vec<String> = args.into_iter().map(|v| v.to_string()).collect();
    println!("{}", parts.join(" "));
    Ok(Value::None)
}

fn native_len(_interp: &mut Interpreter, args: Vec<Value>) -> RatResult<Value> {
    if args.len() != 1 {
        return Err(RatError::error("len expects exactly one argument"));
    }
    let value = &args[0];
    let len = match value {
        Value::String(s) => s.chars().count(),
        Value::List(list) => list.len(),
        Value::Dict(map) => map.len(),
        _ => 0,
    };
    Ok(Value::Int(len as i128))
}
