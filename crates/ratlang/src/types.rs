use crate::ast::{Asyncness, TypeExpr};
use crate::position::Span;
use smol_str::SmolStr;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Any,
    Unknown,
    Int,
    Float,
    Bool,
    String,
    None,
    List(Box<Type>),
    Dict(Box<Type>, Box<Type>),
    Set(Box<Type>),
    Tuple(Vec<Type>),
    Option(Box<Type>),
    Function(Vec<Type>, Box<Type>, Asyncness),
    Struct(SmolStr),
    Enum(SmolStr),
}

impl Type {
    pub fn is_assignable_from(&self, other: &Type) -> bool {
        matches!(self, Type::Any)
            || self == other
            || matches!((self, other), (Type::Float, Type::Int))
    }
}

#[derive(Clone, Default)]
pub struct TypeEnvironment {
    scopes: Vec<HashMap<SmolStr, Type>>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        let mut env = Self { scopes: Vec::new() };
        env.push_scope();
        env
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, name: SmolStr, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, ty);
        }
    }

    pub fn assign(&mut self, name: &SmolStr, ty: Type) -> bool {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.clone(), ty);
                return true;
            }
        }
        false
    }

    pub fn get(&self, name: &SmolStr) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
    }
}

pub fn resolve_type_expr(expr: &TypeExpr) -> Type {
    match expr {
        TypeExpr::Simple(path, _) => match path.last().map(|s| s.as_str()) {
            Some("int") | Some("i64") | Some("i32") => Type::Int,
            Some("float") | Some("f64") => Type::Float,
            Some("bool") => Type::Bool,
            Some("str") | Some("string") => Type::String,
            Some("none") | Some("None") => Type::None,
            Some(name) => Type::Struct(SmolStr::from(name)),
            None => Type::Any,
        },
        TypeExpr::Optional(inner, _) => Type::Option(Box::new(resolve_type_expr(inner))),
        TypeExpr::Tuple(elements, _) => Type::Tuple(elements.iter().map(resolve_type_expr).collect()),
        TypeExpr::List(inner, _) => Type::List(Box::new(resolve_type_expr(inner))),
        TypeExpr::Set(inner, _) => Type::Set(Box::new(resolve_type_expr(inner))),
        TypeExpr::Dict(key, value, _) => {
            Type::Dict(Box::new(resolve_type_expr(key)), Box::new(resolve_type_expr(value)))
        }
        TypeExpr::Function { params, ret, span: _ } => Type::Function(
            params.iter().map(resolve_type_expr).collect(),
            Box::new(resolve_type_expr(ret)),
            Asyncness::Sync,
        ),
        TypeExpr::Generic { base, args, .. } => match base.last().map(|s| s.as_str()) {
            Some("List") | Some("Vec") => {
                let item_ty = args.get(0).map(resolve_type_expr).unwrap_or(Type::Any);
                Type::List(Box::new(item_ty))
            }
            Some("Set") => {
                let item_ty = args.get(0).map(resolve_type_expr).unwrap_or(Type::Any);
                Type::Set(Box::new(item_ty))
            }
            Some("Map") | Some("Dict") => {
                let key_ty = args.get(0).map(resolve_type_expr).unwrap_or(Type::Any);
                let value_ty = args.get(1).map(resolve_type_expr).unwrap_or(Type::Any);
                Type::Dict(Box::new(key_ty), Box::new(value_ty))
            }
            Some("Option") | Some("Optional") => {
                let inner = args.get(0).map(resolve_type_expr).unwrap_or(Type::Any);
                Type::Option(Box::new(inner))
            }
            Some(name) => Type::Struct(SmolStr::from(name)),
            None => Type::Any,
        },
        TypeExpr::SelfType(_) => Type::Any,
    }
}

#[derive(Debug, Default, Clone)]
pub struct TypeInfo {
    pub expr_types: HashMap<Span, Type>,
}

impl TypeInfo {
    pub fn record(&mut self, span: Span, ty: Type) {
        self.expr_types.insert(span, ty);
    }

    pub fn get(&self, span: &Span) -> Option<&Type> {
        self.expr_types.get(span)
    }
}
