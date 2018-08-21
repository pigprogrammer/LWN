use std::fmt;
use parser::Stmt;
use super::NativeFn;
use super::namespace::*;
#[derive(Debug,Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
    Function(Vec<String>, Vec<Stmt>, NamespaceRef),
    NativeFunction(NativeFn),
}

impl Value {
    pub fn is_true(&self) -> bool {
        match self {
            Value::Nil | Value::Bool(false) => false,
            _ => true,
        }
    }

    #[inline(always)]
    pub fn is_false(&self) -> bool {
        !self.is_true()
    }
}


impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(val) => write!(f, "{}", val),
            Value::Int(val) => write!(f, "{}", val),
            Value::Str(val) => write!(f, "{}", val),
            Value::Float(val) => write!(f,"{}",val),
            Value::Function(_, _, _) => write!(f, "<fn>"),
            Value::NativeFunction(_) => write!(f, "<native fn>"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        use self::Value::*;

        match (self, other) {
            (Nil, Nil) => true,
            (Bool(a), Bool(b)) => a == b,
            (Float(a), Float(b)) => a == b,
            (Int(a),Int(b)) => a == b,
            (Float(a),Int(b)) => *a == *b as f64,
            (Int(a),Float(b)) => *a as f64 == *b,
            (Str(a), Str(b)) => a == b,
            _ => false,
        }
    }
}