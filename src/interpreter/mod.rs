pub mod namespace;
mod value;
use parser::{BinaryOp,Expr,Stmt,UnaryOp};
use self::namespace::{Namespace,NamespaceRef};
use self::value::Value;
use std::fmt;
#[derive(Clone)]
pub struct NativeFn(pub fn(&mut VM, &[Expr]) -> Result<Value, String>);

impl fmt::Debug for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<native fn>")
    }
}

pub struct VM {
    stack: Vec<NamespaceRef>,
}
mod builtins;
impl VM {
    pub fn new() -> VM {
        let namespace = Namespace::root();
        namespace.borrow_mut().declare(
            "print".to_string(),
            Value::NativeFunction(NativeFn(builtins::print)),
        );
        namespace.borrow_mut().declare(
            "dprint".to_string(),
            Value::NativeFunction(NativeFn(builtins::dprint)),
        );
        
        VM {
            stack: vec![namespace]
        }
    }

    pub fn current_namespace(&self) -> NamespaceRef {
        self.stack.last().expect("The stack should never be unrooted").clone()
    }

    pub fn push_namespace(&mut self,namespace: NamespaceRef) {
        self.stack.push(namespace)
    }
    pub fn pop(&mut self) -> NamespaceRef {
        if self.stack.len() == 1 {
            panic!("The stack should never be unrooted");
        }

        self.stack
            .pop()
            .expect("The stack should never be unrooted")
        
    }
}

pub trait Evaluate<T> {
    fn eval(&self, ctx: &mut VM) -> Result<T, String>;

    fn eval_in_namespace(&self, ctx: &mut VM, namespace: NamespaceRef) -> Result<T, String> {
        ctx.push_namespace(namespace);
        let val = self.eval(ctx);
        ctx.pop();
        val
    }
}


impl Evaluate<Option<Value>> for Vec<Stmt> {
    fn eval(&self, ctx: &mut VM) -> Result<Option<Value>, String> {
        for stmt in self {
            if let Some(ret) = stmt.eval(ctx)? {
                return Ok(Some(ret));
            }
        }

        Ok(None)
    }
}
impl Evaluate<Option<Value>> for Stmt {
    fn eval(&self, ctx: &mut VM) -> Result<Option<Value>, String> {
        match self {
            Stmt::Return(expr) => Ok(Some(expr.eval(ctx)?)),

            Stmt::ExprStmt(expr) => {
                expr.eval(ctx)?;
                Ok(None)
            }

            Stmt::Declaration(name, expr) => {
                // TODO: Re-inline this once NLL is on stable
                let value = expr.eval(ctx)?;
                let env = ctx.current_namespace();
                env.borrow_mut().declare(name.clone(), value);
                Ok(None)
            }

            Stmt::If(condition, when_true, when_false) => {
                let cond_val = condition.eval(ctx)?;

                if cond_val.is_true() {
                    when_true.eval(ctx)
                } else {
                    when_false.eval(ctx)
                }
            }

            Stmt::While(condition, body) => {
                while condition.eval(ctx)?.is_true() {
                    if let Some(ret) = body.eval(ctx)? {
                        return Ok(Some(ret));
                    }
                }
                Ok(None)
            }
        }
    }
}

impl Evaluate<Value> for Expr {
    fn eval(&self, ctx: &mut VM) -> Result<Value, String> {
        match self {
            Expr::Nil => Ok(Value::Nil),
            Expr::BooleanLiteral(val) => Ok(Value::Bool(*val)),
            Expr::IntLiteral(val) => Ok(Value::Int(*val)),
            Expr::FloatLiteral(val) => Ok(Value::Float(*val)),
            Expr::StringLiteral(val) => Ok(Value::Str(val.clone())),

            Expr::Identifier(name) => {
                // TODO: Re-inline this once NLL is on stable
                let env = ctx.current_namespace();
                let resolved = env.borrow().get(name);

                match resolved {
                    Some(value) => Ok(value),
                    None => Ok(Value::Nil),
                }
            }

            Expr::Assign(name, expr) => {
                // TODO: Re-inline this once NLL is on stable
                let expr_val = expr.eval(ctx)?;
                let env = ctx.current_namespace();
                env.borrow_mut().assign(name.clone(), expr_val.clone())?;
                Ok(expr_val)
            }

            Expr::Function(f) => Ok(Value::Function(
                f.params.clone(),
                f.stmts.clone(),
                ctx.current_namespace(),
            )),

            Expr::Call(target, args) => {
                let target_val = target.eval(ctx)?;

                match target_val {
                    Value::Function(params, body, env) => {
                        if args.len() != params.len() {
                            return Err(format!(
                                "Expected {} arguments, found {}",
                                params.len(),
                                args.len()
                            ));
                        }

                        // TODO: Proper lexical scoping
                        let mut fn_env = Namespace::child(&env);

                        for (arg, name) in args.iter().zip(params) {
                            fn_env.borrow_mut().declare(name.clone(), arg.eval(ctx)?);
                        }

                        if let Some(ret) = body.eval_in_namespace(ctx, fn_env)? {
                            return Ok(ret);
                        }

                        Ok(Value::Nil)
                    }

                    Value::NativeFunction(pointer) => pointer.0(ctx, args),

                    other => Err(format!("{} is not a callable object", other)),
                }
            }
            Expr::Class(_,_,_) => unimplemented!(),
            Expr::UnaryOp(op, expr) => {
                let expr_val = expr.eval(ctx)?;

                match op {
                    UnaryOp::Not => Ok(Value::Bool(!expr_val.is_true())),

                    UnaryOp::UnaryMinus => match expr_val {
                        Value::Int(v) => Ok(Value::Int(-v)),
                        Value::Float(v) => Ok(Value::Float(-v)),
                        other => Err(format!("{} cannot be negated", other)),
                    },
                }
            }

            Expr::BinaryOp(op, left, right) => {
                match op {
                    // Arithmatic
                    BinaryOp::Add => {
                        let left_val = left.eval(ctx)?;
                        let right_val = right.eval(ctx)?;

                        match (left_val, right_val) {
                            (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
                            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l + r)),
                            (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 + r)),
                            (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l + r as f64)),
                            (Value::Str(l), Value::Str(r)) => Ok(Value::Str(l + &r)),
                            (l, r) => Err(format!("{} and {} cannot be added", l, r)),
                        }
                    }

                    BinaryOp::Subtract => {
                        let left_val = left.eval(ctx)?;
                        let right_val = right.eval(ctx)?;

                        match (left_val, right_val) {
                            (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l - r)),
                            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l - r)),
                            (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 - r)),
                            (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l - r as f64)),
                            (l, r) => Err(format!("{} and {} cannot be subtracted", l, r)),
                        }
                    }

                    BinaryOp::Multiply => {
                        let left_val = left.eval(ctx)?;
                        let right_val = right.eval(ctx)?;

                        match (left_val, right_val) {
                            (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l * r)),
                            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l * r)),
                            (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 * r)),
                            (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l * r as f64)),
                            (l, r) => Err(format!("{} and {} cannot be multiplied", l, r)),
                        }
                    }

                    BinaryOp::Divide => {
                        let left_val = left.eval(ctx)?;
                        let right_val = right.eval(ctx)?;

                        match (left_val, right_val) {
                            (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l / r)),
                            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l / r)),
                            (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 / r)),
                            (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l / r as f64)),
                            (l, r) => Err(format!("{} and {} cannot be divided", l, r)),
                        }
                    }

                    // Comparison
                    BinaryOp::Equals => {
                        let left_val = left.eval(ctx)?;
                        let right_val = right.eval(ctx)?;

                        Ok(Value::Bool(left_val == right_val))
                    }

                    BinaryOp::NotEquals => {
                        let left_val = left.eval(ctx)?;
                        let right_val = right.eval(ctx)?;

                        Ok(Value::Bool(left_val != right_val))
                    }

                    BinaryOp::GreaterThan => {
                        let left_val = left.eval(ctx)?;
                        let right_val = right.eval(ctx)?;

                        match (left_val, right_val) {
                            (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l > r)),
                            (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l > r)),
                            (Value::Int(l), Value::Float(r)) => Ok(Value::Bool(l as f64 > r)),
                            (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l > r as f64)),
                            (l, r) => Err(format!("{} and {} cannot be compared", l, r)),
                        }
                    }

                    BinaryOp::GreaterEquals => {
                        let left_val = left.eval(ctx)?;
                        let right_val = right.eval(ctx)?;

                        match (left_val, right_val) {
                            (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l >= r)),
                            (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l >= r)),
                            (Value::Int(l), Value::Float(r)) => Ok(Value::Bool(l as f64 >= r)),
                            (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l >= r as f64)),
                            (l, r) => Err(format!("{} and {} cannot be compared", l, r)),
                        }
                    }

                    BinaryOp::LessThan => {
                        let left_val = left.eval(ctx)?;
                        let right_val = right.eval(ctx)?;

                        match (left_val, right_val) {
                            (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l < r)),
                            (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l < r)),
                            (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) < r)),
                            (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l < r as f64)),
                            (l, r) => Err(format!("{} and {} cannot be compared", l, r)),
                        }
                    }

                    BinaryOp::LessEquals => {
                        let left_val = left.eval(ctx)?;
                        let right_val = right.eval(ctx)?;

                        match (left_val, right_val) {
                            (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l <= r)),
                            (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l <= r)),
                            (Value::Int(l), Value::Float(r)) => Ok(Value::Bool(l as f64 <= r)),
                            (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l <= r as f64)),
                            (l, r) => Err(format!("{} and {} cannot be compared", l, r)),
                        }
                    }

                    // Logic
                    BinaryOp::And => {
                        let left_val = left.eval(ctx)?;

                        if left_val.is_false() {
                            Ok(left_val)
                        } else {
                            right.eval(ctx)
                        }
                    }

                    BinaryOp::Or => {
                        let left_val = left.eval(ctx)?;

                        if left_val.is_true() {
                            Ok(left_val)
                        } else {
                            right.eval(ctx)
                        }
                    }

                }
            }
            Expr::Array(ref array) => {
                let mut values = vec![];
                for i in array.iter() {
                    values.push(i.eval(ctx)?);
                }
                Ok(Value::Array(values))
            }
            _ => unimplemented!()
        }
    }
}