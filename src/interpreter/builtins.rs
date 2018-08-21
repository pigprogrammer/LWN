use super::VM;
use parser::Expr;
use super::value::Value;
use super::Evaluate;
use std::io;
use std::io::Write;
pub fn print(ctx: &mut VM, args: &[Expr]) -> Result<Value, String> {
    let mut args_iter = args.iter();

    let stdout = io::stdout();
    let mut handle = stdout.lock();

    if let Some(arg) = args_iter.next() {
        let value = arg.eval(ctx)?;
        write!(handle, "{}", value).map_err(|e| e.to_string())?;

        for arg in args_iter {
            let value = arg.eval(ctx)?;
            write!(handle, " {}", value).map_err(|e| e.to_string())?;
        }
    }

    writeln!(handle).map_err(|e| e.to_string())?;

    Ok(Value::Nil)
}
pub fn dprint(ctx: &mut VM, args: &[Expr]) -> Result<Value,String> {
    let mut args_iter = args.iter();

    let stdout = io::stdout();
    let mut handle = stdout.lock();

    if let Some(arg) = args_iter.next() {
        let value = arg.eval(ctx)?;
        write!(handle, "{:?}", value).map_err(|e| e.to_string())?;

        for arg in args_iter {
            let value = arg.eval(ctx)?;
            write!(handle, " {:?}", value).map_err(|e| e.to_string())?;
        }
    }

    writeln!(handle).map_err(|e| e.to_string())?;

    Ok(Value::Nil)
}

