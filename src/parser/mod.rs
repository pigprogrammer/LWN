pub mod lexer;

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    And,
    Or,
    Equals,
    NotEquals,
    GreaterThan,
    GreaterEquals,
    LessThan,
    LessEquals,
    Add,
    Subtract,
    Multiply,
    Divide,
}
pub mod grammar;
use self::grammar::*;
use lalrpop_util::ParseError as LParseError;
use self::lexer::{Lexer,LexicalError,Location};
use self::lexer::Token;
pub type ParseError<'input> = LParseError<Location, Token<'input>, LexicalError>;

pub fn parse_program(input: &str) -> Result<Vec<Stmt>, ParseError> {
    let lexer = Lexer::new(input);
    let parser = ProgramParser::new();
    parser.parse(lexer)
}

pub fn parse_expr(input: &str) -> Result<Expr, ParseError> {
    let lexer = Lexer::new(input);
    let parser = ExprParser::new();
    parser.parse(lexer)
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Not,
    UnaryMinus,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Nil,

    Identifier(String),
    FloatLiteral(f64),
    IntLiteral(i64),
    StringLiteral(String),
    BooleanLiteral(bool),

    Assign(String, Box<Expr>),
    Function(Function),
    Class(String,Vec<String>, Vec<Method>,),
    Call(Box<Expr>, Vec<Expr>),

    UnaryOp(UnaryOp, Box<Expr>),
    BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
}
#[derive(Debug,Clone,PartialEq)]
pub struct Function {
    pub params: Vec<String>,
    pub stmts: Vec<Stmt>,
}
#[derive(Debug,Clone,PartialEq)]
pub struct Method {
    params: Vec<String>,
    stmts: Vec<Stmt>,
    class: String,
}

use std::collections::HashMap;

pub struct Class {
    methods: Vec<Method>,
    vars: Vec<HashMap<String, Expr>>
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Return(Expr),
    ExprStmt(Expr),
    Declaration(String, Expr),
    If(Expr, Vec<Stmt>, Vec<Stmt>),
    While(Expr, Vec<Stmt>),
}

