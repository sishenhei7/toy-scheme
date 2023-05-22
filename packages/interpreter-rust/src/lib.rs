pub mod closure;
pub mod env;
pub mod evaluator;
pub mod lexer;
pub mod parser;

use std::cell::RefCell;
use std::rc::Rc;

// use napi_derive::napi;
use closure::*;
use evaluator::*;
use lexer::*;
use parser::*;
use env::*;

// #[napi(custom_finalize)]
pub struct Interpreter {
  node: SchemeCont,
}

pub struct InterpreterError;

impl Interpreter {
  // #[napi(constructor)]
  pub fn new(program: String) -> Result<Self, InterpreterError> {
    let token_list = tokenize(&program).or(Err(InterpreterError))?;
    let scheme_exp = parse(token_list).or(Err(InterpreterError))?;
    let evaluator = Evaluator::new();
    let node = evaluator.evaluate_exp(&scheme_exp, &Rc::new(RefCell::new(Env::new())), &SchemeCont { func: Closure::new(|x| x), env: None, data: None, loc: None }).or(Err(InterpreterError))?;
    Ok(Self { node })
  }
}
