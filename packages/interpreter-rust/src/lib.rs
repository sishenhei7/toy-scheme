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

struct StepResponse {
  range: Option<Location>,
  stack: Vec<String>,
  scope: Vec<String>
}

impl Interpreter {
  // #[napi(constructor)]
  pub fn new(program: String) -> Result<Self, InterpreterError> {
    let token_list = tokenize(&program).or(Err(InterpreterError))?;
    let scheme_exp = parse(token_list).or(Err(InterpreterError))?;
    let evaluator = Evaluator::new();
    let initial_cont = SchemeCont { func: Closure::new(|x| x), env: None, data: None, loc: None };
    let node = evaluator.evaluate_exp(&scheme_exp, &Rc::new(RefCell::new(Env::new())), &initial_cont).or(Err(InterpreterError))?;
    Ok(Self { node })
  }

  // pub fn run(&self) -> String {
  //   let res = self.node.func.call(self.node.data);
  //   "fdsaf".to_string()
  // }

  // pub fn step() -> StepResponse {

  // }
}
