pub mod boxing;
pub mod closure;
pub mod env;
pub mod evaluator;
pub mod lexer;
pub mod parser;

use std::cell::RefCell;
use std::rc::Rc;
use once_cell::sync::Lazy;

use anyhow::Error;
// use napi_derive::napi;
use closure::*;
use env::*;
use evaluator::*;
use lexer::*;
use parser::*;

static EVALUATOR: Lazy<Evaluator> = Lazy::new(|| Evaluator::new());

// #[napi(custom_finalize)]
pub struct Interpreter {
  node: SchemeCont
}

struct StepResponse {
  range: Option<Location>,
  stack: Vec<String>,
  scope: Vec<String>,
}

impl Interpreter {
  // #[napi(constructor)]
  pub fn new(program: String) -> Result<Self, Error> {
    let token_list = tokenize(&program)?;
    let scheme_exp = parse(token_list)?;
    let initial_env = Env::new();
    let initial_cont = SchemeCont {
      func: Closure::new(|x| x),
      env: initial_env.clone(),
      data: None,
      loc: None,
    };
    let node = EVALUATOR.evaluate_exp(&scheme_exp, &initial_env, &initial_cont)?;
    Ok(Self { node })
  }

  // pub fn run(&self) -> String {
  //   let res = self.node.func.call(self.node.data);
  //   "fdsaf".to_string()
  // }

  // pub fn step() -> StepResponse {

  // }
}
