#![feature(iter_advance_by)]

pub mod boxing;
pub mod env;
pub mod evaluator;
pub mod lexer;
pub mod parser;

use anyhow::Error;
// use napi_derive::napi;
use lexer::*;
use parser::*;
use evaluator::Evaluator;

// #[napi(custom_finalize)]
pub struct Interpreter {
  pub evaluator: Evaluator
}

// struct StepResponse {
//   range: Option<Location>,
//   stack: Vec<String>,
//   scope: Vec<String>,
// }

impl Interpreter {
  // #[napi(constructor)]
  pub fn new(program: String) -> Self {
    let token_list = Lexer::new(&program).collect::<Vec<TokenItem>>();
    let data = parse(token_list).expect("输入的 program 有误！");
    Interpreter { evaluator: Evaluator::new(data) }
  }

  pub fn run(&mut self) -> SchemeData {
    self.evaluator.run()
  }

  // pub fn run(&self) -> String {
  //   let res = self.node.func.call(self.node.data);
  //   "fdsaf".to_string()
  // }

  // pub fn step() -> StepResponse {

  // }
}
