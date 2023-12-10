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
  result: SchemeData
}

// struct StepResponse {
//   range: Option<Location>,
//   stack: Vec<String>,
//   scope: Vec<String>,
// }

impl Interpreter {
  // #[napi(constructor)]
  pub fn new(program: String) -> Result<Self, Error> {
    // Ok(Self {})
    let token_list = Lexer::new(&program).collect::<Vec<TokenItem>>();
    let data = parse(token_list).expect("输入的 program 有误！");
    let mut evaluator = Evaluator::new(data);
    let result = evaluator.run();
    Ok(Self { result })
  }

  // pub fn run(&self) -> String {
  //   let res = self.node.func.call(self.node.data);
  //   "fdsaf".to_string()
  // }

  // pub fn step() -> StepResponse {

  // }
}
