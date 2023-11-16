use crate::{parser::SchemeExp, env::Env};

use super::{ Evaluator, Cell };

/**
 * 语法：
 * 函数调用：
 * (vhello "Hello world")
 */
impl Evaluator {
  pub fn parse_proc(&self, node: SchemeExp, env: Env) -> usize {
    panic!()
  }

  pub fn eval_proc(&mut self) -> Option<Cell> {
    None
  }
}