use crate::{parser::SchemeExp, env::Env};

use super::{ Evaluator, Unit };

/**
 * 语法：
 * 函数调用：
 * (vhello "Hello world")
 */
impl Evaluator {
  pub fn parse_proc(&self, node: SchemeExp, env: Env, next: usize) -> usize {
    panic!()
  }

  pub fn eval_proc(&mut self) -> Option<Unit> {
    None
  }
}