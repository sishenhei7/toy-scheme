use crate::{parser::SchemeExp, env::Env};

use super::{ Evaluator, Unit };

/**
 * 语法：
 * (cont xx)
 */
impl Evaluator {
  pub fn parse_cont(&self, node: SchemeExp, env: Env, next: usize) -> usize {
    panic!()
  }

  pub fn eval_cont(&mut self) -> Option<Unit> {
    None
  }
}