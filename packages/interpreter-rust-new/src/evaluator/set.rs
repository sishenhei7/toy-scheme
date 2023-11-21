use crate::{parser::SchemeExp, env::Env};

use super::{ Evaluator, Unit };

/**
 * 语法：
 * (set! var (* var 10))
 */
impl Evaluator {
  pub fn parse_set(&self, node: SchemeExp, env: Env, next: usize) -> usize {
    panic!()
  }

  pub fn eval_set(&mut self) -> Option<Unit> {
    None
  }
}