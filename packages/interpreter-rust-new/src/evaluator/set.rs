use crate::{parser::SchemeExp, env::Env};

use super::{ Evaluator, Cell };

/**
 * 语法：
 * (set! var (* var 10))
 */
impl Evaluator {
  pub fn parse_set(&self, node: SchemeExp, env: Env) -> () {
    ()
  }

  pub fn eval_set(&mut self) -> Option<Cell> {
    None
  }
}