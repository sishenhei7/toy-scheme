use crate::{parser::SchemeExp, env::Env};

use super::{ Evaluator, Cell };

/**
 * 语法：
 * (begin
 *    (set! amount m)
 *    (set! amount n)
 *    amount)
 */
impl Evaluator {
  pub fn parse_begin(&self, node: SchemeExp, env: Env) -> () {
    ()
  }

  pub fn eval_begin(&mut self) -> Option<Cell> {
    None
  }
}