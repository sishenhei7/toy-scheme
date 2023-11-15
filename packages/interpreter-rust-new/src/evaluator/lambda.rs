use crate::{parser::SchemeExp, env::Env};

use super::{ Evaluator, Cell };

/**
 * 语法：
 * (lambda (a b c) (+ a b c))
 */
impl Evaluator {
  pub fn parse_lambda(&self, node: SchemeExp, env: Env) -> () {
    ()
  }

  pub fn eval_lambda(&mut self) -> Option<Cell> {
    None
  }
}