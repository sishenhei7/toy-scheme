use crate::{parser::SchemeExp, env::Env};

use super::{ Evaluator, Cell };

/**
 * 语法：
 * (if predicate then_value else_value)
 */
impl Evaluator {
  pub fn parse_if_clause(&self, node: SchemeExp, env: Env) -> () {
    ()
  }

  pub fn eval_if_clause(&mut self) -> Option<Cell> {
    None
  }
}