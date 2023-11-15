use crate::{parser::SchemeExp, env::Env};

use super::{ Evaluator, Cell };

/**
 * 语法：
 * (cont xx)
 */
impl Evaluator {
  pub fn parse_cont(&self, node: SchemeExp, env: Env) -> () {
    ()
  }

  pub fn eval_cont(&mut self) -> Option<Cell> {
    None
  }
}