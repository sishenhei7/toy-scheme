use crate::{parser::SchemeExp, env::Env};

use super::{ Evaluator, Cell };

/**
 * 语法：
 * (cond
 *     (predicate_1 clauses_1)
 *     (predicate_2 clauses_2)
 *     (else        clauses_else))
 */
impl Evaluator {
  pub fn parse_cond(&self, node: SchemeExp, env: Env, next: usize) -> usize {
    panic!()
  }

  pub fn eval_cond(&mut self) -> Option<Cell> {
    None
  }
}