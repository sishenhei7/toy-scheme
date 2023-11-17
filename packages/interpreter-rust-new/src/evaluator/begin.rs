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
  pub fn parse_begin(&mut self, node: SchemeExp, env: Env, next: usize) -> usize {
    let mut queue = node.value;
    queue.pop_front();
    self.parse_from_left(queue, env, next)
  }

  pub fn eval_begin(&mut self) -> Option<Cell> {
    None
  }
}