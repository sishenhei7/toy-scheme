use crate::{parser::SchemeExp, env::Env};

use super::Evaluator;

/**
 * 语法(**此表达式有部分返回值，有部分返回值的意思是，每条语句只丢弃之前的返回值！**)：
 * (begin
 *    (set! amount m)
 *    (set! amount n)
 *    amount)
 */
impl Evaluator {
  pub fn evaluate_begin(&mut self, node: SchemeExp, env: Env, next: usize) -> usize {
    let mut queue = node.value;
    queue.pop_front();
    self.evaluate_from_left(queue, env, next)
  }
}