use crate::{parser::SchemeExp, env::Env};

use super::Evaluator;

/**
 * 语法：
 * (cont xx)
 */
impl Evaluator {
  pub fn evaluate_cont(&mut self, mut node: SchemeExp, env: Env, _next: usize) -> usize {
    let mut cont = node.value.pop_front().expect("Evaluate cond error!");
    let cont_cid = cont.get_cont_value().expect("Evaluate cond error!");
    let value_node = node.value.pop_front().expect("Evaluate cond-value error!");
    self.evaluate(value_node, env, cont_cid)
  }
}