use crate::{parser::SchemeExp, env::Env};

use super::{ Evaluator, Unit };

/**
 * 语法：
 * (cont xx)
 */
impl Evaluator {
  pub fn parse_cont(&mut self, mut node: SchemeExp, env: Env, _next: usize) -> usize {
    let mut cont = node.value.pop_front().expect("Parse cond error!");
    let cont_cid = cont.get_cont_value().expect("Parse cond error!");
    let value_node = node.value.pop_front().expect("Parse cond-value error!");
    self.parse(value_node, env, cont_cid)
  }

  pub fn eval_cont(&mut self) -> Option<Unit> {
    None
  }
}