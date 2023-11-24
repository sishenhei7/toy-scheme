use crate::{parser::{SchemeExp, SchemeData}, env::Env};

use super::{ Evaluator, Unit };

/**
 * 语法：
 * (set! var (* var 10))
 */
impl Evaluator {
  pub fn evaluate_set(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    node.value.pop_front();

    let name_node = node.value.pop_front().expect("Parse set-name error!");
    let name = name_node.get_identifier_string().expect("Parse set-name error!");
    let value_node = node.value.pop_front().expect("Parse set-value error!");


    let mut env_copy = env.copy();
    let name_copy = name.clone();
    let set_cid = self.insert_map(Unit::new(
      env.copy(),
      None,
      Box::new(move |x| {
        env_copy.set(&name_copy, x.clone());
        (next, SchemeData::Nil)
      })
    ));

    self.parse(value_node, env, set_cid)
  }
}