use crate::{parser::{SchemeExp, SchemeData}, env::Env};

use super::{ Evaluator, Unit };

/**
 * 语法：
 * (if predict then_value else_value)
 */
impl Evaluator {
  pub fn evaluate_if(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    node.value.pop_front();

    let predict_node = node.value.pop_front().expect("Parse predict-clause error!");
    let then_node = node.value.pop_front().expect("Parse then-clause error!");
    let else_node = node.value.pop_front().expect("Parse else-clause error!");
    let then_cid = self.parse(then_node, env.copy(), next);
    let else_cid = self.parse(else_node, env.copy(), next);

    let if_cid = self.insert_map(Unit::new(
      env.copy(),
      None,
      Box::new(move |mut x, _| {
        let predict = x.get_boolean().expect("Cond-predict should be boolean!");
        let next = if predict { then_cid } else { else_cid };
        (next, SchemeData::Nil)
      })
    ));

    self.parse(predict_node, env.copy(), if_cid)
  }
}