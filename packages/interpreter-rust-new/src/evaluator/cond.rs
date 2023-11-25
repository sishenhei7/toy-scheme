use crate::{
  parser:: {
    SchemeExp,
    SchemeData
  },
  env::Env
};

use super::{ Evaluator, Unit };

/**
 * 语法(**此表达式有返回值**)：
 * (cond
 *     (predicate_1 clauses_1)
 *     (predicate_2 clauses_2)
 *     (else        clauses_else))
 */
impl Evaluator {
  pub fn evaluate_cond(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    node.value.pop_front();

    node.value.into_iter().rev().fold(next, |acc, mut cur| {
      let exp_list = cur.get_exp_list().expect("Evaluate cond-list error!");
      let predict = exp_list.pop_front().expect("Evaluate cond-predict error!");
      let value = exp_list.pop_front().expect("Evaluate cond-clause error!");
      if let SchemeData::Identifier(ref y) = predict {
        if y.value == "else" {
          self.evaluate(value, env.copy(), next)
        } else {
          panic!("Evaluate cond-else error!")
        }
      } else {
        let value_cid = self.evaluate(value, env.copy(), next);
        let cond_cid = self.insert_map(Unit::new(
          env.copy(),
          None,
          Box::new(move |mut v| {
            let mut item = v.pop().expect("Evaluate Unit-cond error!");
            let predict = item.get_boolean().expect("Cond-predict should be boolean!");
            let next = if predict { value_cid } else { acc };
            (next, v)
          })
        ));
        self.evaluate(predict, env.copy(), cond_cid)
      }
    })
  }
}