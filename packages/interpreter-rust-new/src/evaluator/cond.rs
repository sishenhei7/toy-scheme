use crate::{
  parser:: {
    SchemeExp,
    SchemeData
  },
  env::Env
};

use super::{ Evaluator, Unit };

/**
 * 语法：
 * (cond
 *     (predicate_1 clauses_1)
 *     (predicate_2 clauses_2)
 *     (else        clauses_else))
 */
impl Evaluator {
  pub fn evaluate_cond(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    node.value.pop_front();

    node.value.into_iter().rev().fold(next, |acc, mut cur| {
      let exp_list = cur.get_exp_list().expect("Parse cond-list error!");
      let predict = exp_list.pop_front().expect("Parse cond-predict error!");
      let value = exp_list.pop_front().expect("Parse cond-clause error!");
      if let SchemeData::Identifier(ref y) = predict {
        if y.value == "else" {
          self.parse(value, env.copy(), next)
        } else {
          panic!("Parse cond-else error!")
        }
      } else {
        let value_cid = self.parse(value, env.copy(), next);
        let cond_cid = self.insert_map(Unit::new(
          env.copy(),
          None,
          Box::new(move |mut x| {
            let predict = x.get_boolean().expect("Cond-predict should be boolean!");
            let next = if predict { value_cid } else { acc };
            (next, SchemeData::Nil)
          })
        ));
        self.parse(predict, env.copy(), cond_cid)
      }
    })
  }
}