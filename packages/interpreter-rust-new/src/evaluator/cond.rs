use crate::{
  parser:: {
    SchemeExp,
    SchemeData
  },
  env::Env
};

use super::{ Evaluator, Unit, UnitName };

/**
 * 语法：
 * (cond
 *     (predicate_1 clauses_1)
 *     (predicate_2 clauses_2)
 *     (else        clauses_else))
 */
impl Evaluator {
  pub fn parse_cond(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    node.value.pop_front();

    node.value.into_iter().rev().fold(next, |acc, mut cur| {
      if let SchemeData::Exp(ref mut x) = cur {
        let predict = x.value.pop_front().unwrap();
        let value = x.value.pop_front().unwrap();

        if let SchemeData::Identifier(ref y) = predict {
          if y.value == "else" {
            return self.parse(value, env.copy(), next)
          } else {
            panic!("Parse cond-else error!")
          }
        } else {
          let value_cid = self.parse(value, env.copy(), next);
          let cond_cid = self.insert_map(Unit::new(
            UnitName::IfElse,
            vec![],
            env.copy(),
            None,
            vec![value_cid, acc]
          ));
          return self.parse(predict, env.copy(), cond_cid)
        }
      }

      panic!("Parse cond-clause error!")
    })
  }

  pub fn eval_cond(&mut self) -> Option<Unit> {
    None
  }
}