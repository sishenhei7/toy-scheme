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
      let exp_list = cur.get_exp_queue().expect("Evaluate cond-list error!");
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
          Box::new(move |mut v, _| {
            let predict = v.get_boolean().expect("Cond-predict should be boolean!");
            let next = if predict { value_cid } else { acc };
            (next, SchemeData::Nil)
          })
        ));
        self.evaluate(predict, env.copy(), cond_cid)
      }
    })
  }
}

#[cfg(test)]
mod tests {
  use crate::{Interpreter, build_number, SchemeData, SchemeNumber};

  #[test]
  fn test_cond() -> () {
    let program = "
    (cond
      ((zero? 1) 1)
      ((zero? 2) 2)
      (else (+ 1 2)))
    ";
    let mut interpreter = Interpreter::new(program.to_string());
    let mut result = interpreter.run();
    let mut expect = build_number!(3 as f64, None);
    assert_eq!(result.is_equal(&mut expect), true);
  }
}