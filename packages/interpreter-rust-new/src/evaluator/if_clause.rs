use crate::{parser::{SchemeExp, SchemeData}, env::Env};

use super::{ Evaluator, Unit };

/**
 * 语法(**此语句有返回值**)：
 * (if predict then_value else_value)
 */
impl Evaluator {
  pub fn evaluate_if(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    node.value.pop_front();

    let predict_node = node.value.pop_front().expect("Evaluate predict-clause error!");
    let then_node = node.value.pop_front().expect("Evaluate then-clause error!");
    let else_node = node.value.pop_front().expect("Evaluate else-clause error!");
    let then_cid = self.evaluate(then_node, env.copy(), next);
    let else_cid = self.evaluate(else_node, env.copy(), next);

    let if_cid = self.insert_map(Unit::new(
      env.copy(),
      None,
      Box::new(move |mut v, _| {
        let predict = v.get_boolean().expect("Cond-predict should be boolean!");
        let next = if predict { then_cid } else { else_cid };
        (next, SchemeData::Nil)
      })
    ));

    self.evaluate(predict_node, env.copy(), if_cid)
  }
}

#[cfg(test)]
mod tests {
  use crate::{Interpreter, build_number, SchemeData, SchemeNumber};

  #[test]
  fn test_if_clause() -> () {
    let program = "
    (if (zero? 1) 1 3)
    ";
    let mut interpreter = Interpreter::new(program.to_string());
    let mut result = interpreter.run();
    let mut expect = build_number!(3 as f64, None);
    assert_eq!(result.is_equal(&mut expect), true);
  }
}