use crate::{parser::{SchemeExp, SchemeData}, env::Env};

use super::{ Evaluator, Unit };

/**
 * 语法(**此语句没有返回值，没有返回值的意思是，之前的和自己的返回值都丢弃！**)：
 * (set! var (* var 10))
 */
impl Evaluator {
  pub fn evaluate_set(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    node.value.pop_front();

    let name_node = node.value.pop_front().expect("Evaluate set-name error!");
    let name = name_node.get_identifier_string().expect("Evaluate set-name error!");
    let value_node = node.value.pop_front().expect("Evaluate set-value error!");


    let mut env_copy = env.copy();
    let name_copy = name.clone();
    let set_cid = self.insert_map(Unit::new(
      env.copy(),
      None,
      Box::new(move |v, _| {
        env_copy.set(&name_copy, v.clone());
        (next, SchemeData::Nil)
      })
    ));

    self.evaluate(value_node, env, set_cid)
  }
}

#[cfg(test)]
mod tests {
  use crate::{Interpreter, build_number, SchemeData, SchemeNumber};

  #[test]
  fn test_set() -> () {
    let program = "
    (define x 0)
    (set! x 3)
    x
    ";
    let mut interpreter = Interpreter::new(program.to_string());
    let mut result = interpreter.run();
    let mut expect = build_number!(3 as f64, None);
    assert_eq!(result.is_equal(&mut expect), true);
  }
}