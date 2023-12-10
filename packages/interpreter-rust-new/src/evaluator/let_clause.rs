use std::collections::VecDeque;

use crate::{parser::{SchemeExp, SchemeData}, env::Env};

use super::{ Evaluator, Unit };

/**
 * 语法(**此语句有返回值**)：
 * 1.let 表达式：
 * (let ((i 1) (j 2))
 *    (+ i j))
 * 2.let* 表达式(可以在定义中引用之前定义的变量)：
 * (let* ((i 1) (j (+ i 2)))
 *    (* i j))
 * 3.letrec 表达式(可以在定义中递归的引用自己)：
 * (letrec ((iter (lambda (ls0 n)
 *    (if (null? ls0)
 *    n
 *    (iter (cdr ls0) (+ (car ls0) n))))))
 *  (iter ls 0))
 */
impl Evaluator {
  pub fn evaluate_let(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    let identifier_node = node.value.pop_front().expect("Evaluate let-symbol error!");
    let identifier = identifier_node.get_identifier_string().expect("Evaluate let-symbol error!");
    let new_env = env.extend(None);
    let mut def_node = node.value.pop_front().expect("Evaluate let-def error!");
    let def_list = def_node.get_exp_queue().expect("Evaluate let-def error!");
    let body_node = node.value.pop_front().expect("Evaluate let-body error!");

    match identifier.as_ref() {
      "let" => self.evaluate_let_normal(def_list, body_node, new_env, next),
      "let*" => self.evaluate_let_star(def_list, body_node, new_env, next),
      "letrec" => self.evaluate_letrec(def_list, body_node, new_env, next),
      _ => panic!("Evaluate let-symbol error!")
    }
  }

  /**
   * ((lambda(a b c)  ... body ...)
   *    a-value
   *    b-value
   *    c-value)
   */
  pub fn evaluate_let_normal(
    &mut self,
    def_list: &mut VecDeque<SchemeData>,
    body_node: SchemeData,
    env: Env,
    next: usize
  ) -> usize {
    println!("evaluate_let_normal{:?}", &body_node);
    let body_cid = self.evaluate(body_node, env.copy(), next);

    def_list.into_iter().rev().fold(body_cid, |acc, cur| {
      let definition = cur.get_exp_queue().expect("Evaluate let-definition error!");
      let name_node = definition.pop_front().expect("Evaluate let-definition-name error!");
      let name = name_node.get_identifier_string().expect("Evaluate let-definition-name error!");
      let value_node = definition.pop_front().expect("Evaluate let-definition-value error!");

      let name_copy = name.clone();
      let mut current_env = env.copy();
      println!("{:?},{:?}", &name, value_node);
      let def_cid = self.insert_map(Unit::new(
        env.copy(),
        None,
        Box::new(move |v, _| {
          current_env.define(&name_copy, v.clone());
          (acc, SchemeData::Nil)
        })
      ));

      let current_env_copy = env.clone();
      self.evaluate(value_node, current_env_copy, def_cid)
    })
  }

  /**
   * ((lambda(a)
   *   ((lambda(b)
   *     ((lambda(c) ... body ...)
   *     c-value))
   *   b-value))
   * c-value)
   */
  pub fn evaluate_let_star(
    &mut self,
    def_list: &mut VecDeque<SchemeData>,
    body_node: SchemeData,
    env: Env,
    next: usize
  ) -> usize {
    if !def_list.is_empty() {
      return self.evaluate(body_node, env.copy(), next)
    }

    let mut definition_node = def_list.pop_front().expect("Evaluate let-definition error!");
    let definition = definition_node.get_exp_queue().expect("Evaluate let-definition error!");
    let name_node = definition.pop_front().expect("Evaluate let-definition-name error!");
    let name = name_node.get_identifier_string().expect("Evaluate let-definition-name error!");
    let value_node = definition.pop_front().expect("Evaluate let-definition-value error!");

    let name_copy = name.clone();
    let mut current_env = env.clone();
    let current_next = self.evaluate_let_star(def_list, body_node, env.extend(None), next);
    let def_cid = self.insert_map(Unit::new(
      env.copy(),
      None,
      Box::new(move |v, _| {
        current_env.define(&name_copy, v.clone());
        println!("{:?},{:?}", &name_copy, current_env);
        (current_next, SchemeData::Nil)
      })
    ));

    let current_env_copy = env.clone();
    self.evaluate(value_node, current_env_copy, def_cid)
  }

  /**
   * (let ((a *undefined*) (b *undefined*) (c *undefined*))
   *   (set! a a-value)
   *   (set! b b-value)
   *   (set! c c-value)
   *   ... body ... )
   */
  pub fn evaluate_letrec(
    &mut self,
    def_list: &mut VecDeque<SchemeData>,
    body_node: SchemeData,
    env: Env,
    next: usize
  ) -> usize {
    let mut env_copy = env.copy();
    for item in def_list.iter_mut() {
      let definition = item.get_exp_queue().expect("Evaluate let-definition error!");
      let name_node = definition.front().expect("Evaluate let-definition error!");
      let name = name_node.get_identifier_string().expect("Evaluate let-name error!");
      env_copy.define(&name.clone(), SchemeData::Nil);
    }
    self.evaluate_let_normal(def_list, body_node, env_copy, next)
  }
}

#[cfg(test)]
mod tests {
  use crate::{Interpreter, build_number, SchemeData, SchemeNumber};

  #[test]
  fn test_let_normal() -> () {
    let mut interpreter = Interpreter::new("(let ((x 1) (y 2)) (+ x y))".to_string());
    let mut result = interpreter.run();
    let mut expect = build_number!(3 as f64, None);
    assert_eq!(result.is_equal(&mut expect), true);
  }

  // #[test]
  // fn test_let_star() -> () {
  //   let mut interpreter = Interpreter::new("(let* ((x 1) (y (+ x 2))) (* x y))".to_string());
  //   let mut result = interpreter.run();
  //   let mut expect = build_number!(3 as f64, None);
  //   assert_eq!(result.is_equal(&mut expect), true);
  // }

  // #[test]
  // fn test_letrec() -> () {
  //   let mut interpreter = Interpreter::new("(letrec ((iter (lambda (x n) (if (zero? n) x (iter (+ x n) (- n 1)))))) (iter 0 5))".to_string());
  //   let mut result = interpreter.run();
  //   let mut expect = build_number!(3 as f64, None);
  //   assert_eq!(result.is_equal(&mut expect), true);
  // }
}
