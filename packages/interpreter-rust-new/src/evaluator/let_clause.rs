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
    let def_list = def_node.get_exp_list().expect("Evaluate let-def error!");
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
    let body_cid = self.evaluate(body_node, env.copy(), next);

    def_list.into_iter().rev().fold(body_cid, |acc, cur| {
      let definition = cur.get_exp_list().expect("Evaluate let-definition error!");
      let name_node = definition.pop_front().expect("Evaluate let-definition-name error!");
      let name = name_node.get_identifier_string().expect("Evaluate let-definition-name error!");
      let value_node = definition.pop_front().expect("Evaluate let-definition-value error!");

      let name_copy = name.clone();
      let mut current_env = env.copy();
      let def_cid = self.insert_map(Unit::new(
        env.copy(),
        None,
        Box::new(move |mut v| {
          let item = v.pop().expect("Evaluate Unit-let error!");
          current_env.set(&name_copy, item.clone());
          (acc, v)
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
    let definition = definition_node.get_exp_list().expect("Evaluate let-definition error!");
    let name_node = definition.pop_front().expect("Evaluate let-definition-name error!");
    let name = name_node.get_identifier_string().expect("Evaluate let-definition-name error!");
    let value_node = definition.pop_front().expect("Evaluate let-definition-value error!");

    let name_copy = name.clone();
    let mut current_env = env.clone();
    let current_next = self.evaluate_let_star(def_list, body_node, env.extend(None), next);
    let def_cid = self.insert_map(Unit::new(
      env.copy(),
      None,
      Box::new(move |mut v| {
        let item = v.pop().expect("Evaluate Unit-let error!");
        current_env.set(&name_copy, item.clone());
        (current_next, v)
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
      let definition = item.get_exp_list().expect("Evaluate let-definition error!");
      let name_node = definition.front().expect("Evaluate let-definition error!");
      let name = name_node.get_identifier_string().expect("Evaluate let-name error!");
      env_copy.set(&name.clone(), SchemeData::Nil);
    }
    self.evaluate_let_normal(def_list, body_node, env_copy, next)
  }
}
