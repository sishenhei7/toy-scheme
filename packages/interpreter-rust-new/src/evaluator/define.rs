use crate::{parser::{SchemeExp, SchemeData, SchemeProc}, env::Env};

use super::{ Evaluator, Unit };

/**
 * 语法(**此语句没有返回值，没有返回值的意思是，之前的和自己的返回值都丢弃！**)：
 * 1.定义变量或函数：
 * (define vhello "Hello world")
 * 2.短形式定义函数：
 * (define (hello name) (string-append "Hello " name "!"))
 */
impl Evaluator {
  pub fn evaluate_define(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    node.value.pop_front();

    let first = node.value.pop_front().expect("Evaluate define-var error!");

    // 定义变量或函数
    if let SchemeData::Identifier(ref x) = first {
      let identifier = x.value.clone();
      let mut env_copy = env.copy();
      let set_cid = self.insert_map(Unit::new(
        env.copy(),
        None,
        Box::new(move |v, _| {
          env_copy.set(&identifier, v.clone());
          (next, SchemeData::Nil)
        })
      ));
      let second: SchemeData = node.value.pop_front().expect("Evaluate define-value error!");
      return self.evaluate(second, env, set_cid)
    }

    // 短形式定义函数
    let first_cloned = first.clone();
    if let SchemeData::Exp(..) = first {
      let mut second = node.value.pop_front().expect("Evaluate define-proc error!");
      let second_list = second.get_exp_list().expect("Evaluate define-proc error!");
      let name_node = second_list.pop_front().expect("Evaluate define-proc-name error!");
      let name = name_node.get_identifier_string().expect("Evaluate define-proc-name error!");
      let name_copy = name.clone();

      let proc = SchemeData::Procedure(SchemeProc {
        name: name.to_string(),
        params: Box::new(first_cloned),
        body: Box::new(second.clone()),
        env: env.extend(None),
        loc: node.loc.clone()
      });
      let mut env_copy = env.copy();
      return self.insert_map(Unit::new(
        env.copy(),
        None,
        Box::new(move |_, _| {
          env_copy.set(&name_copy, proc.clone());
          (next, SchemeData::Nil)
        })
      ));
    }

    panic!("Evaluate cond-else error!")
  }
}