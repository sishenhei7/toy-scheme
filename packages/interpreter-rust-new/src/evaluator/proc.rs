use std::collections::VecDeque;

use crate::{parser::{SchemeExp, SchemeData, SchemeProc}, env::Env};

use super::{ Evaluator, Unit };

/**
 * 语法(**此语句有返回值**)：
 * 函数调用：
 * (vhello "Hello world")
 */
impl Evaluator {
  pub fn evaluate_proc(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    let identifier_data = node.value.pop_front().expect("Evaluate proc-identifier error!");
    let identifier = identifier_data.get_identifier_string().expect("Evaluate proc-identifier error!");
    let mut proc_data = env.get(&identifier).expect("Evaluate proc-data error!");
    let proc = proc_data.get_proc().expect("Evaluate proc-data error!");
    let mut arguments_data = node.value.pop_front().expect("Evaluate proc-arguments error!");
    let arguments = arguments_data.get_exp_queue().expect("Evaluate proc-arguments error!");
    self.evaluate_proc_with_arguments(proc, arguments, env, next)
  }

  pub fn evaluate_proc_with_arguments(&mut self, proc: &mut SchemeProc, arguments: &mut VecDeque<SchemeData>, env: Env, next: usize) -> usize {
    let proc_env = &mut proc.env;
    let params = proc.params.get_exp_queue().expect("Evaluate proc-params error!");
    let body_cid = self.evaluate((*proc.body).clone(), proc_env.clone(), next);

    params.iter().zip(arguments.iter()).rev().fold(body_cid, |acc, cur| {
      let (key, value) = cur;
      let key_clone = key.clone();
      let mut env_clone = env.copy();
      let argument_cid = self.insert_map(Unit::new(
        env.copy(),
        None,
        Box::new(move |v, _| {
          let key_str = key_clone.get_identifier_string().expect("Evaluate proc-params error!");
          env_clone.set(key_str, v.clone());
          (acc, SchemeData::Nil)
        })
      ));

      self.evaluate(value.clone(), env.copy(), argument_cid)
    })
  }
}
