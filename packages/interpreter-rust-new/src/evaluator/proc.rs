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
    let arguments = arguments_data.get_exp_list().expect("Evaluate proc-arguments error!");
    self.evaluate_proc_with_arguments(proc, arguments, env, next)
  }

  pub fn evaluate_proc_with_arguments(&mut self, proc: &mut SchemeProc, arguments: &mut VecDeque<SchemeData>, env: Env, next: usize) -> usize {
    let proc_env = &mut proc.env;
    let params = proc.params.get_exp_list().expect("Evaluate proc-params error!");

    // Evaluate params and arguments
    for (key, value) in params.iter().zip(arguments.iter()) {
      let key_str = key.get_identifier_string().expect("Evaluate proc-params error!");
      proc_env.set(key_str, value.clone());
    }

    // Evaluate body
    self.evaluate((*proc.body).clone(), proc_env.clone(), next)
  }
}