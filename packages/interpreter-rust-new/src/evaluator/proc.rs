use crate::{parser::{SchemeExp, SchemeData, SchemeProc}, env::Env};

use super::{ Evaluator, Unit };

/**
 * 语法：
 * 函数调用：
 * (vhello "Hello world")
 */
impl Evaluator {
  pub fn parse_proc(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    let identifier_data = node.value.pop_front().expect("Parse proc-identifier error!");
    let identifier = identifier_data.get_identifier_string().expect("Parse proc-identifier error!");
    let mut proc_data = env.get(&identifier).expect("Parse proc-data error!");
    let proc = proc_data.get_proc().expect("Parse proc-data error!");
    let proc_env = &mut proc.env;
    let params = proc.params.get_exp_list().expect("Parse proc-params error!");
    let mut argumentsData = node.value.pop_front().expect("Parse proc-arguments error!");
    let arguments = argumentsData.get_exp_list().expect("Parse proc-arguments error!");

    // parse params and arguments
    for (key, value) in params.iter().zip(arguments.iter()) {
      let key_str = key.get_identifier_string().expect("Parse proc-params error!");
      proc_env.set(key_str, value.clone());
    }

    // parse body
    self.parse((*proc.body).clone(), proc_env.clone(), next)
  }

  pub fn eval_proc(&mut self) -> Option<Unit> {
    None
  }
}