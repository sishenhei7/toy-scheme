use std::collections::VecDeque;

use crate::{parser::{SchemeExp, SchemeData, SchemeCont}, env::Env};

use super::{ Evaluator, Unit };

/**
 * 语法（后面加一个 lambda 语句）：
 * (call-with-current-continuation
 *    (lambda (resume-here)
 *      (set! control-state resume-here)
 *      (return element)))
 */
impl Evaluator {
  pub fn parse_call_cc(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    node.value.pop_front();

    let lambda = node.value.pop_front().expect("Parse callcc-lambda error!");
    if let SchemeData::Exp(x) = lambda {
      let mut proc = self.parse_lambda_to_proc(x, env.clone(), next);
      let cont = SchemeData::Continuation(SchemeCont {
        value: next,
        loc: None
      });
      return self.parse_proc_with_arguments(&mut proc, &mut VecDeque::from([cont]), env.copy(), next)
    }

    panic!("Parse callcc error!")
  }

  pub fn eval_call_cc(&mut self) -> Option<Unit> {
    None
  }
}