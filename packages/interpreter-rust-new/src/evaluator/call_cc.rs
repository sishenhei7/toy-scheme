use std::collections::VecDeque;

use crate::{parser::{SchemeExp, SchemeData, SchemeCont}, env::Env};

use super::Evaluator;

/**
 * 语法（后面加一个 lambda 语句）(**此表达式有返回值**)：
 * (call-with-current-continuation
 *    (lambda (resume-here)
 *      (set! control-state resume-here)
 *      (return element)))
 */
impl Evaluator {
  pub fn evaluate_call_cc(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    node.value.pop_front();

    let lambda = node.value.pop_front().expect("Evaluate callcc-lambda error!");
    if let SchemeData::Exp(x) = lambda {
      let mut proc = self.evaluate_lambda_to_proc(x, env.clone());
      let cont = SchemeData::Continuation(SchemeCont {
        value: next,
        loc: None
      });
      return self.evaluate_proc_with_arguments(&mut proc, &mut VecDeque::from([cont]), env.copy(), next)
    }

    panic!("Evaluate callcc error!")
  }
}