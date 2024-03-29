use crate::{parser::{SchemeExp, SchemeData, SchemeProc}, env::Env};

use super::{ Evaluator, Unit };

/**
 * 语法(**此语句有返回值**)：
 * (lambda (a b c) (+ a b c))
 */
impl Evaluator {
  pub fn evaluate_lambda(&mut self, node: SchemeExp, env: Env, next: usize) -> usize {
    let loc = node.loc.clone();
    let proc = SchemeData::Procedure(self.evaluate_lambda_to_proc(node, env.copy()));
    self.insert_map(Unit::new(
      env.copy(),
      loc,
      Box::new(move |_, _| (next, proc.clone()))
    ))
  }

  pub fn evaluate_lambda_to_proc(&mut self, mut node: SchemeExp, env: Env) -> SchemeProc {
    node.value.pop_front();

    let params = node.value.pop_front().expect("Evaluate lambda-params error!");
    let body = node.value.pop_front().expect("Evaluate lambda-body error!");

    if matches!(params, SchemeData::Exp(..)) && matches!(body, SchemeData::Exp(..)) {
      // TODO: 这里每次执行可以用同一个 proc 的 env ？？？
      return SchemeProc {
        name: "<<lambda>>".to_string(),
        params: Box::new(params),
        body: Box::new(body),
        env: env.extend(None),
        loc: node.loc.clone()
      };
    }

    panic!("Evaluate lambda-clause error!");
  }
}