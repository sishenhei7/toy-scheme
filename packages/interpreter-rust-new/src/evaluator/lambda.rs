use crate::{parser::{SchemeExp, SchemeData, SchemeProc}, env::Env};

use super::{ Evaluator, Unit };

/**
 * 语法：
 * (lambda (a b c) (+ a b c))
 */
impl Evaluator {
  pub fn parse_lambda(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    let loc = node.loc.clone();
    let proc = SchemeData::Procedure(self.parse_lambda_to_proc(node, env.copy()));
    self.insert_map(Unit::new(
      env.copy(),
      loc,
      // 这里的 proc.clone() 是否合法？
      Box::new(move |_, _| (next, proc.clone()))
    ))
  }

  pub fn parse_lambda_to_proc(&mut self, mut node: SchemeExp, env: Env) -> SchemeProc {
    node.value.pop_front();

    let params = node.value.pop_front().expect("Parse lambda-params error!");
    let body = node.value.pop_front().expect("Parse lambda-body error!");

    if matches!(params, SchemeData::Exp(..)) && matches!(body, SchemeData::Exp(..)) {
      return SchemeProc {
        name: "<<lambda>>".to_string(),
        params: Box::new(params),
        body: Box::new(body),
        env: env.extend(None),
        loc: node.loc.clone()
      };
    }

    panic!("Parse lambda-clause error!");
  }

  pub fn eval_lambda(&mut self) -> Option<Unit> {
    None
  }
}