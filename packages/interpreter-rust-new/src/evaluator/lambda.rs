use crate::{parser::{SchemeExp, SchemeData, SchemeProc}, env::Env};

use super::{ Evaluator, Unit, UnitName };

/**
 * 语法：
 * (lambda (a b c) (+ a b c))
 */
impl Evaluator {
  pub fn parse_lambda(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    node.value.pop_front();

    let params = node.value.pop_front().unwrap();
    let body = node.value.pop_front().unwrap();
    if matches!(params, SchemeData::Exp(..)) && matches!(body, SchemeData::Exp(..)) {
      let proc = SchemeData::Procedure(SchemeProc {
        name: "<<lambda>>".to_string(),
        params: Box::new(params),
        body: Box::new(body),
        env: env.extend(None),
        loc: node.loc.clone()
      });

      return self.insert_map(Unit::new(
        UnitName::Value,
        vec![proc],
        env.copy(),
        node.loc.clone(),
        vec![next]
      ));
    }

    panic!("Parse lambda-clause error!");
  }

  pub fn eval_lambda(&mut self) -> Option<Unit> {
    None
  }
}