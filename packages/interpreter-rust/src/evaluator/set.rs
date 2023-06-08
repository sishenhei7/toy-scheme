use std::cell::RefCell;
use std::rc::Rc;

use crate::{
  env::Env,
  evaluator::{Evaluator, IEvaluator},
  parser::{BaseSchemeData, SchemeCont, SchemeData, SchemeExp},
};

pub struct SetEvaluator;

impl IEvaluator for SetEvaluator {
  fn can_match(&self, data: &SchemeExp) -> bool {
    true
  }
  fn evaluate(
    &self,
    data: &SchemeExp,
    env: &Env,
    cont: &SchemeCont,
    base_evaluator: &Evaluator,
  ) -> SchemeData {
    SchemeData::new(BaseSchemeData::Nil)
  }
}
