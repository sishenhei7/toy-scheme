use std::cell::RefCell;
use std::rc::Rc;

use crate::{
  env::Env,
  evaluator::{IEvaluator},
  parser::{BaseSchemeData, SchemeCont, SchemeData, SchemeExp},
};

pub struct ProcEvaluator;

impl IEvaluator for ProcEvaluator {
  fn can_match(&self, data: &SchemeExp) -> bool {
    true
  }
  fn evaluate(
    &self,
    data: &SchemeExp,
    env: &Env,
    cont: &SchemeCont,
  ) -> SchemeData {
    SchemeData::new(BaseSchemeData::Nil)
  }
}
