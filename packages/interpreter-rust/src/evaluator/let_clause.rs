use std::cell::RefCell;
use std::rc::Rc;

use crate::{
  env::Env,
  evaluator::{Evaluator, IEvaluator},
  parser::{BaseSchemeData, SchemeCont, SchemeData, SchemeExp},
};

pub struct LetEvalEvaluator;

impl IEvaluator for LetEvalEvaluator {
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
