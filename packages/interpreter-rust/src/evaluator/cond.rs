use std::cell::RefCell;
use std::rc::Rc;

use crate::{
  env::Env,
  evaluator::{Evaluator, IEvaluator},
  parser::{SchemeCont, SchemeData, SchemeExp}
};

pub struct CondEvaluator;

impl IEvaluator for CondEvaluator {
  fn can_match(&self, data: &SchemeExp) -> bool {
    true
  }
  fn evaluate(
    &self,
    data: &SchemeExp,
    env: &Rc<RefCell<Env>>,
    cont: &SchemeCont,
    base_evaluator: &Evaluator,
  ) -> SchemeData {
    SchemeData::Nil
  }
}
