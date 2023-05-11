use std::cell::RefCell;
use std::rc::Rc;

use crate::env::Env;
use crate::evaluator::{Evaluator, IEvaluator};
use crate::parser::{SchemeCont, SchemeData, SchemeExp};

pub struct LetEvalEvaluator;

impl IEvaluator for LetEvalEvaluator {
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
