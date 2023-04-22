use crate::evaluator::IEvaluator;
use crate::parser::SchemeData;

pub struct CallCcEvaluator;

impl IEvaluator for CallCcEvaluator {
  fn can_match(&self) -> bool {
    true
  }
  fn evaluate(&self) -> SchemeData {
    SchemeData::Nil
  }
}
