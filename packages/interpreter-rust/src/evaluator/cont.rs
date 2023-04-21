use crate::evaluator::IEvaluator;
use crate::parser::SchemeData;

pub struct ContEvaluator;

impl IEvaluator for ContEvaluator {
  fn can_match(&self) -> bool {
    true
  }
  fn evaluate(&self) -> SchemeData {
    SchemeData::Nil
  }
}