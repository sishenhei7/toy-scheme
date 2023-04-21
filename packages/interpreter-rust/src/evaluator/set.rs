use crate::evaluator::IEvaluator;
use crate::parser::SchemeData;

pub struct SetEvaluator;

impl IEvaluator for SetEvaluator {
  fn can_match(&self) -> bool {
    true
  }
  fn evaluate(&self) -> SchemeData {
    SchemeData::Nil
  }
}