use crate::evaluator::IEvaluator;
use crate::parser::SchemeData;

pub struct BeginEvaluator;

impl IEvaluator for BeginEvaluator {
  fn can_match(&self) -> bool {
    true
  }
  fn evaluate(&self) -> SchemeData {
    SchemeData::Nil
  }
}
