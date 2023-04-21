use crate::evaluator::IEvaluator;
use crate::parser::SchemeData;

pub struct CondEvaluator;

impl IEvaluator for CondEvaluator {
  fn can_match(&self) -> bool {
    true
  }
  fn evaluate(&self) -> SchemeData {
    SchemeData::Nil
  }
}