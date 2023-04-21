use crate::evaluator::IEvaluator;
use crate::parser::SchemeData;

pub struct LambdaEvaluator;

impl IEvaluator for LambdaEvaluator {
  fn can_match(&self) -> bool {
    true
  }
  fn evaluate(&self) -> SchemeData {
    SchemeData::Nil
  }
}