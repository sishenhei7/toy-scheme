use crate::evaluator::IEvaluator;
use crate::parser::SchemeData;

pub struct DefineEvaluator;

impl IEvaluator for DefineEvaluator {
  fn can_match(&self) -> bool {
    true
  }
  fn evaluate(&self) -> SchemeData {
    SchemeData::Nil
  }
}