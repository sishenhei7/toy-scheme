use crate::evaluator::IEvaluator;
use crate::parser::SchemeData;

pub struct IfEvalEvaluator;

impl IEvaluator for IfEvalEvaluator {
  fn can_match(&self) -> bool {
    true
  }
  fn evaluate(&self) -> SchemeData {
    SchemeData::Nil
  }
}
