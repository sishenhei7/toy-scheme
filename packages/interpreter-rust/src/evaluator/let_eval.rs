use crate::evaluator::IEvaluator;
use crate::parser::SchemeData;

pub struct LetEvalEvaluator;

impl IEvaluator for LetEvalEvaluator {
  fn can_match(&self) -> bool {
    true
  }
  fn evaluate(&self) -> SchemeData {
    SchemeData::Nil
  }
}