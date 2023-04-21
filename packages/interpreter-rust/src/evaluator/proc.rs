use crate::evaluator::IEvaluator;
use crate::parser::SchemeData;

pub struct ProcEvaluator;

impl IEvaluator for ProcEvaluator {
  fn can_match(&self) -> bool {
    true
  }
  fn evaluate(&self) -> SchemeData {
    SchemeData::Nil
  }
}