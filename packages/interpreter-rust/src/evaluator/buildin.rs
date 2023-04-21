use crate::evaluator::IEvaluator;
use crate::parser::SchemeData;

pub struct BuildinEvaluator;

impl IEvaluator for BuildinEvaluator {
  fn can_match(&self) -> bool {
    true
  }
  fn evaluate(&self) -> SchemeData {
    SchemeData::Nil
  }
}