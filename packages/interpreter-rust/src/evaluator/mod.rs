mod begin;
mod buildin;
mod call_cc;
mod cond;
mod define;
mod if_eval;
mod lambda;
mod let_eval;
mod proc;
mod set;

use crate::parser::SchemeData;

struct Evaluator {
  // why dyn ?
  evaluators: Vec<Box<dyn IEvaluator>>,
}

pub trait IEvaluator {
  fn evaluate(&self) -> SchemeData {
    SchemeData::Nil
  }
}

impl Evaluator {
  pub fn new(&self) -> Self {
    Evaluator {
      evaluators: vec![Box::new(begin::BeginEvaluator::new(&self))],
    }
  }

  pub fn evaluate() -> SchemeData {
    SchemeData::Nil
  }
}
