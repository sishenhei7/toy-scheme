mod begin;
mod buildin;
mod call_cc;
mod cond;
mod define;
mod if_eval;
mod let_eval;
mod lambda;
mod proc;
mod set;

use crate::parser::SchemeData;

struct Evaluator<T: IEvaluator> {
  evaluators: Vec<T>
}

pub trait IEvaluator {
  fn evaluate() -> SchemeData {
    SchemeData::Nil
  }
}

impl<T> Evaluator<T> {
  pub fn evaluate() -> SchemeData {
    SchemeData::Nil
  }
}

