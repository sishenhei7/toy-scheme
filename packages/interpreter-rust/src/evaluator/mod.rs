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

use crate::env::Env;
use crate::parser::{SchemeCont, SchemeData};

struct Evaluator {
  i_evaluators: Vec<Box<dyn IEvaluator>>,
}

pub trait IEvaluator {
  fn can_match(&self) -> bool;
  fn evaluate(&self) -> SchemeData;
}

impl Evaluator {
  pub fn new() -> Self {
    Evaluator {
      i_evaluators: vec![
        Box::new(begin::BeginEvaluator),
        Box::new(buildin::BuildinEvaluator),
        Box::new(call_cc::CallCcEvaluator),
        Box::new(cond::CondEvaluator),
        Box::new(define::DefineEvaluator),
        Box::new(if_eval::IfEvalEvaluator),
        Box::new(lambda::LambdaEvaluator),
        Box::new(let_eval::LetEvalEvaluator),
        Box::new(proc::ProcEvaluator),
        Box::new(set::SetEvaluator),
      ],
    }
  }
  pub fn evaluate(&self, data: &SchemeData, env: &Env, cont: SchemeCont) -> SchemeData {
    SchemeData::Nil
  }
}
