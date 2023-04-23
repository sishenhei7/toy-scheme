use std::cell::RefCell;
use std::rc::Rc;

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

struct EvaluateError;

pub trait IEvaluator {
  fn can_match(&self) -> bool;
  fn evaluate(
    &self,
    data: &SchemeData,
    env: Rc<RefCell<Env>>,
    cont: &SchemeCont,
    base_evaluator: Evaluator,
  ) -> SchemeCont;
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
  pub fn evaluate(
    &self,
    data: &SchemeData,
    env: &Rc<RefCell<Env>>,
    cont: &SchemeCont,
  ) -> Result<SchemeCont, EvaluateError> {
    match data {
      SchemeData::List(value) => Ok(SchemeData::Nil),
      SchemeData::Identifier(identifier) => match env.borrow_mut().get(&identifier.value) {
        Some(x) => Ok(SchemeCont {
          func: cont.func.clone(),
          loc: data.get_loc(),
          data: Some(Box::new(x.clone())),
          env: Some(env.clone()),
        }),
        None => Err(EvaluateError),
      },
      _ => Ok(SchemeCont {
        func: cont.func.clone(),
        loc: data.get_loc(),
        data: Some(Box::new(data.clone())),
        env: Some(env.clone()),
      }),
    }
  }
}
