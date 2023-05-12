use std::cell::RefCell;
use std::rc::Rc;

mod begin;
mod buildin;
mod call_cc;
mod cond;
mod cont;
mod define;
mod if_clause;
mod lambda;
mod let_clause;
mod proc;
mod set;

use crate::env::Env;
use crate::parser::{SchemeCont, SchemeData, SchemeExp};

struct Evaluator<'a> {
  i_evaluators: Vec<Box<dyn IEvaluator<'a>>>,
}

struct EvaluateError;

pub trait IEvaluator<'a> {
  fn can_match(&self, data: &SchemeExp) -> bool;
  fn evaluate(
    &self,
    data: &SchemeExp,
    env: &Rc<RefCell<Env>>,
    cont: &SchemeCont,
    base_evaluator: &'a Evaluator,
  ) -> SchemeData;
}

impl<'a> Evaluator<'a> {
  pub fn new() -> Self {
    Evaluator {
      i_evaluators: vec![
        Box::new(begin::BeginEvaluator),
        Box::new(buildin::BuildinEvaluator),
        Box::new(call_cc::CallCcEvaluator),
        Box::new(cond::CondEvaluator),
        Box::new(define::DefineEvaluator),
        Box::new(if_clause::IfEvalEvaluator),
        Box::new(lambda::LambdaEvaluator),
        Box::new(let_clause::LetEvalEvaluator),
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
      SchemeData::Exp(x) => self.evaluate_exp(x, env, cont),
      SchemeData::Identifier(identifier) => match env.borrow_mut().get(&identifier.value) {
        Some(x) => Ok(SchemeCont {
          func: cont.func,
          loc: data.get_loc(),
          data: Some(Box::new(x)),
          env: Some(env.clone()),
        }),
        None => Err(EvaluateError),
      },
      _ => Ok(SchemeCont {
        func: cont.func,
        loc: data.get_loc(),
        data: Some(Box::new(data.clone())),
        env: Some(env.clone()),
      }),
    }
  }
  pub fn evaluate_exp(
    &self,
    data: &'a SchemeExp,
    env: &'a Rc<RefCell<Env>>,
    cont: &'a SchemeCont,
  ) -> Result<SchemeCont, EvaluateError> {
    for i_evaluator in self.i_evaluators.iter() {
      if i_evaluator.can_match(data) {
        return Ok(SchemeCont {
          func: Box::new(|_| i_evaluator.evaluate(data, env, cont, self)),
          loc: data.loc.clone(),
          data: None,
          env: Some(env.clone()),
        });
      }
    }
    Err(EvaluateError)
  }
}
