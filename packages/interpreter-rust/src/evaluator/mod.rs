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

use std::cell::RefCell;
use std::rc::Rc;

use anyhow::Error;

use crate::{
  closure::Closure,
  env::Env,
  parser::{SchemeCont, SchemeData, SchemeExp},
};

pub struct Evaluator {
  i_evaluators: Vec<Box<dyn IEvaluator>>,
}

pub trait IEvaluator {
  fn can_match(&self, data: &SchemeExp) -> bool;
  fn evaluate(
    &self,
    data: &SchemeExp,
    env: &Rc<RefCell<Env>>,
    cont: &SchemeCont,
    base_evaluator: &Evaluator,
  ) -> SchemeData;
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
  ) -> Result<SchemeCont, Error> {
    match data {
      SchemeData::Exp(x) => self.evaluate_exp(x, env, cont),
      SchemeData::Identifier(identifier) => match env.borrow_mut().get(&identifier.value) {
        Some(x) => Ok(SchemeCont {
          func: cont.func.clone(),
          loc: data.get_loc(),
          data: Some(Box::new(x)),
          env: Some(env.clone()),
        }),
        None => Err(Error::msg("Evaluate Error!")),
      },
      _ => Ok(SchemeCont {
        func: cont.func.clone(),
        loc: data.get_loc(),
        data: Some(Box::new(data.clone())),
        env: Some(env.clone()),
      }),
    }
  }
  pub fn evaluate_exp(
    &self,
    data: &SchemeExp,
    env: &Rc<RefCell<Env>>,
    cont: &SchemeCont,
  ) -> Result<SchemeCont, Error> {
    for i_evaluator in self.i_evaluators.iter() {
      if i_evaluator.can_match(data) {
        return Ok(SchemeCont {
          func: Closure::new(|_| i_evaluator.evaluate(data, env, cont, self)),
          loc: data.loc.clone(),
          data: None,
          env: Some(env.clone()),
        });
      }
    }
    Err(Error::msg("Evaluate expression error!"))
  }
}
