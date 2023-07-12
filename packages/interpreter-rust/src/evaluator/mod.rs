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

use anyhow::Error;
use once_cell::sync::Lazy;

use crate::{
  closure::Closure,
  env::Env,
  parser::{BaseSchemeData, SchemeCont, SchemeData, SchemeExp},
};

pub trait IEvaluator {
  fn can_match(&self, data: &SchemeExp) -> bool;
  fn evaluate(
    &self,
    data: &SchemeExp,
    env: &Env,
    cont: &SchemeCont,
  ) -> SchemeData;
}

static SUB_EVALUATORS: Lazy<Vec<Box<dyn IEvaluator + Send + Sync>>> = Lazy::new(|| vec![
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
]);

pub fn evaluate(
  data: &SchemeData,
  env: &Env,
  cont: &SchemeCont,
) -> Result<SchemeCont, Error> {
  match *data.0.borrow() {
    BaseSchemeData::Exp(ref x) => evaluate_exp(x, env, cont),
    BaseSchemeData::Identifier(ref identifier) => match env.get(&identifier.value) {
      Some(ref x) => Ok(SchemeCont {
        func: cont.func.clone(),
        loc: data.get_loc(),
        data: Some(x.clone()),
        env: env.clone(),
      }),
      None => Err(Error::msg("Evaluate Error!")),
    },
    _ => Ok(SchemeCont {
      func: cont.func.clone(),
      loc: data.get_loc(),
      data: Some(data.clone()),
      env: env.clone(),
    }),
  }
}

pub fn evaluate_exp(
  data: &SchemeExp,
  env: &Env,
  cont: &SchemeCont,
) -> Result<SchemeCont, Error> {
  for i_evaluator in SUB_EVALUATORS.iter() {
    if i_evaluator.can_match(data) {
      let data_copy = data.clone();
      let env_copy = env.clone();
      let cont_copy = cont.clone();
      let evaluator = i_evaluator.clone();
      return Ok(SchemeCont {
        func: Closure::new(move |_| evaluator.evaluate(&data_copy, &env_copy, &cont_copy)),
        loc: data.loc.clone(),
        data: None,
        env: env.clone(),
      });
    }
  }
  Err(Error::msg("Evaluate expression error!"))
}
