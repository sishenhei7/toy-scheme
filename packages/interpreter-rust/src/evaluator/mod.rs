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
  match data.value.get(0) {
    Some(x) => {
      match *x.0.borrow() {
        BaseSchemeData::Identifier(ref sym) => {
          // evaluate(data: &SchemeExp, env: &Env, cont: &SchemeCont) -> SchemeData
          let new_cont = match sym.value.as_str() {
            "begin" => begin::evaluate(&data, &env, &cont),
            "call-with-current-continuation" => call_cc::evaluate(&data, &env, &cont),
            "cond" => cond::evaluate(&data, &env, &cont),
            "cont" => cont::evaluate(&data, &env, &cont),
            "define" => define::evaluate(&data, &env, &cont),
            "if" => if_clause::evaluate(&data, &env, &cont),
            "lambda" => lambda::evaluate(&data, &env, &cont),
            "let" => let_clause::evaluate_let(&data, &env, &cont),
            "let*" => let_clause::evaluate_letstar(&data, &env, &cont),
            "letrc" => let_clause::evaluate_letrc(&data, &env, &cont),
            "proc" => proc::evaluate(&data, &env, &cont),
            "set!" => set::evaluate(&data, &env, &cont),
            _ => buildin::evaluate(&data, &env, &cont)
          };

          if let Some(cont) = new_cont {
            return Ok(cont)
          };

          Err(Error::msg("Evaluate expression error!"))
        },
        BaseSchemeData::Continuation(ref _cont) => {
          // cont
          let new_cont = cont::evaluate(&data, &env, &cont);

          if let Some(cont) = new_cont {
            return Ok(cont)
          };

          Err(Error::msg("Evaluate expression error!"))
        }
        BaseSchemeData::Procedure(ref _proc) => {
          // proc
          let new_cont = proc::evaluate(&data, &env, &cont);

          if let Some(cont) = new_cont {
            return Ok(cont)
          };

          Err(Error::msg("Evaluate expression error!"))
        },
        // TODO: 这里有点问题，后续看怎么做？？？？
        BaseSchemeData::Exp(ref x) => evaluate_exp(x, env, cont),
        _ => Err(Error::msg("Evaluate expression error!"))
      }
    }
    None => Err(Error::msg("Evaluate expression error!"))
  }
}
