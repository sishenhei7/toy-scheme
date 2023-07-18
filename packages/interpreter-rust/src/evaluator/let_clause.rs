use anyhow::Error;

use crate::{
  env::Env,
  closure::Closure,
  parser::{BaseSchemeData, SchemeCont, SchemeData, SchemeExp},
};

use super::{
  evaluate_exp,
  EvaluateResponse
};

pub fn evaluate_let(data: &SchemeExp, env: &Env, cont: &SchemeCont) -> Result<EvaluateResponse, Error> {
  Ok(EvaluateResponse::Cont(SchemeCont {
    func: Closure::new(|x| Ok(EvaluateResponse::Data(x))),
    data: None,
    env: env.clone(),
    loc: data.loc.clone(),
  }))
}

pub fn evaluate_letstar(data: &SchemeExp, env: &Env, cont: &SchemeCont) -> Result<EvaluateResponse, Error> {
  Ok(EvaluateResponse::Cont(SchemeCont {
    func: Closure::new(|x| Ok(EvaluateResponse::Data(x))),
    data: None,
    env: env.clone(),
    loc: data.loc.clone(),
  }))
}

pub fn evaluate_letrc(data: &SchemeExp, env: &Env, cont: &SchemeCont) -> Result<EvaluateResponse, Error> {
  Ok(EvaluateResponse::Cont(SchemeCont {
    func: Closure::new(|x| Ok(EvaluateResponse::Data(x))),
    data: None,
    env: env.clone(),
    loc: data.loc.clone(),
  }))
}
