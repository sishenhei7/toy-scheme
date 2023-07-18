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

pub fn evaluate(data: &SchemeExp, env: &Env, cont: &SchemeCont) -> Result<EvaluateResponse, Error> {
  Ok(EvaluateResponse::Cont(SchemeCont {
    func: Closure::new(|x| Ok(EvaluateResponse::Data(x))),
    data: None,
    env: env.clone(),
    loc: data.loc.clone(),
  }))
}