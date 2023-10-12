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
  let new_data = SchemeData::build_exp_from_vec(data.value[1..].to_vec()).ok_or(Error::msg("Evaluate Begin Error!"))?;
  let data_copy = new_data.clone();
  let env_copy = env.clone();
  let cont_copy = cont.clone();
  Ok(EvaluateResponse::Cont(SchemeCont {
    func: Closure::new(move |_| evaluate_exp(&data_copy, &env_copy, &cont_copy)),
    data: None,
    env: env.clone(),
    loc: new_data.loc.clone(),
  }))
}
