use anyhow::Error;

use crate::{
  env::Env,
  closure::Closure,
  parser::{BaseSchemeData, SchemeCont, SchemeData, SchemeExp},
};

use super::evaluate_exp;

// TODO: 想好 evaluate 返回什么，closure 返回什么，什么时候返回 option，什么时候返回 result
pub fn evaluate(data: &SchemeExp, env: &Env, cont: &SchemeCont) -> Result<SchemeCont, Error> {
  let new_data = SchemeData::build_exp_from_vec(data.value[1..].to_vec()).ok_or(Error::msg("Evaluate Error!"))?;
  let data_copy = new_data.clone();
  let env_copy = env.clone();
  let cont_copy = cont.clone();
  Ok(SchemeCont {
    func: Closure::new(move |_| evaluate_exp(&data_copy, &env_copy, &cont_copy)),
    loc: new_data.loc.clone(),
    data: None,
    env: env.clone(),
  })
}
