use crate::{
  env::Env,
  closure::Closure,
  parser::{BaseSchemeData, SchemeCont, SchemeData, SchemeExp},
};

use super::evaluate_exp;

pub fn evaluate(data: &SchemeExp, env: &Env, cont: &SchemeCont) -> Option<SchemeCont> {
  // let data_copy = data.clone();
  // let env_copy = env.clone();
  // let cont_copy = env.clone();
  Some(SchemeCont {
    // func: Closure::new(move |_| evaluate_exp(&data_copy, &env_copy, &cont_copy)),
    func: Closure::new(|x| x),
    loc: data.loc.clone(),
    data: None,
    env: env.clone(),
  })
}
