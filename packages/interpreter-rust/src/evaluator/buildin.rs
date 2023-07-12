use crate::{
  env::Env,
  closure::Closure,
  parser::{BaseSchemeData, SchemeCont, SchemeData, SchemeExp},
};

pub fn evaluate(data: &SchemeExp, env: &Env, cont: &SchemeCont) -> Option<SchemeCont> {
  Some(SchemeCont {
    func: Closure::new(|x| x),
    loc: data.loc.clone(),
    data: None,
    env: env.clone(),
  })
}