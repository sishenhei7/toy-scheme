use crate::{
  env::Env,
  closure::Closure,
  parser::{BaseSchemeData, SchemeCont, SchemeData, SchemeExp},
};

pub fn evaluate_let(data: &SchemeExp, env: &Env, cont: &SchemeCont) -> Option<SchemeCont> {
  Some(SchemeCont {
    func: Closure::new(|x| x),
    loc: data.loc.clone(),
    data: None,
    env: env.clone(),
  })
}

pub fn evaluate_letstar(data: &SchemeExp, env: &Env, cont: &SchemeCont) -> Option<SchemeCont> {
  Some(SchemeCont {
    func: Closure::new(|x| x),
    loc: data.loc.clone(),
    data: None,
    env: env.clone(),
  })
}

pub fn evaluate_letrc(data: &SchemeExp, env: &Env, cont: &SchemeCont) -> Option<SchemeCont> {
  Some(SchemeCont {
    func: Closure::new(|x| x),
    loc: data.loc.clone(),
    data: None,
    env: env.clone(),
  })
}
