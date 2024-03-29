use std::cell::{ RefCell, RefMut };
use std::collections::HashMap;
use std::rc::Rc;

use crate::{
  boxing::Boxing,
  build_boxing,
  parser::{SchemeBoolean, SchemeData, SchemeNumber, SchemeString},
};

#[derive(Debug, PartialEq)]
pub struct BaseStackframe {
  data: SchemeData,
  parent: Option<Stackframe>,
}

#[derive(Debug, PartialEq)]
pub struct BaseEnv {
  scope: HashMap<String, SchemeData>,
  parent: Option<Env>,
  stackframe: Option<Stackframe>,
}

#[derive(Debug, PartialEq)]
pub struct Stackframe(Boxing<BaseStackframe>);

#[derive(Debug, PartialEq, Clone)]
pub struct Env(Boxing<BaseEnv>);

impl Env {
  pub fn new() -> Self {
    Self(build_boxing!(BaseEnv {
      scope: HashMap::new(),
      parent: None,
      stackframe: None,
    }))
  }

  pub fn extend(&self, stackframe: Option<Stackframe>) -> Env {
    Env(build_boxing!(BaseEnv {
      scope: HashMap::new(),
      parent: Some(self.clone()),
      stackframe,
    }))
  }

  pub fn copy(&self) -> Env {
    Env(self.0.clone())
  }

  pub fn borrow_env_mut(&self) -> RefMut<BaseEnv> {
    self.0.borrow_mut()
  }

  pub fn get(&self, key: &str) -> Option<SchemeData> {
    let env = self.borrow_env_mut();
    let scope = &env.scope;
    match scope.get(key) {
      Some(x) => Some(x.clone()),
      None => env.parent.clone()?.get(key),
    }
  }

  pub fn set(&mut self, key: &str, val: SchemeData) -> Option<SchemeData> {
    let mut env = self.borrow_env_mut();
    let scope = &mut env.scope;
    match scope.get(key) {
      Some(_) => scope.insert(key.to_string(), val),
      None => env.parent.clone()?.set(key, val),
    }
  }

  pub fn define(&mut self, key: &str, val: SchemeData) -> Option<SchemeData> {
    let scope = &mut self.borrow_env_mut().scope;
    match scope.get(key) {
      Some(_) => None,
      None => scope.insert(key.to_string(), val),
    }
  }

  pub fn modify(&mut self, key: &str, val: SchemeData) -> Option<SchemeData> {
    let scope = &mut self.borrow_env_mut().scope;
    scope.insert(key.to_string(), val)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::{build_boolean, build_number, build_string};

  #[test]
  fn test() -> () {
    let mut env = Env::new();
    env.define("test", build_number!(1 as f64, None));
    assert_eq!(env.get("test").unwrap(), build_number!(1 as f64, None));
    env.set("test", build_string!("test".to_string(), None));
    assert_eq!(
      env.get("test").unwrap(),
      build_string!("test".to_string(), None)
    );
    env.modify("test", build_boolean!(false, None));
    assert_eq!(env.get("test").unwrap(), build_boolean!(false, None));
  }
}
