use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{
  boxing::Boxing,
  build_boxing,
  parser::{BaseSchemeData, SchemeBoolean, SchemeData, SchemeNumber, SchemeString},
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

  pub fn extend(parent_env: Env, stackframe: Option<Stackframe>) -> Env {
    Env(build_boxing!(BaseEnv {
      scope: HashMap::new(),
      parent: Some(parent_env),
      stackframe,
    }))
  }

  pub fn copy(&self) -> Env {
    Env(self.0.clone())
  }

  pub fn get_parent(&self) -> Option<Env> {
    self.0.borrow_mut().parent.clone()
  }

  pub fn get(&self, key: &str) -> Option<SchemeData> {
    match self.0.borrow().scope.get(key) {
      Some(x) => Some(x.clone()),
      None => self.get_parent()?.get(key),
    }
  }

  pub fn set(&mut self, key: &str, val: SchemeData) -> Option<SchemeData> {
    match self.0.borrow().scope.get(key) {
      Some(_) => self.0.borrow_mut().scope.insert(key.to_string(), val),
      None => self.get_parent()?.set(key, val),
    }
  }

  pub fn define(&mut self, key: &str, val: SchemeData) -> Option<SchemeData> {
    match self.0.borrow().scope.get(key) {
      Some(_) => None,
      None => self.0.borrow_mut().scope.insert(key.to_string(), val),
    }
  }

  pub fn modify(&mut self, key: &str, val: SchemeData) -> Option<SchemeData> {
    self.0.borrow_mut().scope.insert(key.to_string(), val)
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
