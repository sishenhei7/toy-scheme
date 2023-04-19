use core::borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::SchemeData;

#[derive(Debug, PartialEq)]
pub struct Stackframe {
  data: SchemeData,
  parent: Option<Rc<Stackframe>>,
}

#[derive(Debug, PartialEq)]
pub struct Env {
  scope: HashMap<String, SchemeData>,
  parent: Option<Rc<RefCell<Env>>>,
  stackframe: Option<Rc<Stackframe>>,
}

impl Env {
  pub fn new() -> Self {
    Env {
      scope: HashMap::new(),
      parent: None,
      stackframe: None,
    }
  }

  pub fn extend(parent_env: Env, stackframe: Option<Rc<Stackframe>>) -> Env {
    Env {
      scope: HashMap::new(),
      parent: Some(Rc::new(RefCell::new(parent_env))),
      stackframe,
    }
  }

  pub fn get(&self, key: &str) -> Option<SchemeData> {
    match self.scope.get(key) {
      Some(x) => Some(x.clone()),
      None => self.parent.as_ref().and_then(|x| x.borrow().get(key)),
    }
  }

  pub fn set(&mut self, key: &str, val: SchemeData) -> Option<SchemeData> {
    match self.scope.get(key) {
      Some(_) => self.scope.insert(key.to_string(), val),
      None => self
        .parent
        .as_ref()
        .and_then(|x| x.borrow_mut().set(key, val)),
    }
  }

  pub fn define(&mut self, key: &str, val: SchemeData) -> Option<SchemeData> {
    match self.scope.get(key) {
      Some(_) => None,
      None => self.scope.insert(key.to_string(), val),
    }
  }

  pub fn modify(&mut self, key: &str, val: SchemeData) -> Option<SchemeData> {
    self.scope.insert(key.to_string(), val)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test() -> () {
    let mut env = Env::new();
    env.define(
      "test",
      SchemeData::Number(1 as f64, SchemeData::build_default_loc()),
    );
    assert_eq!(
      env.get("test").unwrap(),
      SchemeData::Number(1 as f64, SchemeData::build_default_loc())
    );
    env.set(
      "test",
      SchemeData::String("test".to_string(), SchemeData::build_default_loc()),
    );
    assert_eq!(
      env.get("test").unwrap(),
      SchemeData::String("test".to_string(), SchemeData::build_default_loc())
    );
    env.modify(
      "test",
      SchemeData::Boolean(false, SchemeData::build_default_loc()),
    );
    assert_eq!(
      env.get("test").unwrap(),
      SchemeData::Boolean(false, SchemeData::build_default_loc())
    );
  }
}
