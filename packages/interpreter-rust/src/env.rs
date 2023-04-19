use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::SchemeData;

#[derive(Debug, PartialEq)]
pub struct Stackframe {
  data: Rc<SchemeData>,
  parent: Option<Rc<Stackframe>>,
}

#[derive(Debug, PartialEq)]
pub struct Env {
  scope: HashMap<String, SchemeData>,
  parent: Option<Rc<RefCell<Env>>>,
  stackframe: Option<Stackframe>,
}

impl Env {
  pub fn new() -> Self {
    Env {
      scope: HashMap::new(),
      parent: None,
      stackframe: None,
    }
  }

  pub fn extend(parent_env: Env, stackframe: Option<Stackframe>) -> Env {
    Env {
      scope: HashMap::new(),
      parent: Some(Rc::new(RefCell::new(parent_env))),
      stackframe,
    }
  }

  pub fn get(&self, key: &str) -> Option<&SchemeData> {
    match self.scope.get(key) {
      Some(x) => Some(x),
      None => self.parent.unwrap().borrow().get(key)
    }
  }

  pub fn set(&mut self, key: String, val: SchemeData) -> Option<SchemeData> {
    match self.scope.get(&key) {
      Some(_) => self.scope.insert(key, val),
      None => self.parent?.borrow().set(key, val)
    }
  }

  pub fn define(&mut self, key: String, val: SchemeData) -> Option<SchemeData> {
    match self.scope.get(&key) {
      Some(_) => None,
      None => self.scope.insert(key, val)
    }
  }

  pub fn modify(&mut self, key: String, val: SchemeData) -> Option<SchemeData> {
    self.scope.insert(key, val)
  }
}
