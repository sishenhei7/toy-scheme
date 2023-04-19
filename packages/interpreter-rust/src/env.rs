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
  hashmap: HashMap<String, SchemeData>,
  parent: Option<Rc<RefCell<Env>>>,
  stackframe: Option<Stackframe>,
}

impl Env {
  pub fn new() -> Self {
    Env {
      hashmap: HashMap::new(),
      parent: None,
      stackframe: None,
    }
  }

  pub fn extend(parent_env: Env, stackframe: Option<Stackframe>) -> Env {
    Env {
      hashmap: HashMap::new(),
      parent: Some(Rc::new(RefCell::new(parent_env))),
      stackframe,
    }
  }

  pub fn get_parent(&self) -> Option<Rc<RefCell<Env>>> {
    self.parent
  }

  pub fn get(&self, key: String) -> Option<&SchemeData> {
    // if self.hashmap.contains_key(&key) {
    //   self.hashmap.get(&key)
    // } else {
    //   self.parent
    // }
  }

  pub fn set(key: String, val: SchemeData) -> SchemeData {}

  pub fn define(key: String, val: SchemeData) -> SchemeData {}

  pub fn modify(key: String, val: SchemeData) -> SchemeData {}
}
