use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::{SchemeBoolean, SchemeData, SchemeNumber, SchemeString};

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
