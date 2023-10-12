use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use anyhow::Error;

use crate::{
  boxing::Boxing,
  build_boxing,
  parser::{
    SchemeData,
    SchemeCont
  },
  evaluator::EvaluateResponse
};

pub struct Closure(Boxing<dyn FnMut(SchemeData) -> Result<EvaluateResponse, Error>>);

impl Closure {
  pub fn new(func: impl FnMut(SchemeData) -> Result<EvaluateResponse, Error> + 'static) -> Closure {
    Closure(build_boxing!(func))
  }
  pub fn copy(&self) -> Closure {
    Closure(self.0.clone())
  }
  pub fn call(&self, val: SchemeData) -> Result<EvaluateResponse, Error> {
    self.0.borrow_mut()(val)
  }
}

impl fmt::Debug for Closure {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "Closure")
  }
}

impl PartialEq for Closure {
  fn eq(&self, other: &Closure) -> bool {
    // Reference Equality
    Rc::ptr_eq(&self.0, &other.0)
  }
}

impl Clone for Closure {
  fn clone(&self) -> Self {
    self.copy()
  }
}
