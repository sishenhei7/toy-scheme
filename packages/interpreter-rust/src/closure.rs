use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::parser::SchemeData;

pub struct Closure(Rc<RefCell<Box<dyn FnMut(SchemeData) -> SchemeData>>>);

impl Closure {
  pub fn new(func: impl FnMut(SchemeData) -> SchemeData) -> Closure {
    Closure(Rc::new(RefCell::new(Box::new(func))))
  }
  pub fn copy(&self) -> Closure {
    Closure(self.0.clone())
  }
  pub fn call(&self, val: SchemeData) -> SchemeData {
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
