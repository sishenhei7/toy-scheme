use std::fmt;

use crate::parser::{SchemeData};

pub struct Closure(Box<dyn FnMut(SchemeData) -> SchemeData>);

impl Closure {
  pub fn new(func: impl FnMut(SchemeData) -> SchemeData) -> Closure {
    Closure(Box::new(func))
  }
  pub fn call(&self, val: SchemeData) -> SchemeData {
    self.0(val)
  }
}

impl fmt::Debug for Closure {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "Closure")
  }
}

// impl PartialEq<Schema> for Closure {
//   fn eq(&self, other: &&Schema) -> bool {
//     Schema::eq(*self, *other)
//   }
// }