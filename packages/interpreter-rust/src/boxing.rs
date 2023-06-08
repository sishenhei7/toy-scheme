use std::cell::RefCell;
use std::rc::Rc;

pub type Boxing<T> = Box<Rc<RefCell<T>>>;

#[macro_export]
macro_rules! build_boxing {
  ($value:expr) => {
    Box::new(Rc::new(RefCell::new($value)))
  };
}
