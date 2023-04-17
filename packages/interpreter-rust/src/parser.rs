use crate::env::Env;
use crate::lexer::Location;

pub struct SchemeIdentifier {
  value: String,
  loc: Location,
}

pub struct SchemeNumber {
  value: f64,
  loc: Location,
}

pub struct SchemeString {
  value: String,
  loc: Location,
}

pub struct SchemeBoolean {
  value: bool,
  loc: Location,
}

pub struct SchemeList {
  car: Box<SchemeData>,
  cdr: Box<SchemeData>,
  loc: Location,
}

pub struct SchemeCont {
  value: fn(SchemeData) -> SchemeData,
  loc: Location,
}

pub struct SchemeProc {
  name: String,
  params: Box<SchemeList>,
  body: Box<SchemeList>,
  closure: Env,
  loc: Location,
}
