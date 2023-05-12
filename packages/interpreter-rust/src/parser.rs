use std::cell::RefCell;
use std::rc::Rc;

use crate::env::Env;
use crate::lexer::{Location, TokenItem, TokenType};

// TODO: make these attributes private
#[derive(Debug, PartialEq, Clone)]
pub struct SchemeIdentifier {
  pub value: String,
  pub loc: Option<Location>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SchemeNumber {
  pub value: f64,
  pub loc: Option<Location>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SchemeString {
  pub value: String,
  pub loc: Option<Location>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SchemeBoolean {
  pub value: bool,
  pub loc: Option<Location>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SchemeList {
  pub value: (Box<SchemeData>, Box<SchemeData>),
  pub loc: Option<Location>,
}

// 由于 rust 暂时还不支持 type 作为 trait，
// 所以这里暂时不把 SchemeCont 放到 SchemeData 里面去
// (因为 SchemeData 需要有Debug, PartialEq, Clone特性)
pub struct SchemeCont {
  pub func: Box<dyn FnMut(SchemeData) -> SchemeData>,
  pub env: Option<Rc<RefCell<Env>>>,
  pub data: Option<Box<SchemeData>>,
  pub loc: Option<Location>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SchemeProc {
  pub name: String,
  pub params: Box<SchemeData>,
  pub body: Box<SchemeData>,
  pub env: Rc<RefCell<Env>>,
  pub loc: Option<Location>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SchemeExp {
  pub value: Vec<SchemeData>,
  pub loc: Option<Location>,
}

// TODO: scheme 里面的 data 都是保存在 heap 里面的，我们这里需要用 Rc 和 RefCell 包裹吗？
#[derive(Debug, PartialEq, Clone)]
pub enum SchemeData {
  Nil, // only for SchemeData::List
  Identifier(SchemeIdentifier),
  Number(SchemeNumber),
  String(SchemeString),
  Boolean(SchemeBoolean),
  List(SchemeList),
  Procedure(SchemeProc),
  Exp(SchemeExp), // only for schemedata wrapper
}

#[derive(Debug)]
pub struct ParseError {
  msg: String,
}

#[macro_export]
macro_rules! build_identifier {
  ($value:expr, $loc:expr) => {
    SchemeData::Identifier(SchemeIdentifier {
      value: $value,
      loc: $loc,
    })
  };
}

#[macro_export]
macro_rules! build_number {
  ($value:expr, $loc:expr) => {
    SchemeData::Number(SchemeNumber {
      value: $value,
      loc: $loc,
    })
  };
}

#[macro_export]
macro_rules! build_string {
  ($value:expr, $loc:expr) => {
    SchemeData::String(SchemeString {
      value: $value,
      loc: $loc,
    })
  };
}

#[macro_export]
macro_rules! build_boolean {
  ($value:expr, $loc:expr) => {
    SchemeData::Boolean(SchemeBoolean {
      value: $value,
      loc: $loc,
    })
  };
}

#[macro_export]
macro_rules! build_list {
  ($car:expr, $cdr:expr, $loc:expr) => {
    SchemeData::List(SchemeList {
      value: (Box::new($car), Box::new($cdr)),
      loc: $loc,
    })
  };
}

#[macro_export]
macro_rules! build_exp {
  ($vec:expr, $loc:expr) => {
    SchemeData::Exp(SchemeExp {
      value: $vec,
      loc: $loc,
    })
  };
}

impl SchemeData {
  pub fn get_loc(&self) -> Option<Location> {
    match self {
      SchemeData::Identifier(x) => x.loc.clone(),
      SchemeData::Number(x) => x.loc.clone(),
      SchemeData::String(x) => x.loc.clone(),
      SchemeData::Boolean(x) => x.loc.clone(),
      SchemeData::List(x) => x.loc.clone(),
      SchemeData::Procedure(x) => x.loc.clone(),
      SchemeData::Exp(x) => x.loc.clone(),
      _ => None,
    }
  }

  pub fn set_loc(&mut self, new_loc: Location) -> &Self {
    match self {
      SchemeData::Identifier(x) => x.loc = Some(new_loc),
      SchemeData::Number(x) => x.loc = Some(new_loc),
      SchemeData::String(x) => x.loc = Some(new_loc),
      SchemeData::Boolean(x) => x.loc = Some(new_loc),
      SchemeData::List(x) => x.loc = Some(new_loc),
      SchemeData::Procedure(x) => x.loc = Some(new_loc),
      SchemeData::Exp(x) => x.loc = Some(new_loc),
      _ => panic!(),
    };
    self
  }

  pub fn build_default_loc() -> Location {
    Location {
      line_start: 1,
      column_start: 1,
      line_end: 1,
      column_end: 1,
    }
  }

  pub fn build_list_from_vec(vec: &mut Vec<SchemeData>) -> SchemeData {
    let mut data = SchemeData::Nil;
    let last_loc = if let Some(last) = vec.last() {
      last.get_loc().unwrap_or(Default::default())
    } else {
      Default::default()
    };

    while let Some(item) = vec.pop() {
      if let Some(loc) = item.get_loc() {
        data = SchemeData::List(SchemeList {
          value: (Box::new(item), Box::new(data)),
          loc: Some(Location {
            line_start: loc.line_start,
            column_start: loc.column_start,
            line_end: last_loc.line_end,
            column_end: last_loc.column_end,
          }),
        })
      }
    }

    data
  }

  pub fn parse_list_from_end(
    list: &mut Vec<TokenItem>,
    left_paren_loc: Location,
  ) -> Result<SchemeExp, ParseError> {
    let mut scheme_data_list: Vec<SchemeData> = vec![];

    while let Some(TokenItem { token, loc }) = list.pop() {
      scheme_data_list.push(match token {
        // ignore WhiteSpace and EOL
        TokenType::WhiteSpace => continue,
        TokenType::EOL => continue,
        TokenType::Identifier(value) => build_identifier!(value, Some(loc)),
        TokenType::Number(value) => build_number!(value, Some(loc)),
        TokenType::String(value) => build_string!(value, Some(loc)),
        TokenType::Boolean(value) => build_boolean!(value, Some(loc)),
        TokenType::Quote => {
          if let Some(peek) = list.last() {
            let mut quote_exp = match peek.token {
              TokenType::LParen => SchemeData::parse_list_from_end(list, loc)?,
              _ => SchemeData::parse_list_from_end(&mut vec![list.pop().unwrap()], loc)?,
            };
            SchemeData::build_list_from_vec(&mut quote_exp.value)
          } else {
            return Err(ParseError {
              msg: "Quote parsing error!".to_string(),
            });
          }
        }
        TokenType::LParen => {
          let paren_exp = SchemeData::parse_list_from_end(list, loc)?;
          SchemeData::Exp(paren_exp)
        }
        TokenType::RParen => {
          return Ok(SchemeExp {
            value: scheme_data_list,
            loc: Some(Location {
              line_start: left_paren_loc.line_start,
              column_start: left_paren_loc.column_start,
              line_end: loc.line_end,
              column_end: loc.column_end,
            }),
          });
        }
        _ => {
          return Err(ParseError {
            msg: "Unexpected token!".to_string(),
          })
        }
      })
    }

    return if let Some(last_data) = scheme_data_list.last() {
      let last_loc = last_data
        .get_loc()
        .unwrap_or(SchemeData::build_default_loc());
      Ok(SchemeExp {
        value: scheme_data_list,
        loc: Some(Location {
          line_start: left_paren_loc.line_start,
          column_start: left_paren_loc.column_start,
          line_end: last_loc.line_end,
          column_end: last_loc.column_end,
        }),
      })
    } else {
      Err(ParseError {
        msg: "Parsing null!".to_string(),
      })
    };
  }
}

pub fn parse(mut list: Vec<TokenItem>) -> Result<SchemeExp, ParseError> {
  list.reverse();
  SchemeData::parse_list_from_end(&mut list, SchemeData::build_default_loc())
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer::tokenize;

  #[test]
  fn test_add() {
    let tokens = tokenize("(+ 1 2)").unwrap_or(vec![]);
    let data = parse(tokens).unwrap_or(SchemeExp {
      value: vec![],
      loc: None,
    });
    assert_eq!(
      data,
      SchemeExp {
        value: vec![SchemeData::Exp(SchemeExp {
          value: vec![
            build_identifier!(
              "+".to_string(),
              Some(Location {
                line_start: 1,
                column_start: 2,
                line_end: 1,
                column_end: 3
              })
            ),
            build_number!(
              1 as f64,
              Some(Location {
                line_start: 1,
                column_start: 4,
                line_end: 1,
                column_end: 5
              })
            ),
            build_number!(
              2 as f64,
              Some(Location {
                line_start: 1,
                column_start: 6,
                line_end: 1,
                column_end: 7
              })
            )
          ],
          loc: Some(Location {
            line_start: 1,
            column_start: 1,
            line_end: 1,
            column_end: 8
          })
        })],
        loc: Some(Location {
          line_start: 1,
          column_start: 1,
          line_end: 1,
          column_end: 8
        })
      }
    );
  }
}
