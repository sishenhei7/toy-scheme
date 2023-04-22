use std::cell::RefCell;
use std::rc::Rc;
use std::vec;

use crate::env::Env;
use crate::lexer::*;

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

#[derive(Debug, PartialEq, Clone)]
pub struct SchemeCont {
  pub func: fn(SchemeData) -> SchemeData,
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

// TODO: scheme 里面的 data 都是保存在 heap 里面的，我们这里需要用 Rc 和 RefCell 包裹吗？
#[derive(Debug, PartialEq, Clone)]
pub enum SchemeData {
  Nil, // only for SchemeData::List
  Identifier(SchemeIdentifier),
  Number(SchemeNumber),
  String(SchemeString),
  Boolean(SchemeBoolean),
  List(SchemeList),
  Continuation(SchemeCont),
  Procedure(SchemeProc),
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

impl SchemeData {
  pub fn get_loc(&self) -> Option<Location> {
    match self {
      SchemeData::Identifier(x) => x.loc.clone(),
      SchemeData::Number(x) => x.loc.clone(),
      SchemeData::String(x) => x.loc.clone(),
      SchemeData::Boolean(x) => x.loc.clone(),
      SchemeData::List(x) => x.loc.clone(),
      SchemeData::Continuation(x) => x.loc.clone(),
      SchemeData::Procedure(x) => x.loc.clone(),
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
      SchemeData::Continuation(x) => x.loc = Some(new_loc),
      SchemeData::Procedure(x) => x.loc = Some(new_loc),
      _ => panic!(),
    };
    self
  }

  pub fn build_default_loc() -> Location {
    Location {
      ..Default::default()
    }
  }

  pub fn build_list_from_vec(list: &mut Vec<SchemeData>) -> SchemeData {
    let mut data = SchemeData::Nil;
    let last_loc = if let Some(last) = list.last() {
      last.get_loc().unwrap_or(Default::default())
    } else {
      Default::default()
    };

    while let Some(item) = list.pop() {
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
    left_paren_loc: Option<Location>,
  ) -> Result<SchemeData, ParseError> {
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
            match peek.token {
              TokenType::LParen => SchemeData::parse_list_from_end(list, None)?,
              _ => SchemeData::parse_list_from_end(&mut vec![list.pop().unwrap()], None)?,
            }
          } else {
            SchemeData::Nil
          }
        }
        TokenType::LParen => SchemeData::parse_list_from_end(list, Some(loc))?,
        TokenType::RParen => {
          return if let Some(left_loc) = left_paren_loc {
            let mut data = SchemeData::build_list_from_vec(&mut scheme_data_list);
            data.set_loc(Location {
              line_start: left_loc.line_start,
              column_start: left_loc.column_start,
              line_end: loc.line_end,
              column_end: loc.column_end,
            });
            Ok(data)
          } else {
            Err(ParseError {
              msg: "Right parens are more than left parens!".to_string(),
            })
          }
        }
        _ => {
          return Err(ParseError {
            msg: "Unexpected token!".to_string(),
          })
        }
      })
    }

    if left_paren_loc.is_some() {
      return Err(ParseError {
        msg: "Left parens are more than left parens!".to_string(),
      });
    }

    Ok(SchemeData::build_list_from_vec(&mut scheme_data_list))
  }
}

pub fn parse(mut list: Vec<TokenItem>) -> Result<SchemeData, ParseError> {
  list.reverse();
  SchemeData::parse_list_from_end(&mut list, None)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_add() {
    let tokens = tokenize("(+ 1 2)").unwrap_or(vec![]);
    let data = parse(tokens).unwrap_or(SchemeData::Nil);
    assert_eq!(
      data,
      build_list!(
        build_list!(
          build_identifier!(
            "+".to_string(),
            Some(Location {
              line_start: 1,
              column_start: 2,
              line_end: 1,
              column_end: 3
            })
          ),
          build_list!(
            build_number!(
              1 as f64,
              Some(Location {
                line_start: 1,
                column_start: 4,
                line_end: 1,
                column_end: 5
              })
            ),
            build_list!(
              build_number!(
                2 as f64,
                Some(Location {
                  line_start: 1,
                  column_start: 6,
                  line_end: 1,
                  column_end: 7
                })
              ),
              SchemeData::Nil,
              Some(Location {
                line_start: 1,
                column_start: 6,
                line_end: 1,
                column_end: 7
              })
            ),
            Some(Location {
              line_start: 1,
              column_start: 4,
              line_end: 1,
              column_end: 7
            })
          ),
          Some(Location {
            line_start: 1,
            column_start: 1,
            line_end: 1,
            column_end: 8
          })
        ),
        SchemeData::Nil,
        Some(Location {
          line_start: 1,
          column_start: 1,
          line_end: 1,
          column_end: 8
        })
      )
    );
  }
}
