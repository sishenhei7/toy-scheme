use std::cell::RefCell;
use std::rc::Rc;

use anyhow::Error;

use crate::{
  boxing::Boxing,
  build_boxing,
  closure::Closure,
  env::Env,
  lexer::{Location, TokenItem, TokenType},
};

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
  pub value: (SchemeData, SchemeData),
  pub loc: Option<Location>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SchemeCont {
  pub func: Closure,
  pub env: Env,
  pub data: Option<SchemeData>,
  pub loc: Option<Location>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SchemeProc {
  pub name: String,
  pub params: SchemeData,
  pub body: SchemeData,
  pub env: Env,
  pub loc: Option<Location>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SchemeExp {
  pub value: Vec<SchemeData>,
  pub loc: Option<Location>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BaseSchemeData {
  Nil,
  Identifier(SchemeIdentifier),
  Number(SchemeNumber),
  String(SchemeString),
  Boolean(SchemeBoolean),
  List(SchemeList),
  Continuation(SchemeCont),
  Procedure(SchemeProc),
  Exp(SchemeExp),
}

#[derive(Debug, PartialEq, Clone)]
pub struct SchemeData(pub Boxing<BaseSchemeData>);

#[macro_export]
macro_rules! build_identifier {
  ($value:expr, $loc:expr) => {
    SchemeData::new(BaseSchemeData::Identifier(SchemeIdentifier {
      value: $value,
      loc: $loc,
    }))
  };
}

#[macro_export]
macro_rules! build_number {
  ($value:expr, $loc:expr) => {
    SchemeData::new(BaseSchemeData::Number(SchemeNumber {
      value: $value,
      loc: $loc,
    }))
  };
}

#[macro_export]
macro_rules! build_string {
  ($value:expr, $loc:expr) => {
    SchemeData::new(BaseSchemeData::String(SchemeString {
      value: $value,
      loc: $loc,
    }))
  };
}

#[macro_export]
macro_rules! build_boolean {
  ($value:expr, $loc:expr) => {
    SchemeData::new(BaseSchemeData::Boolean(SchemeBoolean {
      value: $value,
      loc: $loc,
    }))
  };
}

#[macro_export]
macro_rules! build_list {
  ($car:expr, $cdr:expr, $loc:expr) => {
    SchemeData::new(BaseSchemeData::List(SchemeList {
      value: ($car, $cdr),
      loc: $loc,
    }))
  };
}

#[macro_export]
macro_rules! build_exp {
  ($vec:expr, $loc:expr) => {
    SchemeData::new(BaseSchemeData::Exp(SchemeExp {
      value: $vec,
      loc: $loc,
    }))
  };
}

impl SchemeData {
  pub fn new(data: BaseSchemeData) -> Self {
    Self(build_boxing!(data))
  }

  pub fn get_loc(&self) -> Option<Location> {
    match *self.0.borrow() {
      BaseSchemeData::Identifier(ref x) => x.loc.clone(),
      BaseSchemeData::Number(ref x) => x.loc.clone(),
      BaseSchemeData::String(ref x) => x.loc.clone(),
      BaseSchemeData::Boolean(ref x) => x.loc.clone(),
      BaseSchemeData::List(ref x) => x.loc.clone(),
      BaseSchemeData::Continuation(ref x) => x.loc.clone(),
      BaseSchemeData::Procedure(ref x) => x.loc.clone(),
      BaseSchemeData::Exp(ref x) => x.loc.clone(),
      _ => None,
    }
  }

  // 为什么这里 match 里面需要加 mut ?
  pub fn set_loc(&mut self, new_loc: Location) -> &Self {
    match *self.0.borrow_mut() {
      BaseSchemeData::Identifier(ref mut x) => x.loc = Some(new_loc),
      BaseSchemeData::Number(ref mut x) => x.loc = Some(new_loc),
      BaseSchemeData::String(ref mut x) => x.loc = Some(new_loc),
      BaseSchemeData::Boolean(ref mut x) => x.loc = Some(new_loc),
      BaseSchemeData::List(ref mut x) => x.loc = Some(new_loc),
      BaseSchemeData::Continuation(ref mut x) => x.loc = Some(new_loc),
      BaseSchemeData::Procedure(ref mut x) => x.loc = Some(new_loc),
      BaseSchemeData::Exp(ref mut x) => x.loc = Some(new_loc),
      _ => panic!(),
    };
    self
  }

  pub fn build_list_from_vec(vec: &mut Vec<SchemeData>) -> SchemeData {
    let mut data = SchemeData::new(BaseSchemeData::Nil);
    let last_loc = if let Some(last) = vec.last() {
      last.get_loc().unwrap_or(Default::default())
    } else {
      Default::default()
    };

    while let Some(item) = vec.pop() {
      if let Some(loc) = item.get_loc() {
        data = build_list!(
          item,
          data,
          Some(Location {
            line_start: loc.line_start,
            column_start: loc.column_start,
            line_end: last_loc.line_end,
            column_end: last_loc.column_end,
          })
        )
      }
    }

    data
  }

  pub fn parse_list_from_end(
    list: &mut Vec<TokenItem>,
    left_paren_loc: Location,
  ) -> Result<SchemeExp, Error> {
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
            return Err(Error::msg("Quote parsing error!"));
          }
        }
        TokenType::LParen => {
          let paren_exp = SchemeData::parse_list_from_end(list, loc)?;
          SchemeData::new(BaseSchemeData::Exp(paren_exp))
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
        _ => return Err(Error::msg("Unexpected token!")),
      })
    }

    return if let Some(last_data) = scheme_data_list.last() {
      let last_loc = last_data.get_loc().unwrap_or(Default::default());
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
      Err(Error::msg("Parsing null!"))
    };
  }
}

pub fn parse(mut list: Vec<TokenItem>) -> Result<SchemeExp, Error> {
  list.reverse();
  SchemeData::parse_list_from_end(&mut list, Default::default())
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
        value: vec![build_exp!(
          vec![
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
          Some(Location {
            line_start: 1,
            column_start: 1,
            line_end: 1,
            column_end: 8
          })
        )],
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
