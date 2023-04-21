use std::cell::RefCell;
use std::rc::Rc;
use std::vec;

use crate::env::Env;
use crate::lexer::*;

#[derive(Debug, Default, PartialEq, Clone)]
struct SchemeDataMeta {
  loc: Option<Location>,
  env: Option<Rc<RefCell<Env>>>,
  data: Option<Box<SchemeData>>
}

// TODO: scheme 里面的 data 都是保存在 heap 里面的，我们这里需要用 Rc 和 RefCell 包裹吗？
#[derive(Debug, PartialEq, Clone)]
pub enum SchemeData {
  Nil, // only for SchemeData::List
  Identifier(String, SchemeDataMeta),
  Number(f64, SchemeDataMeta),
  String(String, SchemeDataMeta),
  Boolean(bool, SchemeDataMeta),
  List((Box<SchemeData>, Box<SchemeData>), SchemeDataMeta),
  Continuation(
    fn(SchemeData) -> SchemeData,
    SchemeDataMeta
  ),
  Procedure(
    (String, Box<SchemeData>, Box<SchemeData>, Rc<Env>),
    SchemeDataMeta,
  ),
}

#[derive(Debug)]
pub struct ParseError {
  msg: String,
}

impl SchemeData {
  pub fn get_loc(&self) -> Option<Location> {
    match self {
      SchemeData::Identifier(_, meta) => meta.loc.clone(),
      SchemeData::Number(_, meta) => meta.loc.clone(),
      SchemeData::String(_, meta) => meta.loc.clone(),
      SchemeData::Boolean(_, meta) => meta.loc.clone(),
      SchemeData::List(_, meta) => meta.loc.clone(),
      SchemeData::Continuation(_, meta) => meta.loc.clone(),
      SchemeData::Procedure(_, meta) => meta.loc.clone(),
      _ => None,
    }
  }

  pub fn set_loc(&mut self, new_loc: Location) -> &Self {
    match self {
      SchemeData::Identifier(_, meta) => meta.loc = Some(new_loc),
      SchemeData::Number(_, meta) => meta.loc = Some(new_loc),
      SchemeData::String(_, meta) => meta.loc = Some(new_loc),
      SchemeData::Boolean(_, meta) => meta.loc = Some(new_loc),
      SchemeData::List(_, meta) => meta.loc = Some(new_loc),
      SchemeData::Continuation(_, meta) => meta.loc = Some(new_loc),
      SchemeData::Procedure(_, meta) => meta.loc = Some(new_loc),
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
    let Some(last_loc) = if let Some(last) = list.last() {
      last.get_loc()
    } else {
      Default::default()
    };

    while let Some(item) = list.pop() {
      if let Some(loc) = item.get_loc() {
        data = SchemeData::List(
          (Box::new(item), Box::new(data)),
          SchemeDataMeta {
            data: None,
            env: None,
            loc: Some(Location {
              line_start: loc.line_start,
              column_start: loc.column_start,
              line_end: last_loc.line_end,
              column_end: last_loc.column_end,
            })
          }
        )
      }
    }

    data
  }

  pub fn parse_list_from_end(
    list: &mut Vec<TokenItem>,
    left_paren_loc: Location,
  ) -> Result<SchemeData, ParseError> {
    let mut scheme_data_list: Vec<SchemeData> = vec![];

    while let Some(TokenItem { token, loc }) = list.pop() {
      scheme_data_list.push(match token {
        // ignore WhiteSpace and EOL
        TokenType::WhiteSpace => continue,
        TokenType::EOL => continue,
        TokenType::Identifier(item) => SchemeData::Identifier(item, SchemeDataMeta { loc: Some(loc), data: None, env: None }),
        TokenType::Number(item) => SchemeData::Number(item, SchemeDataMeta { loc: Some(loc), data: None, env: None }),
        TokenType::String(item) => SchemeData::String(item, SchemeDataMeta { loc: Some(loc), data: None, env: None }),
        TokenType::Boolean(item) => SchemeData::Boolean(item, SchemeDataMeta { loc: Some(loc), data: None, env: None }),
        TokenType::Quote => {
          if let Some(peek) = list.last() {
            match peek.token {
              TokenType::LParen => {
                SchemeData::parse_list_from_end(list, SchemeData::build_default_loc())?
              }
              _ => SchemeData::parse_list_from_end(
                &mut vec![list.pop().unwrap()],
                SchemeData::build_default_loc(),
              )?,
            }
          } else {
            SchemeData::Nil
          }
        }
        TokenType::LParen => SchemeData::parse_list_from_end(list, loc)?,
        TokenType::RParen => {
          return if left_paren_loc
            == (Location {
              ..Default::default()
            }) {
            Err(ParseError {
              msg: "Right parens are more than left parens!".to_string(),
            })
          } else {
            let mut data = SchemeData::build_list_from_vec(&mut scheme_data_list);
            data.set_loc(Location {
              line_start: left_paren_loc.line_start,
              column_start: left_paren_loc.column_start,
              line_end: loc.line_end,
              column_end: loc.column_end,
            });
            Ok(data)
          }
        }
        _ => {
          return Err(ParseError {
            msg: "Unexpected token!".to_string(),
          })
        }
      })
    }

    if left_paren_loc
      != (Location {
        ..Default::default()
      })
    {
      return Err(ParseError {
        msg: "Left parens are more than left parens!".to_string(),
      });
    }

    Ok(SchemeData::build_list_from_vec(&mut scheme_data_list))
  }
}

pub fn parse(mut list: Vec<TokenItem>) -> Result<SchemeData, ParseError> {
  list.reverse();
  SchemeData::parse_list_from_end(&mut list, SchemeData::build_default_loc())
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
      SchemeData::List(
        (
          Box::new(SchemeData::List(
            (
              Box::new(SchemeData::Identifier(
                "+".to_string(),
                Location {
                  line_start: 1,
                  column_start: 2,
                  line_end: 1,
                  column_end: 3
                }
              )),
              Box::new(SchemeData::List(
                (
                  Box::new(SchemeData::Number(
                    1 as f64,
                    Location {
                      line_start: 1,
                      column_start: 4,
                      line_end: 1,
                      column_end: 5
                    }
                  )),
                  Box::new(SchemeData::List(
                    (
                      Box::new(SchemeData::Number(
                        2 as f64,
                        Location {
                          line_start: 1,
                          column_start: 6,
                          line_end: 1,
                          column_end: 7
                        }
                      )),
                      Box::new(SchemeData::Nil)
                    ),
                    Location {
                      line_start: 1,
                      column_start: 6,
                      line_end: 1,
                      column_end: 7
                    }
                  ))
                ),
                Location {
                  line_start: 1,
                  column_start: 4,
                  line_end: 1,
                  column_end: 7
                }
              ))
            ),
            Location {
              line_start: 1,
              column_start: 1,
              line_end: 1,
              column_end: 8
            }
          )),
          Box::new(SchemeData::Nil)
        ),
        Location {
          line_start: 1,
          column_start: 1,
          line_end: 1,
          column_end: 8
        }
      )
    );
  }
}
