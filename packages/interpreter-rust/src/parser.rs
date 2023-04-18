use core::panic;
use std::rc::Rc;
use std::vec;

use crate::env::Env;
use crate::lexer::*;

#[derive(Debug, PartialEq)]
pub enum SchemeData {
  Nil, // only for SchemeData::List
  Identifier(String, Location),
  Number(f64, Location),
  String(String, Location),
  Boolean(bool, Location),
  List((Box<SchemeData>, Box<SchemeData>), Location),
  Continuation(fn(SchemeData) -> SchemeData, Location),
  Procedure(
    (String, Box<SchemeData>, Box<SchemeData>, Rc<Env>),
    Location,
  ),
}

#[derive(Debug)]
pub struct ParseError {
  msg: String,
}

impl SchemeData {
  pub fn get_loc(&self) -> Location {
    match self {
      SchemeData::Identifier(_, loc) => loc.clone(),
      SchemeData::Number(_, loc) => loc.clone(),
      SchemeData::String(_, loc) => loc.clone(),
      SchemeData::Boolean(_, loc) => loc.clone(),
      SchemeData::List(_, loc) => loc.clone(),
      SchemeData::Continuation(_, loc) => loc.clone(),
      SchemeData::Procedure(_, loc) => loc.clone(),
      _ => panic!()
    }
  }

  pub fn set_loc(&mut self, new_loc: Location) -> &Self {
    match self {
      SchemeData::Identifier(_, loc) => *loc = new_loc,
      SchemeData::Number(_, loc) => *loc = new_loc,
      SchemeData::String(_, loc) => *loc = new_loc,
      SchemeData::Boolean(_, loc) => *loc = new_loc,
      SchemeData::List(_, loc) => *loc = new_loc,
      SchemeData::Continuation(_, loc) => *loc = new_loc,
      SchemeData::Procedure(_, loc) => *loc = new_loc,
      _ => panic!()
    };
    self
  }

  pub fn build_default_loc() -> Location {
    Location { ..Default::default() }
  }

  pub fn build_list_from_vec(list: &mut Vec<SchemeData>) -> SchemeData {
    let mut data = SchemeData::Nil;
    let last_loc = if let Some(last) = list.last() {
      last.get_loc()
    } else {
      Default::default()
    };

    while let Some(item) = list.pop() {
      let loc = item.get_loc();
      data = SchemeData::List(
        (Box::new(item), Box::new(data)),
        Location {
          line_start: loc.line_start,
          column_start: loc.column_start,
          line_end: last_loc.line_end,
          column_end: last_loc.column_end,
        },
      )
    }

    data
  }

  pub fn parse_list_from_end(list: &mut Vec<TokenItem>, left_paren_loc: Location) -> SchemeData {
    let mut scheme_data_list: Vec<SchemeData> = vec![];

    while let Some(TokenItem { token, loc }) = list.pop() {
      scheme_data_list.push(match token {
        // ignore WhiteSpace and EOL
        TokenType::WhiteSpace => continue,
        TokenType::EOL => continue,
        TokenType::Identifier(item) => SchemeData::Identifier(item, loc),
        TokenType::Number(item) => SchemeData::Number(item, loc),
        TokenType::String(item) => SchemeData::String(item, loc),
        TokenType::Boolean(item) => SchemeData::Boolean(item, loc),
        TokenType::Quote => {
          if let Some(peek) = list.last() {
            match peek.token {
              TokenType::LParen => SchemeData::parse_list_from_end(list, SchemeData::build_default_loc()),
              _ => SchemeData::parse_list_from_end(&mut vec![list.pop().unwrap()], SchemeData::build_default_loc()),
            }
          } else {
            SchemeData::Nil
          }
        }
        TokenType::LParen => {
          SchemeData::parse_list_from_end(list, loc)
        }
        TokenType::RParen => {
          if left_paren_loc == (Location { ..Default::default() }) {
            // Err(ParseError { msg: String::from("Right parens are more than left parens!") })
            panic!("Right parens are more than left parens!")
          } else {
            let mut data = SchemeData::build_list_from_vec(&mut scheme_data_list);
            data.set_loc(Location {
              line_start: left_paren_loc.line_start,
              column_start: left_paren_loc.column_start,
              line_end: loc.line_end,
              column_end: loc.column_end
            });
            return data
          }
        }
        _ => panic!("Unexpected token!")
        //  _ => return Err(ParseError { msg: String::from("Unexpected token!") })
      })
    }

    if left_paren_loc != (Location { ..Default::default() }) {
      panic!("Left parens are more than left parens!")
    }

    // TODO: refine code
    SchemeData::build_list_from_vec(&mut scheme_data_list)
  }
}

pub fn parse(mut list: Vec<TokenItem>) -> SchemeData {
  list.reverse();
  SchemeData::parse_list_from_end(&mut list, SchemeData::build_default_loc())
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_add() {
    let tokens = tokenize("(+ 1 2)").unwrap_or(vec![]);
    let data = parse(tokens);
    assert_eq!(
      data,
      SchemeData::List(
        (
          Box::new(SchemeData::List(
            (
              Box::new(SchemeData::Identifier(
                String::from("+"),
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
