use core::panic;
use std::rc::Rc;
use std::vec;

use crate::env::Env;
use crate::lexer::*;

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
      _ => panic!(),
    }
  }

  pub fn build_list_from_vec(list: &mut Vec<SchemeData>) -> SchemeData {
    let mut data = SchemeData::Nil;
    let last_loc = if let Some(last) = list.last() {
      last.get_loc()
    } else {
      Default::default()
    };

    if let Some(item) = list.pop() {
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

  pub fn parse_list_from_end(list: &mut Vec<TokenItem>) -> SchemeData {
    let mut scheme_data_list: Vec<SchemeData> = vec![];

    while let Some(TokenItem { token, loc }) = list.pop() {
      scheme_data_list.push(match token {
        TokenType::Identifier(item) => SchemeData::Identifier(item, loc),
        TokenType::Number(item) => SchemeData::Number(item, loc),
        TokenType::String(item) => SchemeData::String(item, loc),
        TokenType::Boolean(item) => SchemeData::Boolean(item, loc),
        TokenType::Quote => {
          if let Some(peek) = list.last() {
            match peek.token {
              TokenType::LParen => SchemeData::parse_list_from_end(list),
              _ => SchemeData::parse_list_from_end(&mut vec![list.pop().unwrap()]),
            }
          } else {
            SchemeData::Nil
          }
        }
        TokenType::LParen => SchemeData::parse_list_from_end(list),
        TokenType::RParen => return SchemeData::build_list_from_vec(&mut scheme_data_list),
        TokenType::WhiteSpace => continue,
        TokenType::EOL => continue,
        _ => continue, // _ => return Err(ParseError { msg: String::from("Unexpected token!") })
      })
    }

    // TODO: refine code
    SchemeData::build_list_from_vec(&mut scheme_data_list)
  }
}

pub fn parse(list: &mut Vec<TokenItem>) -> SchemeData {
  list.reverse();
  SchemeData::parse_list_from_end(list)
}
