use std::collections::VecDeque;
use anyhow::Error;

use crate::{
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
  pub value: (Box<SchemeData>, Box<SchemeData>),
  pub loc: Option<Location>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SchemeCont {
  pub value: usize, // 所有的 cont 都保存到一个数组里面去
  pub loc: Option<Location>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SchemeProc {
  pub name: String,
  pub params: Box<SchemeData>,
  pub body: Box<SchemeData>,
  pub env: Env, // proc 自带一个属于自己的 env
  pub loc: Option<Location>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SchemeExp {
  pub value: VecDeque<SchemeData>,
  pub loc: Option<Location>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum SchemeData {
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
      value: ($car, $cdr),
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
      SchemeData::Identifier(ref x) => x.loc.clone(),
      SchemeData::Number(ref x) => x.loc.clone(),
      SchemeData::String(ref x) => x.loc.clone(),
      SchemeData::Boolean(ref x) => x.loc.clone(),
      SchemeData::List(ref x) => x.loc.clone(),
      SchemeData::Continuation(ref x) => x.loc.clone(),
      SchemeData::Procedure(ref x) => x.loc.clone(),
      SchemeData::Exp(ref x) => x.loc.clone(),
      _ => None,
    }
  }

  pub fn set_loc(&mut self, new_loc: Location) -> &Self {
    match self {
      SchemeData::Identifier(ref mut x) => x.loc = Some(new_loc),
      SchemeData::Number(ref mut x) => x.loc = Some(new_loc),
      SchemeData::String(ref mut x) => x.loc = Some(new_loc),
      SchemeData::Boolean(ref mut x) => x.loc = Some(new_loc),
      SchemeData::List(ref mut x) => x.loc = Some(new_loc),
      SchemeData::Continuation(ref mut x) => x.loc = Some(new_loc),
      SchemeData::Procedure(ref mut x) => x.loc = Some(new_loc),
      SchemeData::Exp(ref mut x) => x.loc = Some(new_loc),
      _ => panic!(),
    };
    self
  }

  pub fn cons(a: &SchemeData, b: &SchemeData) -> SchemeData {
    let loc = a.get_loc().unwrap_or_default();
    let last_loc = b.get_loc().unwrap_or(loc.clone());
    build_list!(
      Box::new(a.clone()),
      Box::new(b.clone()),
      Some(Location {
        line_start: loc.line_start,
        column_start: loc.column_start,
        line_end: last_loc.line_end,
        column_end: last_loc.column_end,
      })
    )
  }

  pub fn cons_from_vec(vec: &mut VecDeque<SchemeData>) -> SchemeData {
    vec.iter().fold(SchemeData::Nil, |acc, x| SchemeData::cons(x, &acc))
  }

  // this will consume the list
  pub fn parse_token_list(list: &mut Vec<TokenItem>, start_loc: Location) -> Result<SchemeExp, Error> {
    let mut scheme_data_list = vec![];
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
          // 'xxxx 和 '(xx, xxx)
          if let Some(peek) = list.last() {
            let mut quote_exp = match peek.token {
              TokenType::LParen => SchemeData::parse_token_list(list, loc)?,
              _ => SchemeData::parse_token_list(&mut vec![list.pop().unwrap()], loc)?,
            };
            SchemeData::cons_from_vec(&mut quote_exp.value)
          } else {
            return Err(Error::msg("Quote parsing error!"));
          }
        }
        TokenType::LParen => {
          let paren_exp = SchemeData::parse_token_list(list, loc)?;
          SchemeData::Exp(paren_exp)
        }
        TokenType::RParen => {
          return Ok(SchemeExp {
            value: VecDeque::from(scheme_data_list),
            loc: Some(Location {
              line_start: start_loc.line_start,
              column_start: start_loc.column_start,
              line_end: loc.line_end,
              column_end: loc.column_end,
            }),
          });
        }
        _ => return Err(Error::msg("Unexpected token!")),
      })
    }

    return if let Some(last_data) = scheme_data_list.last() {
      let last_loc = last_data.get_loc().unwrap_or_default();
      Ok(SchemeExp {
        value: VecDeque::from(scheme_data_list),
        loc: Some(Location {
          line_start: start_loc.line_start,
          column_start: start_loc.column_start,
          line_end: last_loc.line_end,
          column_end: last_loc.column_end,
        }),
      })
    } else {
      Err(Error::msg("Parsing null!"))
    };
  }

  pub fn get_exp_queue(&mut self) -> Option<&mut VecDeque<SchemeData>> {
    if let SchemeData::Exp(x) = self {
      Some(&mut x.value)
    } else {
      None
    }
  }

  pub fn get_identifier_string(& self) -> Option<&String> {
    if let SchemeData::Identifier(x) = self {
      Some(&x.value)
    } else {
      None
    }
  }

  pub fn get_proc(&mut self) -> Option<&mut SchemeProc> {
    if let SchemeData::Procedure(x) = self {
      Some(x)
    } else {
      None
    }
  }

  pub fn get_boolean(&mut self) -> Option<bool> {
    if let SchemeData::Boolean(x) = self {
      Some(x.value)
    } else {
      None
    }
  }

  pub fn get_cont_value(&mut self) -> Option<usize> {
    if let SchemeData::Continuation(x) = self {
      Some(x.value)
    } else {
      None
    }
  }

  pub fn get_number(&mut self) -> Option<f64> {
    if let SchemeData::Number(x) = self {
      Some(x.value)
    } else {
      None
    }
  }

  pub fn get_list(&mut self) -> Option<(Box<SchemeData>, Box<SchemeData>)> {
    if let SchemeData::List(x) = self {
      Some(x.value.clone())
    } else {
      None
    }
  }

  pub fn get_list_car(&mut self) -> Option<Box<SchemeData>> {
    if let SchemeData::List(x) = self {
      Some(x.value.0.clone())
    } else {
      None
    }
  }

  pub fn get_list_cdr(&mut self) -> Option<Box<SchemeData>> {
    if let SchemeData::List(x) = self {
      Some(x.value.1.clone())
    } else {
      None
    }
  }

  pub fn get_list_length(&mut self) -> Option<f64> {
    fn get_list_length_func(mut node: SchemeData) -> f64 {
      match node {
        SchemeData::List(..) => 1.0 + get_list_length_func(*node.get_list_cdr().unwrap()),
        SchemeData::Nil => 0.0,
        _ => 0.0
      }
    }

    if let SchemeData::List(..) = self {
      let self_copy = self.clone();
      Some(get_list_length_func(self_copy))
    } else {
      None
    }
  }

  pub fn add(&mut self, node: &mut SchemeData) -> () {
    self.get_number().expect("SchemeData add error!");
    let num = node.get_number().expect("SchemeData add error!");
    if let SchemeData::Number(ref mut x) = self {
      x.value += num
    }
  }

  pub fn minus(&mut self, node: &mut SchemeData) -> () {
    self.get_number().expect("SchemeData minus error!");
    let num = node.get_number().expect("SchemeData minus error!");
    if let SchemeData::Number(ref mut x) = self {
      x.value -= num
    }
  }

  pub fn multiply(&mut self, node: &mut SchemeData) -> () {
    self.get_number().expect("SchemeData multiply error!");
    let num = node.get_number().expect("SchemeData multiply error!");
    if let SchemeData::Number(ref mut x) = self {
      x.value *= num
    }
  }

  pub fn divide(&mut self, node: &mut SchemeData) -> () {
    self.get_number().expect("SchemeData divide error!");
    let num = node.get_number().expect("SchemeData divide error!");
    if let SchemeData::Number(ref mut x) = self {
      x.value /= num
    }
  }

  pub fn min(&mut self, node: &mut SchemeData) -> () {
    self.get_number().expect("SchemeData min error!");
    let num = node.get_number().expect("SchemeData min error!");
    if let SchemeData::Number(ref mut x) = self {
      x.value = x.value.min(num);
    }
  }

  pub fn max(&mut self, node: &mut SchemeData) -> () {
    self.get_number().expect("SchemeData max error!");
    let num = node.get_number().expect("SchemeData max error!");
    if let SchemeData::Number(ref mut x) = self {
      x.value = x.value.max(num);
    }
  }

  pub fn abs(&mut self) -> () {
    self.get_number().expect("SchemeData abs error!");
    if let SchemeData::Number(ref mut x) = self {
      x.value = x.value.abs();
    }
  }

  pub fn is_nil(& self) -> bool {
    matches!(*self, SchemeData::Nil)
  }
}

// list is moved
pub fn parse(list: Vec<TokenItem>) -> Result<SchemeExp, Error> {
  SchemeData::parse_token_list(&mut list.into_iter().rev().collect(), Default::default())
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer::Lexer;

  #[test]
  fn test_add() {
    let tokens = Lexer::new("(+ 1 2)").collect::<Vec<TokenItem>>();
    let data = parse(tokens).unwrap_or(SchemeExp {
      value: VecDeque::new(),
      loc: None,
    });
    assert_eq!(
      data,
      SchemeExp {
        value: VecDeque::from([build_exp!(
          VecDeque::from([
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
          ]),
          Some(Location {
            line_start: 1,
            column_start: 1,
            line_end: 1,
            column_end: 8
          })
        )]),
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
