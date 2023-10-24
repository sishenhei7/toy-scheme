use std::collections::HashMap;

mod begin;
mod buildin;
mod call_cc;
mod cond;
mod cont;
mod define;
mod if_clause;
mod lambda;
mod let_clause;
mod proc;
mod set;

use anyhow::Error;

use crate::{
  env::Env,
  parser::{SchemeCont, SchemeData, SchemeExp}, lexer::Location,
};

#[derive(Debug)]
pub enum CellName {
  Value,
  Begin,
  Callcc,
  Cond,
  Cont,
  Define,
  IfClause,
  Lambda,
  LetClause,
  Proc,
  Set
}

pub struct Cell {
  pub name: CellName,
  pub params: Vec<SchemeData>,
  pub env: Env,
  pub loc: Option<Location>,
  pub next: Option<Box<Cell>>,
  pub prev: Option<Box<Cell>>
}

impl Cell {
  pub fn new(name: CellName, params: Vec<SchemeData>, env: Env, loc: Option<Location>) -> Self {
    Self {
      name,
      params,
      env,
      loc,
      next: None,
      prev: None
    }
  }
}

pub struct Evaluator {
  pub initial_input: SchemeData,
  pub initial_env: Env,
  pub cell_chain: Option<Cell>,
  pub cell_map:HashMap<usize, Cell>,
  pub cid: usize
}

impl Evaluator {
  pub fn new(input: SchemeData) -> Self {
    Self {
      initial_input: input.clone(),
      initial_env: Env::new(),
      cell_chain: None,
      cell_map: HashMap::new(),
      cid: 1
    }
  }

  // 从第一个开始进行匹配 begin、callcc 等，没匹配到则视为单独的求值
  // 从左往右一次求值，最后一个的结果是这个 exp 的值
  pub fn parse(&self, node: SchemeData, env: Env) -> Option<Cell> {
    match node {
      SchemeData::Identifier(ref _x) => self.parse_identifier(node, env),
      SchemeData::Number(ref _x) => Some(Cell::new(
        CellName::Value,
        vec![node.clone()],
        env.copy(),
        node.get_loc()
      )),
      SchemeData::String(ref _x) => Some(Cell::new(
        CellName::Value,
        vec![node.clone()],
        env.copy(),
        node.get_loc()
      )),
      SchemeData::Boolean(ref _x) => Some(Cell::new(
        CellName::Value,
        vec![node.clone()],
        env.copy(),
        node.get_loc()
      )),
      SchemeData::List(ref _x) => Some(Cell::new(
        CellName::Value,
        vec![node.clone()],
        env.copy(),
        node.get_loc()
      )),
      SchemeData::Continuation(ref _x) => None,
      SchemeData::Procedure(ref _x) => None,
      SchemeData::Exp(ref x) => self.parse_exp(x.clone(), env),
      _ => None,
    }
  }

  // use VecDeque ?
  pub fn parse_exp(&self, node: SchemeExp, env: Env) -> Option<Cell> {
    let mut last_cell = None;
    while let Some(&data) = node.value.first() {
      let cell = match data {
        SchemeData::Identifier(ref x) => {
          match x.value.as_str() {
            "begin" => self.parse_begin(node, env),
            _ => None
          }
        },
        SchemeData::Number(ref _x) => {
          Some(Cell::new(
            CellName::Value,
            vec![data.clone()],
            env.copy(),
            data.get_loc()
          ))
        },
        SchemeData::String(ref _x) => Some(Cell::new(
          CellName::Value,
          vec![data.clone()],
          env.copy(),
          data.get_loc()
        )),
        SchemeData::Boolean(ref _x) => Some(Cell::new(
          CellName::Value,
          vec![data.clone()],
          env.copy(),
          data.get_loc()
        )),
        SchemeData::List(ref _x) => Some(Cell::new(
          CellName::Value,
          vec![data.clone()],
          env.copy(),
          data.get_loc()
        )),
        SchemeData::Continuation(ref _x) => None,
        SchemeData::Procedure(ref _x) => None,
        SchemeData::Exp(ref x) => self.parse_exp(x.clone(), env),
        _ => None
      };
    };
    None
  }

  pub fn parse_identifier(&self, node: SchemeData, env: Env) -> Option<Cell> {
    None
  }
}

// pub enum EvaluateResponse {
//   Data(SchemeData),
//   Cont(SchemeCont)
// }

// pub fn evaluate(
//   data: &SchemeData,
//   env: &Env,
//   cont: &SchemeCont,
// ) -> Result<EvaluateResponse, Error> {
//   match *data.0.borrow() {
//     BaseSchemeData::Exp(ref x) => evaluate_exp(x, env, cont),
//     BaseSchemeData::Identifier(ref identifier) => match env.get(&identifier.value) {
//       Some(ref x) => Ok(EvaluateResponse::Cont(SchemeCont {
//         func: cont.func.clone(),
//         data: Some(x.clone()),
//         loc: data.get_loc(),
//         env: env.clone(),
//       })),
//       None => Err(Error::msg("Evaluate Error!")),
//     },
//     _ => Ok(EvaluateResponse::Cont(SchemeCont {
//       func: cont.func.clone(),
//       data: Some(data.clone()),
//       loc: data.get_loc(),
//       env: env.clone(),
//     })),
//   }
// }

// pub fn evaluate_exp(
//   data: &SchemeExp,
//   env: &Env,
//   cont: &SchemeCont,
// ) -> Result<EvaluateResponse, Error> {
//   match data.value.get(0) {
//     Some(x) => {
//       match *x.0.borrow() {
//         BaseSchemeData::Identifier(ref sym) => {
//           match sym.value.as_str() {
//             "begin" => begin::evaluate(&data, &env, &cont),
//             "call-with-current-continuation" => call_cc::evaluate(&data, &env, &cont),
//             "cond" => cond::evaluate(&data, &env, &cont),
//             "cont" => cont::evaluate(&data, &env, &cont),
//             "define" => define::evaluate(&data, &env, &cont),
//             "if" => if_clause::evaluate(&data, &env, &cont),
//             "lambda" => lambda::evaluate(&data, &env, &cont),
//             "let" => let_clause::evaluate_let(&data, &env, &cont),
//             "let*" => let_clause::evaluate_letstar(&data, &env, &cont),
//             "letrc" => let_clause::evaluate_letrc(&data, &env, &cont),
//             "proc" => proc::evaluate(&data, &env, &cont),
//             "set!" => set::evaluate(&data, &env, &cont),
//             _ => buildin::evaluate(&data, &env, &cont)
//           }
//         },
//         BaseSchemeData::Continuation(ref _cont) => cont::evaluate(&data, &env, &cont),
//         BaseSchemeData::Procedure(ref _proc) => proc::evaluate(&data, &env, &cont),
//         // TODO: 这里有点问题，后续看怎么做？？？？
//         BaseSchemeData::Exp(ref x) => evaluate_exp(x, env, cont),
//         _ => Err(Error::msg("Evaluate expression error!"))
//       }
//     }
//     None => Err(Error::msg("Evaluate expression error!"))
//   }
// }
