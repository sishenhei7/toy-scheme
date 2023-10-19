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
pub enum EvaluatorList {
  Begin,
  Buildin(Buildin),
  Callcc,
  Cond,
  Cont,
  Define,
  IfClause,
  Lambda,
  LetClause,
  Proc,
  Set,
}

#[derive(Debug)]
pub enum Buildin {
  Cons,
  IsNull,
  Car,
  Cdr,
  Cadr,
  Equal,
  MoreThan,
  LessThan,
  Add,
}

pub struct Cell {
  pub env: Env,
  pub name: String,
  pub params: Vec<SchemeData>,
  pub next: Option<Box<Cell>>,
  pub prev: Option<Box<Cell>>,
  pub loc: Location
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

  pub fn parse(&self) -> Option<Cell> {
    None
  }

  pub fn parse_exp(&self) -> Option<Cell> {
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
