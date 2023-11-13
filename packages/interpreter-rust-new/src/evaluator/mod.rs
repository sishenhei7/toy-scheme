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
  Identifier,
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
  pub next: Option<usize>
}

impl Cell {
  pub fn new(name: CellName, params: Vec<SchemeData>, env: Env, loc: Option<Location>) -> Self {
    Self {
      name,
      params,
      env,
      loc,
      next: None
    }
  }
}

// 由于所有权的原因，cell 不方便同时放入 head、tail、cell_map、next 里面(用Rc也可以)
// 所以考虑把所有的 cell 放入 cell_map，head、tail、next 里面只存索引
pub struct Evaluator {
  pub initial_input: SchemeData,
  pub initial_env: Env,
  pub head: Option<usize>,
  pub tail: Option<usize>,
  pub cell_map: HashMap<usize, Option<Box<Cell>>>,
  pub cid: usize
}

impl Evaluator {
  pub fn new(input: SchemeData) -> Self {
    Self {
      initial_input: input.clone(),
      initial_env: Env::new(),
      head: None,
      tail: None,
      cell_map: HashMap::new(),
      cid: 1
    }
  }

  pub fn append(&mut self, value: Option<Box<Cell>>) -> usize {
    self.cid += 1;
    self.cell_map.insert(self.cid, value);
    self.modify_tail_next(self.cid);
    self.tail = Some(self.cid);
    self.cid
  }

  fn modify_tail_next(&mut self, cid: usize) -> () {
    // 修改 next 的指向
    if let Some(x) = self.tail {
      if let Some(y) = self.cell_map.get_mut(&x) {
        if let Some(z) = y {
          z.next = Some(cid)
        }
      }
    };
  }

  // 从第一个开始进行匹配 begin、callcc 等，没匹配到则视为单独的求值
  // 从左往右一次求值，最后一个的结果是这个 exp 的值
  // this consumes the node
  pub fn parse(&mut self, node: SchemeData, env: Env) -> () {
    match node {
      SchemeData::Identifier(ref _x) => {
        self.append(Some(Box::new(Cell::new(
          CellName::Identifier,
          vec![node.clone()],
          env.copy(),
          node.get_loc()
        ))));
      },
      SchemeData::Number(ref _x) => {
        self.append(Some(Box::new(Cell::new(
          CellName::Value,
          vec![node.clone()],
          env.copy(),
          node.get_loc()
        ))));
      },
      SchemeData::String(ref _x) => {
        self.append(Some(Box::new(Cell::new(
          CellName::Value,
          vec![node.clone()],
          env.copy(),
          node.get_loc()
        ))));
      },
      SchemeData::Boolean(ref _x) => {
        self.append(Some(Box::new(Cell::new(
          CellName::Value,
          vec![node.clone()],
          env.copy(),
          node.get_loc()
        ))));
      },
      SchemeData::List(ref _x) => {
        self.append(Some(Box::new(Cell::new(
          CellName::Value,
          vec![node.clone()],
          env.copy(),
          node.get_loc()
        ))));
      },
      SchemeData::Continuation(ref _x) => (),
      SchemeData::Procedure(ref _x) => (),
      SchemeData::Exp(ref x) => self.parse_exp(x.clone(), env),
      _ => (),
    }
  }

  // use VecDeque ?
  // 从左往右一次求值，最后一个的结果是这个 exp 的值
  // this consumes the node
  pub fn parse_exp(&mut self, node: SchemeExp, env: Env) -> () {
    match node.value.front() {
      // 匹配上了语法
      Some(SchemeData::Identifier(ref x)) => {
        match x.value.as_str() {
          "begin" => self.parse_begin(node.clone(), env.copy()),
          "call-with-current-continuation" => self.parse_call_cc(node.clone(), env.copy()),
          _ => ()
        }
      },
      // 没匹配上语法，则从左到右依次 parse
      _ => self.parse_from_left(node, env)
    }
  }

  // 从左到右依次求值，返回最后一个
  pub fn parse_from_left(&mut self, node: SchemeExp, env: Env) -> () {
    for node in node.value.into_iter() {
      self.parse(node, env.copy())
    }
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
