use std::collections::{HashMap, VecDeque};

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
pub enum UnitName {
  Value,        // 直接以 params[0] 返回
  EnvGet,       // 以 params[0].value 作为 key 到 env 里面查找
  EnvSet,       // 以 params[0].value 作为 key，params[1] 作为 value，存到 env
  IfElse        // 控制流，如果 params[0] 为真，则跳转 next[0]，否则跳转 next[1]
}

pub struct Unit {
  pub name: UnitName,
  pub params: Vec<SchemeData>,
  pub env: Env,
  pub loc: Option<Location>,
  pub next: Vec<usize>
}

impl Unit {
  pub fn new(
    name: UnitName,
    params: Vec<SchemeData>,
    env: Env,
    loc: Option<Location>,
    next: Vec<usize>
  ) -> Self {
    Self {
      name,
      params,
      env,
      loc,
      next
    }
  }
}

// 由于所有权的原因，cell 不方便同时放入 cell_map、next 里面(用Rc也可以)
// 所以考虑把所有的 cell 放入 cell_map、next 里面只存索引
pub struct Evaluator {
  pub initial_input: SchemeData,
  pub initial_env: Env,
  pub cell_map: HashMap<usize, Unit>,
  pub cid: usize
}

impl Evaluator {
  pub fn new(input: SchemeData) -> Self {
    Self {
      initial_input: input.clone(),
      initial_env: Env::new(),
      cell_map: HashMap::new(),
      cid: 1
    }
  }

  pub fn insert_map(&mut self, value: Unit) -> usize {
    self.cid += 1;
    self.cell_map.insert(self.cid, value);
    self.cid
  }

  // 从第一个开始进行匹配 begin、callcc 等，没匹配到则视为单独的求值
  // 从左往右一次求值，最后一个的结果是这个 exp 的值
  // this consumes the node
  pub fn parse(&mut self, node: SchemeData, env: Env, next: usize) -> usize {
    match node {
      SchemeData::Identifier(ref _x) => {
        self.insert_map(Unit::new(
          UnitName::EnvGet,
          vec![node.clone()],
          env.copy(),
          node.get_loc(),
          vec![next]
        ))
      },
      SchemeData::Number(ref _x) => {
        self.insert_map(Unit::new(
          UnitName::Value,
          vec![node.clone()],
          env.copy(),
          node.get_loc(),
          vec![next]
        ))
      },
      SchemeData::String(ref _x) => {
        self.insert_map(Unit::new(
          UnitName::Value,
          vec![node.clone()],
          env.copy(),
          node.get_loc(),
          vec![next]
        ))
      },
      SchemeData::Boolean(ref _x) => {
        self.insert_map(Unit::new(
          UnitName::Value,
          vec![node.clone()],
          env.copy(),
          node.get_loc(),
          vec![next]
        ))
      },
      SchemeData::List(ref _x) => {
        self.insert_map(Unit::new(
          UnitName::Value,
          vec![node.clone()],
          env.copy(),
          node.get_loc(),
          vec![next]
        ))
      },
      SchemeData::Continuation(ref _x) => panic!(),
      SchemeData::Procedure(ref _x) => panic!(),
      SchemeData::Exp(ref x) => self.parse_exp(x.clone(), env, next),
      _ => panic!(),
    }
  }

  pub fn parse_exp(&mut self, node: SchemeExp, env: Env, next: usize) -> usize {
    match node.value.front() {
      // 匹配上了语法
      Some(SchemeData::Identifier(ref x)) => {
        match x.value.as_str() {
          "begin" => self.parse_begin(node, env.copy(), next),
          "call-with-current-continuation" => self.parse_call_cc(node, env.copy(), next),
          "cond" => self.parse_cond(node, env.copy(), next),
          "cont" => self.parse_cont(node, env.copy(), next),
          "define" => self.parse_define(node, env.copy(), next),
          "if" => self.parse_if_clause(node, env.copy(), next),
          "lambda" => self.parse_lambda(node, env.copy(), next),
          "let" | "let*" | "letrec" => self.parse_let_clause(node, env.copy(), next),
          "set!" => self.parse_set(node, env.copy(), next),
          _ => panic!()
        }
      },
      // 没匹配上语法，则从左到右依次 parse
      _ => self.parse_from_left(node.value, env, next)
    }
  }

  // 从左到右依次求值，返回最后一个
  pub fn parse_from_left(&mut self, queue: VecDeque<SchemeData>, env: Env, next: usize) -> usize {
    queue.into_iter().rev().fold(
      next,
      |acc, cur| self.parse(cur, env.copy(), acc)
    )
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
