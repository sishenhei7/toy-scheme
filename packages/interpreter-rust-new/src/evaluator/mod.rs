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

pub struct Unit {
  pub env: Env,
  pub loc: Option<Location>,
  pub computer: Box<dyn Fn(SchemeData, Env) -> (usize, SchemeData)>
}

impl Unit {
  pub fn new(
    env: Env,
    loc: Option<Location>,
    computer: Box<dyn Fn(SchemeData, Env) -> (usize, SchemeData)>
  ) -> Self {
    Self {
      env,
      loc,
      computer
    }
  }
}

// 由于所有权的原因，unit 不方便同时放入 unit_map、next 里面(用Rc也可以)
// 所以考虑把所有的 unit 放入 unit_map、next 里面只存索引
pub struct Evaluator {
  pub initial_input: SchemeData,
  pub initial_env: Env,
  pub unit_map: HashMap<usize, Unit>,
  pub cid: usize
}

impl Evaluator {
  pub fn new(input: SchemeData) -> Self {
    Self {
      initial_input: input.clone(),
      initial_env: Env::new(),
      unit_map: HashMap::new(),
      cid: 1
    }
  }

  pub fn insert_map(&mut self, value: Unit) -> usize {
    self.cid += 1;
    self.unit_map.insert(self.cid, value);
    self.cid
  }

  // 从第一个开始进行匹配 begin、callcc 等，没匹配到则视为单独的求值
  // 从左往右一次求值，最后一个的结果是这个 exp 的值
  // this consumes the node
  // TODO: 把这里的 node 和 env 改成引用
  pub fn parse(&mut self, node: SchemeData, env: Env, next: usize) -> usize {
    match node {
      SchemeData::Identifier(ref x) => {
        let identifier = x.value.clone();
        self.insert_map(Unit::new(
          env.copy(),
          node.get_loc(),
          Box::new(move |_, env| {
            let node = env.get(&identifier).expect("Env get 错误！");
            (next, node)
          })
        ))
      },
      SchemeData::Number(..)
        | SchemeData::String(..)
        | SchemeData::Boolean(..)
        | SchemeData::List(..) => {
        self.insert_map(Unit::new(
          env.copy(),
          node.get_loc(),
          Box::new(move |_, _| (next, node.clone()))
        ))
      },
      SchemeData::Continuation(..) => panic!(),
      SchemeData::Procedure(..) => panic!(),
      SchemeData::Exp(ref x) => self.parse_exp(x.clone(), env, next),
      _ => panic!(),
    }
  }

  pub fn parse_exp(&mut self, node: SchemeExp, env: Env, next: usize) -> usize {
    match node.value.front() {
      // 匹配上了语法
      Some(SchemeData::Identifier(ref x)) => {
        match x.value.as_str() {
          "begin" => self.evaluate_begin(node, env.copy(), next),
          "call-with-current-continuation" => self.evaluate_call_cc(node, env.copy(), next),
          "cond" => self.evaluate_cond(node, env.copy(), next),
          "define" => self.evaluate_define(node, env.copy(), next),
          "if" => self.evaluate_if(node, env.copy(), next),
          "lambda" => self.evaluate_lambda(node, env.copy(), next),
          "let" | "let*" | "letrec" => self.evaluate_let(node, env.copy(), next),
          "set!" => self.evaluate_set(node, env.copy(), next),
          _ => panic!()
        }
      },
      Some(SchemeData::Continuation(..)) => self.evaluate_cont(node, env.copy(), next),
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
