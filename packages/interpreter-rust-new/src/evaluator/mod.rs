use std::collections::{HashMap, VecDeque};
use std::fmt;

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

// use anyhow::Error;
use crate::{
  env::Env,
  parser::{SchemeData, SchemeExp}, lexer::Location,
};

pub struct Unit {
  pub env: Env,
  pub loc: Option<Location>,
  pub computer: Box<dyn FnMut(SchemeData, &mut Vec<Vec<SchemeData>>) -> (usize, SchemeData)>
}

impl Unit {
  pub fn new(
    env: Env,
    loc: Option<Location>,
    computer: Box<dyn FnMut(SchemeData, &mut Vec<Vec<SchemeData>>) -> (usize, SchemeData)>
  ) -> Self {
    Self {
      env,
      loc,
      computer
    }
  }

  pub fn run(
    &mut self,
    value: SchemeData,
    stack: &mut Vec<Vec<SchemeData>>
  ) -> (usize, SchemeData) {
    (self.computer)(value, stack)
  }
}

impl fmt::Debug for Unit {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "Unit,{:?}", self.env)
  }
}

// 由于所有权的原因，unit 不方便同时放入 unit_map、next 里面(用Rc也可以)
// 所以考虑把所有的 unit 放入 unit_map、next 里面只存索引
pub struct Evaluator {
  pub initial_input: SchemeExp,
  pub initial_env: Env,
  pub unit_map: HashMap<usize, Unit>,
  pub initial_cid: usize,
  pub cid: usize,
  // 用来解决 (+ x (* y yy yyy) xx xxx) 的情况
  pub stack: Vec<Vec<SchemeData>>
}

impl Evaluator {
  pub fn new(input: SchemeExp) -> Self {
    let mut evaluator = Self {
      initial_input: input.clone(),
      initial_env: Env::new(),
      unit_map: HashMap::new(),
      initial_cid: 0,
      cid: 0,
      stack: vec![]
    };
    evaluator.initial_cid = evaluator.evaluate_exp(input, evaluator.initial_env.copy(), 0);
    evaluator
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
  pub fn evaluate(&mut self, node: SchemeData, env: Env, next: usize) -> usize {
    match node {
      SchemeData::Identifier(ref x) => {
        let identifier = x.value.clone();
        let env_copy = env.copy();
        self.insert_map(Unit::new(
          env.copy(),
          node.get_loc(),
          Box::new(move |_, _| {
            println!("{:?},{:?}", &identifier, env_copy);
            let node = env_copy.get(&identifier).expect("Env get 错误！");
            (next, node)
          })
        ))
      },
      SchemeData::Number(..)
        | SchemeData::String(..)
        | SchemeData::Boolean(..)
        | SchemeData::List(..) => {
        let node_copy = node.clone();
        self.insert_map(Unit::new(
          env.copy(),
          node.get_loc(),
          Box::new(move |_, _| (next, node_copy.clone()))
        ))
      },
      SchemeData::Exp(ref x) => self.evaluate_exp(x.clone(), env, next),
      _ => panic!(),
    }
  }

  pub fn evaluate_exp(&mut self, node: SchemeExp, env: Env, next: usize) -> usize {
    match node.value.front() {
      // 匹配上了语法
      Some(SchemeData::Identifier(ref x)) => {
        let identifier = x.value.as_str();
        match identifier {
          "begin" => self.evaluate_begin(node, env, next),
          "call-with-current-continuation" => self.evaluate_call_cc(node, env, next),
          "cond" => self.evaluate_cond(node, env, next),
          "define" => self.evaluate_define(node, env, next),
          "if" => self.evaluate_if(node, env, next),
          "lambda" => self.evaluate_lambda(node, env, next),
          "let" | "let*" | "letrec" => self.evaluate_let(node, env, next),
          "set!" => self.evaluate_set(node, env, next),
          _ => self.evaluate_buildin(node, env, next)
        }
      },
      Some(SchemeData::Procedure(..)) => self.evaluate_proc(node, env, next),
      Some(SchemeData::Continuation(..)) => self.evaluate_cont(node, env, next),
      _ => self.evaluate_from_left(node.value, env, next),
    }
  }

  // 从左到右依次求值，返回最后一个
  // 这里的每一项都应该丢弃之前的返回值，只返回自己
  pub fn evaluate_from_left(&mut self, queue: VecDeque<SchemeData>, env: Env, next: usize) -> usize {
    queue.into_iter().rev().fold(next, |acc, cur| {
      let value_cid = self.evaluate(cur, env.copy(), acc);
      self.insert_map(Unit::new(
        env.copy(),
        None,
        Box::new(move |_, _| (value_cid, SchemeData::Nil))
      ))
    })
  }

  pub fn push_stack(&mut self, node: SchemeData) -> () {
    let last_stack = self.stack.last_mut().expect("Evaluator push stack error!");
    last_stack.push(node);
  }

  pub fn add_stack(&mut self) -> () {
    self.stack.push(vec![]);
  }

  pub fn pop_stack(&mut self) -> Vec<SchemeData> {
    self.stack.pop().expect("Evaluator pop stack error!")
  }

  pub fn run_unit(&mut self, cid: usize, value: SchemeData) -> (usize, SchemeData) {
    let map = &mut self.unit_map;
    let unit = map.get_mut(&cid).expect("Evaluator run error!");
    unit.run(value, &mut self.stack)
  }

  pub fn run(&mut self) -> SchemeData {
    let mut cid = self.initial_cid;
    let mut value = SchemeData::Nil;
    while let Some(x) = self.unit_map.get_mut(&cid) {
      (cid, value) = x.run(value, &mut self.stack);
      println!("{:?},{:?}", cid, value);
    }
    value
  }
}
