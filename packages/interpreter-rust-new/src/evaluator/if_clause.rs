use crate::{parser::SchemeExp, env::Env};

use super::{ Evaluator, Cell };

/**
 * 语法：
 * (if predict then_value else_value)
 */
impl Evaluator {
  pub fn parse_if_clause(&mut self, mut node: SchemeExp, env: Env) -> usize {
    // parse predict
    match node.value.pop_front() {
      Some(x) => self.parse(x, env.copy()),
      _ => panic!("Parse if clause error!")
    };

    // 这里又卡住了，怎么 parse then_value 和 else_value 呢？
    // parse 了之后又怎么跳转呢？
    panic!()
  }

  pub fn eval_if_clause(&mut self) -> Option<Cell> {
    None
  }
}