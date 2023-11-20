use crate::{parser::SchemeExp, env::Env};

use super::{ Evaluator, Cell, CellName };

/**
 * 语法：
 * (if predict then_value else_value)
 */
impl Evaluator {
  pub fn parse_if_clause(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    let predict_node = node.value.pop_front();
    let then_node = node.value.pop_front();
    let else_node = node.value.pop_front();

    // parse then_value
    let then_cid = match then_node {
      Some(x) => self.parse(x, env.copy(), next),
      _ => panic!("Parse then-clause error!")
    };

    // parse then_value
    let else_cid = match else_node {
      Some(x) => self.parse(x, env.copy(), next),
      _ => panic!("Parse then-clause error!")
    };

    let if_cid = self.insert_map(Cell::new(
      CellName::IfClause,
      vec![],
      env.copy(),
      node.loc.clone(),
      vec![then_cid, else_cid]
    ));

    // parse predict
    match predict_node {
      Some(x) => self.parse(x, env.copy(), if_cid),
      _ => panic!("Parse then-clause error!")
    }
  }

  pub fn eval_if_clause(&mut self) -> Option<Cell> {
    None
  }
}