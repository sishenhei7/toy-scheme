use crate::{parser::{SchemeExp, SchemeData, SchemeProc}, env::Env};

use super::{ Evaluator, Cell, CellName };

/**
 * 语法：
 * 1.定义变量或函数：
 * (define vhello "Hello world")
 * 2.短形式定义函数：
 * (define (hello name) (string-append "Hello " name "!"))
 */
impl Evaluator {
  pub fn parse_define(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    node.value.pop_front();

    let first = node.value.pop_front().unwrap();

    // 定义变量或函数
    if let SchemeData::Identifier(_x) = first {
      let set_cid = self.insert_map(Cell::new(
        CellName::EnvSet,
        vec![],
        env.copy(),
        None,
        vec![next]
      ));
      let second: SchemeData = node.value.pop_front().unwrap();
      return self.parse(second, env, set_cid)
    }

    // 短形式定义函数
    let first_cloned = first.clone();
    if let SchemeData::Exp(_x) = first {
      let mut second = node.value.pop_front().unwrap();

      if let SchemeData::Exp(ref mut y) = second {
        let name_node = y.value.pop_front().unwrap();

        if let SchemeData::Identifier(z) = name_node {
          let proc = SchemeData::Procedure(SchemeProc {
            name: z.value,
            params: Box::new(first_cloned),
            body: Box::new(second.clone()),
            env: env.extend(None),
            loc: node.loc.clone()
          });

          let set_cid = self.insert_map(Cell::new(
            CellName::EnvSet,
            vec![],
            env.copy(),
            None,
            vec![next]
          ));

          return self.insert_map(Cell::new(
            CellName::Value,
            vec![proc],
            env.copy(),
            node.loc.clone(),
            vec![set_cid]
          ));
        }
      }
    }
    panic!("Parse define-clause error!")
  }

  pub fn eval_define(&mut self) -> Option<Cell> {
    None
  }
}