use crate::{parser::SchemeExp, env::Env};

use super::{ Evaluator, Cell };

/**
 * 语法（后面加一个 lambda 语句）：
 * (call-with-current-continuation
 *    (lambda (resume-here)
 *      (set! control-state resume-here)
 *      (return element)))
 */
impl Evaluator {
  pub fn parse_call_cc(&self, node: SchemeExp, env: Env) -> usize {
    panic!()
  }

  pub fn eval_call_cc(&mut self) -> Option<Cell> {
    None
  }
}