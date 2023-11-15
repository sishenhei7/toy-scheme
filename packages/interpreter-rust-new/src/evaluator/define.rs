use crate::{parser::SchemeExp, env::Env};

use super::{ Evaluator, Cell };

/**
 * 语法：
 * 1.定义变量或函数：
 * (define vhello "Hello world")
 * 2.短形式定义函数：
 * (define (hello name) (string-append "Hello " name "!"))
 */
impl Evaluator {
  pub fn parse_define(&self, node: SchemeExp, env: Env) -> () {
    ()
  }

  pub fn eval_define(&mut self) -> Option<Cell> {
    None
  }
}