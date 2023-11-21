use crate::{parser::SchemeExp, env::Env};

use super::{ Evaluator, Unit };

/**
 * 语法：
 * 1.let 表达式：
 * (let ((i 1) (j 2))
 *    (+ i j))
 * 2.let* 表达式(可以在定义中引用之前定义的变量)：
 * (let* ((i 1) (j (+ i 2)))
 *    (* i j))
 * 3.letrec 表达式(可以在定义中递归的引用自己)：
 * (letrec ((iter (lambda (ls0 n)
 *    (if (null? ls0)
 *    n
 *    (iter (cdr ls0) (+ (car ls0) n))))))
 *  (iter ls 0))
 */
impl Evaluator {
  pub fn parse_let_clause(&self, node: SchemeExp, env: Env, next: usize) -> usize {
    panic!()
  }

  pub fn eval_let_clause(&mut self) -> Option<Unit> {
    None
  }
}