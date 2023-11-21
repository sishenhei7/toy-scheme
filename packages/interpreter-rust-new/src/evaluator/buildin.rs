use crate::{parser::SchemeExp, env::Env};

use super::{ Evaluator, Unit };

impl Evaluator {
  pub fn parse_buildin(&self, node: SchemeExp, env: Env, next: usize) -> () {
    ()
  }

  pub fn eval_buildin(&mut self) -> Option<Unit> {
    None
  }
}