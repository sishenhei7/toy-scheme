use crate::{parser::SchemeExp, env::Env};

use super::{ Evaluator, Unit };

impl Evaluator {
  pub fn evaluate_buildin(&self, node: SchemeExp, env: Env, next: usize) -> () {
    ()
  }
}