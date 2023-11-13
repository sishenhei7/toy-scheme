use crate::{parser::SchemeExp, env::Env};

use super::{ Evaluator, Cell };

impl Evaluator {
  pub fn parse_call_cc(&self, node: SchemeExp, env: Env) -> () {
    ()
  }

  pub fn eval_call_cc(&mut self) -> Option<Cell> {
    None
  }
}