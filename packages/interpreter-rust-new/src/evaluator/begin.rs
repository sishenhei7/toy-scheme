use crate::{parser::SchemeExp, env::Env};

use super::{ Evaluator, Cell };

impl Evaluator {
  pub fn parse_begin(&self, node: SchemeExp, env: Env) -> Option<Cell> {
    None
  }

  pub fn eval_begin(&mut self) -> Option<Cell> {
    None
  }
}