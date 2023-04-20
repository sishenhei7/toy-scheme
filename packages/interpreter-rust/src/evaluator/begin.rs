use std::rc::Rc;

use crate::evaluator::{Evaluator, IEvaluator};

pub struct BeginEvaluator<'a> {
  base_evaluator: &'a Evaluator,
}

impl BeginEvaluator<'_> {
  // TODO: how to return Self ?
  pub fn new<'a>(base_evaluator: &'a Evaluator) -> BeginEvaluator<'a> {
    BeginEvaluator { base_evaluator }
  }
}

impl IEvaluator for BeginEvaluator<'_> {}
