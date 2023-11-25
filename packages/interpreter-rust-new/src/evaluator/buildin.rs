use core::panic;
use std::collections::VecDeque;

use crate::{parser::{SchemeExp, SchemeData}, env::Env};

use super::{ Evaluator, Unit };

impl Evaluator {
  pub fn evaluate_buildin(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    let identifier_node = node.value.pop_front().expect("Evaluate buildin-symbol error!");
    let identifier = identifier_node.get_identifier_string().expect("Evaluate buildin-symbol error!");
    match identifier.as_ref() {
      "cons" => self.evaluate_buildin_cons(node, env, next),
      "null?" => self.evaluate_buildin_is_null(node, env, next),
      "car" => self.evaluate_buildin_car(node, env, next),
      "cdr" => self.evaluate_buildin_cdr(node, env, next),
      "cadr" => self.evaluate_buildin_cadr(node, env, next),
      "=" => self.evaluate_buildin_is_equal(node, env, next),
      ">" => self.evaluate_buildin_is_more_than(node, env, next),
      "<" => self.evaluate_buildin_is_less_than(node, env, next),
      "+" => self.evaluate_buildin_add(node, env, next),
      "-" => self.evaluate_buildin_minus(node, env, next),
      "*" => self.evaluate_buildin_multiply(node, env, next),
      "/" => self.evaluate_buildin_divide(node, env, next),
      "min" => self.evaluate_buildin_min(node, env, next),
      "max" => self.evaluate_buildin_max(node, env, next),
      "abs" => self.evaluate_buildin_abs(node, env, next),
      "zero?" => self.evaluate_buildin_is_zero(node, env, next),
      "length" => self.evaluate_buildin_length(node, env, next),
      "not" => self.evaluate_buildin_not(node, env, next),
      "and" => self.evaluate_buildin_and(node, env, next),
      "or" => self.evaluate_buildin_or(node, env, next),
      _ => panic!("Evaluate buildin error!")
    }
  }

  // 已经解决了(+ x xx xxx xxx)无限多个的情况
  // 但是(+ x (* y yy yyy) xx xxx)的情况又怎么解决呢？(TODO:fix it)
  // 初步的想法是，在 evaluator 上面加一个 stack 来保存
  pub fn evaluate_buildin_cons(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    // let first_node = node.value.pop_front().expect("Evaluate buildin-cons error!");
    // let second_node = node.value.pop_front().expect("Evaluate buildin-cons error!");

    // let cons_cid = self.insert_map(Unit::new(
    //   env.copy(),
    //   None,
    //   Box::new(move |x| {
    //     // evaluator.evaluate(item, env, current_next)
    //     (next, SchemeData::cons())
    //   })
    // ));

    // let second_cid = self.evaluate(second_node, env, cons_cid);
    // self.evaluate(first_node, env, second_cid)

    // let mut list = VecDeque::new();
    // let cons_cid = self.insert_map(Unit::new(
    //   env.copy(),
    //   None,
    //   Box::new(move |mut v| {
    //     // evaluator.evaluate(item, env, current_next)
    //     v.push(SchemeData::cons_from_vec(&mut list));
    //     (next, v)
    //   })
    // ));
    // node.value.into_iter().rev().fold(cons_cid, |acc, cur| {
    //   let begin_cid = self.insert_map(Unit::new(
    //     env.copy(),
    //     None,
    //     Box::new(move |mut v| {
    //       // 丢弃之前的返回，只返回自己
    //       // let res = if v.is_empty() { v } else { vec![v.pop().expect("Evaluate Begin-value error!")] };
    //       list.push_back(v.pop().expect("xxxx"));
    //       (acc, v)
    //     })
    //   ));

    //   self.evaluate(cur, env.copy(), begin_cid)
    // })

    self.cid
  }

  pub fn evaluate_buildin_is_null(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }

  pub fn evaluate_buildin_car(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }

  pub fn evaluate_buildin_cdr(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }

  pub fn evaluate_buildin_cadr(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }

  pub fn evaluate_buildin_is_equal(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }

  pub fn evaluate_buildin_is_more_than(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }

  pub fn evaluate_buildin_is_less_than(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }

  pub fn evaluate_buildin_add(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }

  pub fn evaluate_buildin_minus(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }

  pub fn evaluate_buildin_multiply(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }

  pub fn evaluate_buildin_divide(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }

  pub fn evaluate_buildin_min(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }

  pub fn evaluate_buildin_max(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }

  pub fn evaluate_buildin_abs(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }

  pub fn evaluate_buildin_is_zero(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }

  pub fn evaluate_buildin_length(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }

  pub fn evaluate_buildin_not(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }

  pub fn evaluate_buildin_and(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }

  pub fn evaluate_buildin_or(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }
}