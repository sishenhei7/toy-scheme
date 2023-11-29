use core::panic;
use std::collections::VecDeque;

use crate::{parser::{SchemeExp, SchemeData, SchemeBoolean}, env::Env};

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

  pub fn evaluate_buildin_base_one(
    &mut self,
    mut node: SchemeExp,
    env: Env,
    next: usize,
    func: fn(SchemeData) -> SchemeData
  ) -> usize {
    let first_node = node.value.pop_front().expect("Evaluate buildin-is_null error!");
    let finally_cid = self.insert_map(Unit::new(
      env.copy(),
      None,
      Box::new(move |x, _| (next, func(x)))
    ));
    self.evaluate(first_node, env.copy(), finally_cid)
  }

  pub fn evaluate_buildin_base_two(
    &mut self,
    mut node: SchemeExp,
    env: Env,
    next: usize,
    func: fn(SchemeData, SchemeData) -> SchemeData
  ) -> usize {
    let finally_cid = self.insert_map(Unit::new(
      env.copy(),
      None,
      Box::new(move |_, mut e: Evaluator| {
        let mut last_stack = e.pop_stack();
        let second_value = last_stack.pop().expect("Evaluate buildin-cons-second error!");
        let first_value = last_stack.pop().expect("Evaluate buildin-cons-first error!");
        (next, func(first_value, second_value))
      })
    ));

    let first_node = node.value.pop_front().expect("Evaluate buildin-cons-first error!");
    let second_node = node.value.pop_front().expect("Evaluate buildin-cons-second error!");
    let second_cid = self.evaluate(second_node, env.copy(), finally_cid);
    let first_cid = self.evaluate(first_node, env.copy(), second_cid);

    self.insert_map(Unit::new(
      env.copy(),
      None,
      Box::new(move |_, mut e| {
        e.add_stack();
        (first_cid, SchemeData::Nil)
      })
    ))
  }

  // 只能 cons 两个
  // TODO: 下面的 node 加 mut 前缀和不加有什么区别？为什么不需要加前缀？
  pub fn evaluate_buildin_cons(&mut self, node: SchemeExp, env: Env, next: usize) -> usize {
    self.evaluate_buildin_base_two(
      node,
      env,
      next,
      |x, y| SchemeData::cons(&x, &y)
    )
  }

  pub fn evaluate_buildin_is_null(&mut self, node: SchemeExp, env: Env, next: usize) -> usize {
    self.evaluate_buildin_base_one(node, env, next, |x| SchemeData::Boolean(SchemeBoolean{
      value: x.clone().is_nil(),
      loc: x.get_loc()
    }))
  }

  pub fn evaluate_buildin_car(&mut self, node: SchemeExp, env: Env, next: usize) -> usize {
    self.evaluate_buildin_base_one(node, env, next, |mut x| {
      let car = x.get_list_car().expect("Evaluate buildin-car error!");
      *car.clone()
    })
  }

  pub fn evaluate_buildin_cdr(&mut self, node: SchemeExp, env: Env, next: usize) -> usize {
    self.evaluate_buildin_base_one(node, env, next, |mut x| {
      let cdr = x.get_list_cdr().expect("Evaluate buildin-car error!");
      *cdr.clone()
    })
  }

  pub fn evaluate_buildin_cadr(&mut self, node: SchemeExp, env: Env, next: usize) -> usize {
    self.evaluate_buildin_base_one(node, env, next, |mut x| {
      let mut cdr = x.get_list_cdr().expect("Evaluate buildin-cadr error!");
      let cadr = cdr.get_list_car().expect("Evaluate buildin-cadr error!");
      *cadr.clone()
    })
  }

  pub fn evaluate_buildin_is_equal(&mut self, node: SchemeExp, env: Env, next: usize) -> usize {
    self.evaluate_buildin_base_two(
      node,
      env,
      next,
      |mut x, mut y| {
        let first_value = x.get_number().expect("Evaluate buildin-is-equal error!");
        let second_value = y.get_number().expect("Evaluate buildin-is-equal error!");
        SchemeData::Boolean(SchemeBoolean{
          value: first_value == second_value,
          loc: None
        })
      }
    )
  }

  pub fn evaluate_buildin_is_more_than(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }

  pub fn evaluate_buildin_is_less_than(&mut self, mut node: SchemeExp, env: Env, next: usize) -> usize {
    self.cid
  }

  // TODO: 把所有的 Unit::new 的 loc 写上！
  pub fn evaluate_buildin_add(&mut self, node: SchemeExp, env: Env, next: usize) -> usize {
    let cons_cid = self.insert_map(Unit::new(
      env.copy(),
      None,
      Box::new(move |_, mut e| {
        let mut last_stack = e.pop_stack();
        let value = last_stack.iter_mut().reduce(|acc, mut cur| {
          acc.add(cur);
          acc
        }).expect("Evaluate add error!");
        (next, value.clone())
      })
    ));

    let value_cid = node.value.into_iter().rev().fold(cons_cid, |acc, cur| {
      let begin_cid = self.insert_map(Unit::new(
        env.copy(),
        None,
        Box::new(move |v, mut e| {
          e.push_stack(v.clone());
          (acc, SchemeData::Nil)
        })
      ));

      self.evaluate(cur, env.copy(), begin_cid)
    });

    self.insert_map(Unit::new(
      env.copy(),
      None,
      Box::new(move |_, mut e| {
        e.add_stack();
        (value_cid, SchemeData::Nil)
      })
    ))
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