pub mod env;
pub mod lexer;
pub mod parser;
pub mod closure;
pub mod evaluator;

use napi_derive::napi;

#[napi]
pub fn fibonacci(n: u32) -> u32 {
  match n {
    1 | 2 => 1,
    _ => fibonacci(n - 1) + fibonacci(n - 2),
  }
}
