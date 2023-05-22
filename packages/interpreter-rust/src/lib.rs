pub mod closure;
pub mod env;
pub mod evaluator;
pub mod lexer;
pub mod parser;

// use napi_derive::napi;
use evaluator::*;
use lexer::*;
use parser::*;

// #[napi(custom_finalize)]
pub struct Interpreter {
  node: SchemeData,
}

pub struct InterpreterError;

impl Interpreter {
  // #[napi(constructor)]
  pub fn new(program: String) -> Result<Self, InterpreterError> {
    // const tokenList = tokenize(st).filter((token) => !token.isIgnoreToken())
    // const tokenList = parse(program)
    // const evaluator = new Evaluator(options)
    // this.node = evaluator.evaluateList(tokenList, new Env())
    let token_list = tokenize(&program).or(Err(InterpreterError))?;
    let scheme_exp = parse(token_list).or(Err(InterpreterError))?;
    let evaluator = Evaluator::new();
    // Ok(Interpreter { node: scheme_exp })
  }
}
