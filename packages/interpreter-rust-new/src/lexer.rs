#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
  Quote,
  LParen,
  RParen,
  Number(f64),
  String(String),
  Boolean(bool),
  WhiteSpace,
  Comment(String),
  Identifier(String),
  EOL,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Location {
  pub line_start: usize,
  pub column_start: usize,
  pub line_end: usize,
  pub column_end: usize,
}

impl Default for Location {
  fn default() -> Self {
    Self {
      line_start: 1,
      column_start: 1,
      line_end: 1,
      column_end: 1,
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TokenItem {
  pub token: TokenType,
  pub loc: Location,
}

#[derive(Debug)]
pub struct Lexer<'a> {
  iter: std::str::Chars<'a>,
  cur_line: usize,
  cur_column: usize,
}

impl<'a> Lexer<'a> {
  pub fn new(input: &'a str) -> Self {
    Lexer {
      iter: input.chars(),
      cur_line: 1,
      cur_column: 1,
    }
  }

  pub fn from(input: &'a str) -> Self {
    Lexer::new(input)
  }

  fn cur(&self) -> Option<char> {
    self.iter.clone().next()
  }

  fn peek(&self) -> Option<char> {
    self.iter.clone().nth(1)
  }

  fn advance(&mut self, column: usize, line: usize) -> () {
    self.iter.advance_by(column).unwrap_or(());
    self.cur_line += line;
    self.cur_column += column;
  }

  fn get_content<F>(&self, predict: F) -> Option<String>
  where
    F: Fn(&char) -> bool
  {
    let content = self
      .iter
      .clone()
      .take_while(|x| predict(x))
      .collect::<String>();
    Some(content).filter(|x| !x.is_empty())
  }

  fn to_token(&self, token_type: TokenType, offset: usize) -> TokenItem {
    TokenItem {
      token: token_type,
      loc: Location {
        line_start: self.cur_line,
        column_start: self.cur_column,
        line_end: self.cur_line,
        column_end: self.cur_column + offset,
      },
    }
  }

  fn read_eol(&mut self) -> Option<TokenItem> {
    let token = self.to_token(TokenType::EOL, 1);
    self.advance(1, 1);
    Some(token)
  }

  fn read_quote(&mut self) -> Option<TokenItem> {
    let token = self.to_token(TokenType::Quote, 1);
    self.advance(1, 0);
    Some(token)
  }

  fn read_paren(&mut self) -> Option<TokenItem> {
    let token_type = match self.cur() {
      Some('(') => TokenType::LParen,
      Some(')') => TokenType::RParen,
      _ => return None,
    };
    let token = self.to_token(token_type, 1);
    self.advance(1, 0);
    Some(token)
  }

  fn read_bool(&mut self) -> Option<TokenItem> {
    let token_type = match self.peek() {
      Some('t') => TokenType::Boolean(true),
      Some('f') => TokenType::Boolean(false),
      _ => return None
    };
    let token = self.to_token(token_type, 2);
    self.advance(2, 0);
    Some(token)
  }

  fn read_comment(&mut self) -> Option<TokenItem> {
    let content = self
      .get_content(|&c| c != '\n')
      .unwrap_or(";".to_string());
    let offset = content.len();
    let str = &content[1..offset];
    let token = self.to_token(TokenType::Comment(str.to_string()), offset);
    self.advance(offset, 0);
    Some(token)
  }

  fn read_string(&mut self) -> Option<TokenItem> {
    let content = self
      .get_content(|&c| c != '"' && c != '\n')
      .unwrap_or("\"\"".to_string());
    let offset = content.len();
    let str = &content[1..offset - 1];
    let token = self.to_token(TokenType::String(str.to_string()), offset);
    self.advance(offset, 0);
    Some(token)
  }

  fn read_whitespace(&mut self) -> Option<TokenItem> {
    let content = self
      .get_content(|&c| c.is_whitespace())
      .unwrap_or("".to_string());
    let offset = content.len();
    let token = self.to_token(TokenType::WhiteSpace, offset);
    self.advance(offset, 0);
    Some(token)
  }

  fn read_other(&mut self) -> Option<TokenItem> {
    let content = self
      .get_content(|&c| {
        !c.is_whitespace()
          && c != '\''
          && c != '('
          && c != ')'
          && c != '#'
          && c != ';'
          && c != '"'
      })
      .unwrap_or("".to_string());
    let offset = content.len();
    let token = if let Ok(v) = content.parse::<f64>() {
      self.to_token(TokenType::Number(v), offset)
    } else {
      self.to_token(TokenType::Identifier(content), offset)
    };
    self.advance(offset, 0);
    Some(token)
  }
}

impl<'a> Iterator for Lexer<'a> {
  type Item = TokenItem;

  fn next(&mut self) -> Option<Self::Item> {
    let ch = match self.cur() {
      Some(x) => x,
      None => return None,
    };

    match ch {
      '\n' => self.read_eol(),
      '\'' => self.read_quote(),
      '(' | ')' => self.read_paren(),
      '#' => self.read_bool(),
      ';' => self.read_comment(),
      '"' => self.read_string(),
      _ => {
        if ch.is_whitespace() {
          self.read_whitespace()
        } else {
          self.read_other()
        }
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_lexer() {
    let tokens = Lexer::new("(+ 1 2)").collect::<Vec<TokenItem>>();
    assert_eq!(
      tokens,
      vec![
        TokenItem {
          token: TokenType::LParen,
          loc: Location {
            line_start: 1,
            column_start: 1,
            line_end: 1,
            column_end: 2
          }
        },
        TokenItem {
          token: TokenType::Identifier("+".to_string()),
          loc: Location {
            line_start: 1,
            column_start: 2,
            line_end: 1,
            column_end: 3
          }
        },
        TokenItem {
          token: TokenType::WhiteSpace,
          loc: Location {
            line_start: 1,
            column_start: 3,
            line_end: 1,
            column_end: 4
          }
        },
        TokenItem {
          token: TokenType::Number(1 as f64),
          loc: Location {
            line_start: 1,
            column_start: 4,
            line_end: 1,
            column_end: 5
          }
        },
        TokenItem {
          token: TokenType::WhiteSpace,
          loc: Location {
            line_start: 1,
            column_start: 5,
            line_end: 1,
            column_end: 6
          }
        },
        TokenItem {
          token: TokenType::Number(2 as f64),
          loc: Location {
            line_start: 1,
            column_start: 6,
            line_end: 1,
            column_end: 7
          }
        },
        TokenItem {
          token: TokenType::RParen,
          loc: Location {
            line_start: 1,
            column_start: 7,
            line_end: 1,
            column_end: 8
          }
        },
      ]
    );
  }
}
