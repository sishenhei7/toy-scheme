#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Location {
  pub line_start: usize,
  pub column_start: usize,
  pub line_end: usize,
  pub column_end: usize,
}

#[derive(Debug, PartialEq)]
pub struct TokenItem {
  pub token: TokenType,
  pub loc: Location,
}

pub struct TokenError {
  msg: String,
}

pub fn tokenize(program: &str) -> Result<Vec<TokenItem>, TokenError> {
  let mut cursor = 0;
  let mut line = 1;
  let mut column = 1;
  let mut tokens = vec![];
  let mut char_list = program.chars().collect::<Vec<char>>();
  let len = char_list.len();

  fn get_content(
    list: &mut Vec<char>,
    mut start: usize,
    stop_check_fn: fn(char) -> bool,
  ) -> Result<String, TokenError> {
    let mut content = String::new();
    while start < list.len() {
      let peek = list.get(start).ok_or(TokenError { msg: String::from("Token error!") })?;
      if stop_check_fn(*peek) {
        break;
      }
      content.push(*peek);
      start += 1;
    }
    Ok(content)
  }

  while cursor < len {
    let ch = char_list.get(cursor).ok_or(TokenError { msg: String::from("Token error!") })?;
    let (n, token) = match ch {
      '\n' => (1, TokenType::EOL),
      '\'' => (1, TokenType::Quote),
      '(' => (1, TokenType::LParen),
      ')' => (1, TokenType::RParen),
      '#' => {
        let next = char_list.get(cursor + 1).ok_or(TokenError { msg: String::from("Token error: boolean!") })?;
        (
          2,
          TokenType::Boolean(if *next == 't' { true } else { false }),
        )
      }
      ';' => {
        let content = get_content(&mut char_list, cursor + 1, |peek| peek == '\n')?;
        (1 + content.len(), TokenType::Comment(content))
      }
      '"' => {
        // don't support newline in TokenType::String
        let content = get_content(&mut char_list, cursor + 1, |peek| {
          peek == '"' || peek == '\n'
        })?;
        (2 + content.len(), TokenType::String(content))
      }
      _ => {
        if ch.is_whitespace() {
          let content = get_content(&mut char_list, cursor, |peek| !peek.is_whitespace())?;
          (content.len(), TokenType::WhiteSpace)
        } else {
          let content = get_content(&mut char_list, cursor, |peek| {
            peek.is_whitespace()
              || peek == '\''
              || peek == '('
              || peek == ')'
              || peek == '#'
              || peek == ';'
              || peek == '"'
          })?;

          (
            content.len(),
            if let Ok(v) = content.parse::<f64>() {
              TokenType::Number(v)
            } else {
              TokenType::Identifier(content)
            },
          )
        }
      }
    };

    // because token is moved
    let is_new_line = token == TokenType::EOL;

    tokens.push(TokenItem {
      token,
      loc: Location {
        line_start: line,
        column_start: column,
        line_end: line,
        column_end: column + n,
      },
    });

    cursor += n;
    column += n;

    if is_new_line {
      line += 1;
      column = 0;
    }
  }

  Ok(tokens)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_add() {
    let tokens = tokenize("(+ 1 2)").unwrap_or(vec![]);
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
          token: TokenType::Identifier(String::from("+")),
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
