#[derive(Debug, PartialEq)]
pub enum TokenType {
  Quote,
  LParen,
  RParen,
  Number(i64),
  String(String),
  Boolean(bool),
  WhiteSpace,
  Comment(String),
  Identifier(String),
  EOL,
}

#[derive(Debug, PartialEq)]
pub struct Location {
  lineStart: usize,
  columnStart: usize,
  lineEnd: usize,
  columnEnd: usize,
}

#[derive(Debug, PartialEq)]
pub struct TokenItem {
  token: TokenType,
  loc: Location,
}

#[derive(Debug)]
pub struct TokenError {
  msg: String,
}

pub fn tokenize(program: &str) -> Result<Vec<TokenItem>, TokenError> {
  let mut cursor: usize = 0;
  let mut line: usize = 1;
  let mut column: usize = 1;
  let mut tokens = vec![];
  let mut char_list = program.chars().collect::<Vec<char>>();
  let len = char_list.len();

  fn get_content(
    list: &mut Vec<char>,
    mut start: usize,
    stop_check_fn: fn(char) -> bool,
  ) -> String {
    let mut content = String::new();
    while start < list.len() {
      let peek = list.get(start).unwrap_or(&' ');
      if stop_check_fn(*peek) {
        break;
      }
      content.push(*peek);
      start += 1;
    }
    content
  }

  while cursor < len {
    let ch = char_list.get(cursor).unwrap_or(&' ');
    let (n, token) = match ch {
      '\n' => (1, TokenType::EOL),
      '\'' => (1, TokenType::Quote),
      '(' => (1, TokenType::LParen),
      ')' => (1, TokenType::RParen),
      '#' => {
        let next = char_list.get(cursor + 1).unwrap_or(&' ');
        (
          2,
          TokenType::Boolean(if *next == 't' { true } else { false }),
        )
      }
      ';' => {
        let content = get_content(&mut char_list, cursor + 1, |peek| peek == '\n');
        (1 + content.len(), TokenType::Comment(content))
      }
      '"' => {
        // don't support newline in TokenType::String
        let content = get_content(&mut char_list, cursor + 1, |peek| {
          peek == '"' || peek == '\n'
        });
        (2 + content.len(), TokenType::String(content))
      }
      _ => {
        if ch.is_whitespace() {
          let content = get_content(&mut char_list, cursor, |peek| !peek.is_whitespace());
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
          });

          (
            content.len(),
            if let Ok(v) = content.parse::<i64>() {
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
        lineStart: line,
        columnStart: column,
        lineEnd: line,
        columnEnd: column + n,
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
            lineStart: 1,
            columnStart: 1,
            lineEnd: 1,
            columnEnd: 2
          }
        },
        TokenItem {
          token: TokenType::Identifier(String::from("+")),
          loc: Location {
            lineStart: 1,
            columnStart: 2,
            lineEnd: 1,
            columnEnd: 3
          }
        },
        TokenItem {
          token: TokenType::WhiteSpace,
          loc: Location {
            lineStart: 1,
            columnStart: 3,
            lineEnd: 1,
            columnEnd: 4
          }
        },
        TokenItem {
          token: TokenType::Number(1 as i64),
          loc: Location {
            lineStart: 1,
            columnStart: 4,
            lineEnd: 1,
            columnEnd: 5
          }
        },
        TokenItem {
          token: TokenType::WhiteSpace,
          loc: Location {
            lineStart: 1,
            columnStart: 5,
            lineEnd: 1,
            columnEnd: 6
          }
        },
        TokenItem {
          token: TokenType::Number(2 as i64),
          loc: Location {
            lineStart: 1,
            columnStart: 6,
            lineEnd: 1,
            columnEnd: 7
          }
        },
        TokenItem {
          token: TokenType::RParen,
          loc: Location {
            lineStart: 1,
            columnStart: 7,
            lineEnd: 1,
            columnEnd: 8
          }
        },
      ]
    );
  }
}
