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

#[derive(Debug)]
pub struct Location {
  lineStart: usize,
  columnStart: usize,
  lineEnd: usize,
  columnEnd: usize,
}

#[derive(Debug)]
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
  let mut tokens = Vec::new();
  let char_list = program.chars().collect::<Vec<char>>();
  let len = char_list.len();

  fn get_content(list: Vec<char>, start: usize, stop_check_fn: fn(char) -> bool) -> String {
    let mut content = String::new();
    while start < list.len() {
      let mut peek = list.get(start).unwrap_or(&' ');
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
    let (token, n) = match ch {
      '\n' => (TokenType::EOL, 1),
      '\'' => (TokenType::Quote, 1),
      '(' => (TokenType::LParen, 1),
      ')' => (TokenType::RParen, 1),
      '#' => {
        let next = char_list.get(cursor + 1).unwrap_or(&' ');
        (
          TokenType::Boolean(if *next == 't' { true } else { false }),
          2,
        )
      }
      ';' => {
        let content = get_content(char_list, cursor + 1, |peek| peek == '\n');
        (TokenType::Comment(content), 1 + content.len())
      }
      '"' => {
        // don't support newline in TokenType::String
        let content = get_content(char_list, cursor + 1, |peek| peek == '"' || peek == '\n');
        (TokenType::String(content), 2 + content.len())
      }
      _ => {
        if ch.is_whitespace() {
          let content = get_content(char_list, cursor, |peek| !peek.is_whitespace());
          (TokenType::WhiteSpace, content.len())
        } else {
          let content = get_content(char_list, cursor, |peek| {
            peek.is_whitespace()
              || peek == '\''
              || peek == '('
              || peek == ')'
              || peek == '#'
              || peek == ';'
              || peek == '"'
          });

          (
            if let Ok(v) = content.parse::<i64>() {
              TokenType::Number(v)
            } else {
              TokenType::Identifier(content)
            },
            content.len(),
          )
        }
      }
    };

    tokens.push(TokenItem {
      token,
      loc: Location {
        lineStart: line,
        columnStart: column,
        lineEnd: line,
        columnEnd: column + 2,
      },
    });

    cursor += n;
    column += n;

    if token == TokenType::EOL {
      line += 1;
      column = 0;
    }
  }

  Ok(tokens)
}
