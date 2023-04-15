#[derive(Debug)]
pub enum TokenType {
  Quote(String),
  LParen,
  RParen,
  Number(i32),
  String(String),
  Boolean(bool),
  WhiteSpace,
  Comment(String),
  Symbol(String),
  EOL,
}

#[derive(Debug)]
pub struct Location {
  lineStart: u32,
  columnStart: u32,
  lineEnd: u32,
  columnEnd: u32,
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
  let mut cursor = 0;
  let mut line = 1;
  let mut column = 1;
  let mut tokens = Vec::new();
  let char_list = program.chars().collect::<Vec<char>>();
  let len = char_list.len();

  fn forward(token: TokenType, n: i32) -> () {
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

    if (token == TokenType::EOL) {
      line += 1;
      column = 0;
    }
  }

  fn get_content(start: i32, stop_check_fn: fn(char) -> bool) -> String {
    let mut content = String::new();
    while start < len {
      let mut peek = char_list.get(start);
      if stop_check_fn(peek) {
        break;
      }
      content.push(peek);
      start += 1;
    }
    content
  }

  while cursor < len {
    let ch = char_list.get(cursor);
    match ch {
      '\n' => forward(TokenType::EOL, 1),
      '\'' => forward(TokenType::Quote, 1),
      '(' => forward(TokenType::LParen, 1),
      ')' => forward(TokenType::RParen, 1),
      '#' => {
        let next = char_list.get(cursor + 1);
        forward(
          TokenType::Boolean(if next == 't' { true } else { false }),
          2,
        );
      }
      ';' => {
        let content = get_content(cursor + 1, |peek| peek == '\n');
        forward(TokenType::Comment, 1 + content.len());
      }
      '"' => {
        // don't support newline in TokenType::String
        let content = get_content(cursor + 1, |peek| peek == '"' || peek == '\n');
        forward(TokenType::String, 2 + content.len());
      }
      _ => {
        if ch.is_whitespace() {
          let content = get_content(cursor, |peek| !peek.is_whitespace());
          forward(TokenType::WhiteSpace, content.len());
        } else {
          let content = get_content(cursor, |peek| {
            peek.is_whitespace()
              || peek == '\''
              || peek == '('
              || peek == ')'
              || peek == '#'
              || peek == ';'
              || peek == '"'
          });

          forward(
            if let Ok(v) = content.parse::<f64>() {
              TokenType::Number(v)
            } else {
              TokenType::Symbol(content)
            },
            content.len(),
          );
        }
      }
    }
  }

  tokens
}
