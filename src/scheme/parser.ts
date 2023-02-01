/**
 * parser 接受一个 st 字符串，然后返回一个 ast 结构
 * 注意：为了更好地扩展性，这里不涉及任何 let、define 等语法，这些语法在后面处理
 */

enum TokenType {
  Quote,
  LParen,
  RParen,
  Symbol,
  Number,
  String,
  WhiteSpace,
  Boolean,
  Comment,
  EOF,
}

const regexList: [TokenType, RegExp][] = [
  [TokenType.Quote, /^'/],
  [TokenType.LParen, /^\(/],
  [TokenType.RParen, /^\)/],
  [TokenType.Symbol, /^[^\s()',]+/],
  [TokenType.Number, /^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?/],
  [TokenType.String, /^"([^\\"]+|\\.)*"/],
  [TokenType.WhiteSpace, /^\s*/],
  [TokenType.Boolean, /^#t|^#f/],
  [TokenType.Comment, /^;.*/]
]

export class Token {
  constructor(
    private type: TokenType,
    private start: number,
    private end: number,
  ) { }
}

export function parse(st: string) {
  const stack = []
  let index = 0
  return parseExpression()

  function getNextToken() {
    for (const [token, reg] of regexList) {
      const matched = reg.exec(st)

      if (matched) {
        return {
          token,
          len: matched[0].length
        }
      }
    }

    return {
      token: TokenType.EOF,
      len: 0
    }
  }

  function advance(n: number): void {
    index += n
    st = st.substring(n)
  }

  function parseExpression() {
    const { token, len } = getNextToken()
    stack.push(new Token(token, index, index + len))
    advance(len)
  }
}
