/**
 * parser 接受一个 st 字符串，然后返回一个 ast 结构
 * 注意：为了更好地扩展性，这里不涉及任何 let、define 等语法，这些语法在后面处理
 */

import { TokenType, NodeAtom, NodeContainer } from "./node"

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

interface TokenItem {
  token: TokenType
  value: string
  len: number
}

export function parse(st: string) {
  let index = 0
  return parseExpression()

  function getNextToken(): TokenItem {
    for (const [token, reg] of regexList) {
      const matched = reg.exec(st)
      const value = matched[0]

      if (matched) {
        return {
          token,
          value,
          len: value.length
        }
      }
    }

    return {
      token: TokenType.EOF,
      value: null,
      len: 0
    }
  }

  function advance(n: number): void {
    index += n
    st = st.substring(n)
  }

  function parseExpression(): NodeAtom | NodeContainer {
    const { token, value, len } = getNextToken()
    let res = null

    switch (token) {
      case TokenType.WhiteSpace:
      case TokenType.Comment:
        return parseExpression();
      case TokenType.LParen:
        return parseParen();
      default:
        const start = index
        advance(len)
        return new NodeAtom(token, value, start, start + len);
    }
  }

  function parseParen(): NodeContainer {

  }
}
