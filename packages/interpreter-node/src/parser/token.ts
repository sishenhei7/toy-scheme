import { ILocation } from './data'
import { assert } from '../utils'

export enum TokenType {
  Quote,
  LParen,
  RParen,
  Number,
  String,
  Boolean,
  WhiteSpace,
  Comment,
  Symbol
}

export const tokenRegexList: [TokenType, RegExp][] = [
  [TokenType.Quote, /^'\(?/],
  [TokenType.LParen, /^\(/],
  [TokenType.RParen, /^\)/],
  [TokenType.Number, /^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?/],
  [TokenType.String, /^"([^\\"]+|\\.)*"/],
  [TokenType.Boolean, /^#t|^#f/],
  [TokenType.WhiteSpace, /^\s+/],
  [TokenType.Comment, /^;.*/],
  [TokenType.Symbol, /^[^\s()',]+/]
]

export class TokenItem extends ILocation {
  constructor(public type: TokenType, public value: string, public len: number) {
    super()
  }

  public isOpenToken(): boolean {
    return [TokenType.LParen].includes(this.type)
  }

  public isCloseToken(): boolean {
    return [TokenType.RParen].includes(this.type)
  }

  public isIgnoreToken(): boolean {
    return [TokenType.WhiteSpace, TokenType.Comment].includes(this.type)
  }

  public isExpressionToken(token: TokenType): boolean {
    return [TokenType.LParen, TokenType.RParen].includes(token)
  }
}

/**
 * 把一段 string 解析成一个 token 数组
 */
export default function tokenize(st: string): TokenItem[] {
  let cursor = 0
  let line = 1
  let column = 1
  const stack = []

  function forward(n: number, isNewLine: boolean = false) {
    cursor += n
    st = st.substring(n)
    line += isNewLine ? 1 : 0
    column = isNewLine ? 1 : column + n
  }

  while (st) {
    if (st.startsWith('\n')) {
      forward(1, true)
      continue
    }

    let isMatched = false

    for (const [tokenType, reg] of tokenRegexList) {
      const matched = reg.exec(st)

      if (matched) {
        const value = matched?.[0]
        const len = value.length
        stack.push(
          new TokenItem(tokenType, value, len).setLocationInfo({
            lineStart: line,
            columnStart: column,
            lineEnd: line,
            columnEnd: column + len
          })
        )

        // advance
        forward(len)
        isMatched = true
        break
      }
    }

    assert(isMatched, `Token parsing failed: ${st}`)
  }

  return stack
}
