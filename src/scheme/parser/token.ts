import { ILocation } from './data'

export enum TokenType {
  Quote,
  LParen,
  RParen,
  Symbol,
  Number,
  String,
  WhiteSpace,
  Boolean,
  Comment
}

export const tokenRegexList: [TokenType, RegExp][] = [
  [TokenType.Quote, /^'\S+/],
  [TokenType.LParen, /^\(/],
  [TokenType.RParen, /^\)/],
  [TokenType.Symbol, /^[^\s()',]+/],
  [TokenType.Number, /^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?/],
  [TokenType.String, /^"([^\\"]+|\\.)*"/],
  [TokenType.WhiteSpace, /^\s+/],
  [TokenType.Boolean, /^#t|^#f/],
  [TokenType.Comment, /^;.*/]
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
export function tokenizer(st: string): TokenItem[] {
  let cursor = 0
  const stack = []

  while (st) {
    let isMatched = false

    for (const [tokenType, reg] of tokenRegexList) {
      const matched = reg.exec(st)

      if (matched) {
        const value = matched?.[0]
        const len = value.length
        stack.push(new TokenItem(tokenType, value, len).setLocationInfo(cursor, cursor + len))

        // advance
        cursor += len
        st = st.substring(len)
        isMatched = true
      }
    }

    if (!isMatched) {
      throw Error(`Token parsing failed: ${st}`)
    }
  }

  return stack
}
