/**
 * 数据结构：
 * 占位: 表达式、符号
 * 简单：number、string、boolean、quote
 * 复杂：todo
 */
import { type TokenItem, TokenType } from './token'

export class ILocation {
  start: number = 0
  end: number = 0

  public setLocationInfo(start: number, end: number): this {
    this.start = start
    this.end = end
    return this
  }
}

export class BaseData extends ILocation {
  public next: BaseData | null = null
}

/**
 * 占位数据结构：表达式
 */
export class SchemeExp extends BaseData {
  constructor(public body: BaseData | null) {
    super()
  }
}

/**
 * 占位数据结构：符号
 */
export class SchemeSym extends BaseData {
  constructor(public body: string) {
    super()
  }
}

/**
 * 基础数据结构：number
 */
export class SchemeNumber extends BaseData {
  constructor(public value: number) {
    super()
  }
}

/**
 * 基础数据结构：string
 */
export class SchemeString extends BaseData {
  constructor(public value: string) {
    super()
  }
}

/**
 * 基础数据结构：boolean
 */
export class SchemeBoolean extends BaseData {
  constructor(public value: boolean) {
    super()
  }
}

/**
 * 基础数据结构：quote
 */
export class SchemeQuote extends BaseData {
  constructor(public value: string) {
    super()
  }
}

/**
 * 把一段 token 数组解析成 data 单链表
 */
export function getParser(tokenList: TokenItem[]): () => BaseData | null {
  let tokenCursor = 0
  const rParenEndList: number[] = []

  function parseTokenList(): BaseData | null {
    let last: BaseData | null = null
    let first: BaseData | null = null
    let shouldEnd = false

    while (!shouldEnd && tokenCursor < tokenList.length) {
      const { type, value, start, end } = tokenList[tokenCursor++]
      let currentData = null

      switch (type) {
        case TokenType.Boolean:
          currentData = new SchemeBoolean(value === '#f').setLocationInfo(start, end)
          break
        case TokenType.Number:
          currentData = new SchemeNumber(Number(value)).setLocationInfo(start, end)
          break
        case TokenType.String:
          currentData = new SchemeString(value).setLocationInfo(start, end)
          break
        case TokenType.Quote:
          currentData = new SchemeQuote(value).setLocationInfo(start, end)
          break
        case TokenType.Symbol:
          currentData = new SchemeSym(value).setLocationInfo(start, end)
          break
        case TokenType.LParen:
          currentData = new SchemeExp(parseTokenList())

          // 使用上次右括号的 end 来更新此节点的 end
          if (rParenEndList.length === 0) {
            throw Error('Parsing error: too many left paren')
          }
          currentData.setLocationInfo(start, rParenEndList.pop()!)

          break
        case TokenType.RParen:
          // 右括号不新建节点，储存 end，并终止此次循环
          rParenEndList.push(end)
          shouldEnd = true
          break
        default:
          break
      }

      !first && (first = currentData)
      last && (last.next = currentData)
      last = currentData
    }

    return first
  }

  return parseTokenList
}
