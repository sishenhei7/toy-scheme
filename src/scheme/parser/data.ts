/**
 * 数据结构：
 * 占位: 表达式、符号
 * 简单：number、string、boolean、quote、nil
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

  static isNil(item: BaseData): item is SchemeNil {
    return item instanceof SchemeNil
  }
}

/**
 * 占位数据结构：表达式
 */
export class SchemeExp extends BaseData {
  next: SchemeExp | null = null

  constructor(public body: BaseData | null) {
    super()
  }

  static matches(item: BaseData): item is SchemeExp {
    return item instanceof SchemeExp
  }
}

/**
 * 占位数据结构：符号
 */
export class SchemeSym extends BaseData {
  constructor(public body: string) {
    super()
  }

  static matches(item: BaseData): item is SchemeSym {
    return item instanceof SchemeSym
  }
}

/**
 * 基础数据结构：number
 */
export class SchemeNumber extends BaseData {
  constructor(public value: number) {
    super()
  }

  static matches(item: BaseData): item is SchemeNumber {
    return item instanceof SchemeNumber
  }
}

/**
 * 基础数据结构：string
 */
export class SchemeString extends BaseData {
  constructor(public value: string) {
    super()
  }

  static matches(item: BaseData): item is SchemeString {
    return item instanceof SchemeString
  }
}

/**
 * 基础数据结构：boolean
 */
export class SchemeBoolean extends BaseData {
  constructor(public value: boolean) {
    super()
  }

  static matches(item: BaseData): item is SchemeBoolean {
    return item instanceof SchemeBoolean
  }

  static isTrue(item: BaseData): boolean {
    return SchemeBoolean.matches(item) && item.value
  }
}

/**
 * 基础数据结构：quote
 */
export class SchemeQuote extends BaseData {
  constructor(public value: string) {
    super()
  }

  static matches(item: BaseData): item is SchemeQuote {
    return item instanceof SchemeQuote
  }
}

/**
 * 基础数据结构：nil
 */
export class SchemeNil extends BaseData {
  constructor() {
    super()
  }

  static matches(item: BaseData): item is SchemeNil {
    return item instanceof SchemeNil
  }
}

/**
 * 把一段 token 数组解析成 data 单链表
 * Todo: 是否需要一个指向 parent 的指针？
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