/**
 * 数据结构：
 * 占位数据: 表达式、符号
 * 简单数据：number、string、boolean、quote
 * 复杂数据：暂时还没有（continuation是一等公民应该怎么弄呢）
 * 其它数据：continuation、function
 */
import { type TokenItem, TokenType } from './token'
import { assert } from '../utils'

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
  next: BaseData | null = null
}

/**
 * 占位数据结构：表达式
 */
export class SchemeExp extends BaseData {
  constructor(public body: BaseData | null) {
    super()
  }

  static matches(item: SchemeData): item is SchemeExp {
    return item instanceof SchemeExp
  }
}

/**
 * 占位数据结构：符号
 */
export class SchemeSym extends BaseData {
  constructor(public tag: string) {
    super()
  }

  static matches(item: SchemeData): item is SchemeSym {
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

  static matches(item: SchemeData): item is SchemeNumber {
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

  static matches(item: SchemeData): item is SchemeString {
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

  static matches(item: SchemeData): item is SchemeBoolean {
    return item instanceof SchemeBoolean
  }

  static isTrue(item: SchemeData): boolean {
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

  static matches(item: SchemeData): item is SchemeQuote {
    return item instanceof SchemeQuote
  }
}

/**
 * 基础数据结构：nil (因为scheme里面并没有nil，所以这里不引入nil)
 */
// export class SchemeNil extends BaseData {
//   constructor(private value = null) {
//     super()
//   }

//   static matches(item: BaseData): item is SchemeNil {
//     return item instanceof SchemeNil
//   }

//   static isNil(item: BaseData): item is SchemeNil {
//     return item instanceof SchemeNil
//   }
// }

/**
 * 其它数据结构：continuation 是一等公民
 */
export type Cont = (node: SchemeData) => SchemeData

/**
 * 其它数据结构：function 是一等公民
 */
export class SchemeFunction {
  constructor(public args: SchemeData, public body: SchemeData) { }

  static matches(item: any): item is SchemeFunction {
    return item instanceof SchemeFunction
  }
}

// cont、function 都是一等公民？
export type SchemeData = BaseData | Cont | SchemeFunction

/**
 * 把一段 token 数组解析成 data 单链表
 * Todo: 是否需要一个指向 parent 的指针？
 */
export function getParser(tokenList: TokenItem[]): () => BaseData | null {
  let tokenCursor = 0
  const rParenEndList: number[] = []

  function parseTokenList(): ReturnType<ReturnType<typeof getParser>> {
    let last: BaseData | null = null
    let first: BaseData | null = null
    let shouldEnd = false

    while (!shouldEnd && tokenList.length && tokenCursor < tokenList.length) {
      const { type, value, start, end } = tokenList[tokenCursor++]
      let currentData: BaseData | null = null

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
          assert(rParenEndList.length > 0, 'Parsing error: too many left paren')
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
