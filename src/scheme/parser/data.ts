/**
 * 数据结构：
 * 占位数据: 表达式、符号
 * 简单数据：number、string、boolean、quote
 * 复杂数据：列表
 * 其它数据：continuation、proc
 */
import { type TokenItem, TokenType } from './token'
import type { Env } from '../env'
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

export class NodeData extends ILocation {
  next: NodeData | null = null
}

/**
 * 占位数据结构：表达式
 */
export class SchemeExp extends NodeData {
  constructor(public body: NodeData | null) {
    super()
  }

  static matches(item: NodeData): item is SchemeExp {
    return item instanceof SchemeExp
  }
}

/**
 * 占位数据结构：符号
 */
export class SchemeSym extends NodeData {
  constructor(public tag: string) {
    super()
  }

  toString(): string {
    return this.tag
  }

  static matches(item: NodeData): item is SchemeSym {
    return item instanceof SchemeSym
  }
}

/**
 * 基础数据结构：number
 */
export class SchemeNumber extends NodeData {
  constructor(public value: number) {
    super()
  }

  toString(): string {
    return String(this.value)
  }

  static matches(item: SchemeData): item is SchemeNumber {
    return item instanceof SchemeNumber
  }
}

/**
 * 基础数据结构：string
 */
export class SchemeString extends NodeData {
  constructor(public value: string) {
    super()
  }

  toString(): string {
    return this.value
  }

  static matches(item: SchemeData): item is SchemeString {
    return item instanceof SchemeString
  }
}

/**
 * 基础数据结构：boolean
 */
export class SchemeBoolean extends NodeData {
  constructor(public value: boolean) {
    super()
  }

  toString(): string {
    return String(this.value)
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
export class SchemeQuote extends NodeData {
  constructor(public value: string) {
    super()
  }

  toString(): string {
    return this.value
  }

  static matches(item: SchemeData): item is SchemeQuote {
    return item instanceof SchemeQuote
  }
}

/**
 * 基础数据结构：nil (因为scheme里面并没有nil，所以这里不引入nil)
 */
// export class SchemeNil extends NodeData {
//   constructor(private value = null) {
//     super()
//   }

//   static matches(item: NodeData): item is SchemeNil {
//     return item instanceof SchemeNil
//   }

//   static isNil(item: NodeData): item is SchemeNil {
//     return item instanceof SchemeNil
//   }
// }

/**
 * 复杂数据结构：列表
 */
export class SchemeList {
  constructor(private car: NodeData, private cdr: NodeData) {}

  toString(): string {
    return ''
    // return String(this.value)
  }

  static isNil() {

  }
}


/**
 * 其它数据结构：continuation 是一等公民
 */
export type Cont = (node: SchemeData) => SchemeData

/**
 * 其它数据结构：proc 是一等公民
 */
export class SchemeProc {
  constructor(
    public name: string,
    public params: NodeData | null,
    public body: NodeData,
    public envClosure: Env
  ) { }

  toString(): string {
    return '<<function>>'
  }

  static matches(item: SchemeData): item is SchemeProc {
    return item instanceof SchemeProc
  }
}

// cont、proc 都是一等公民？
export type BaseData = SchemeNumber | SchemeString | SchemeBoolean | SchemeQuote
export type SchemeData = BaseData | Cont | SchemeProc

/**
 * 把一段 token 数组解析成 data 单链表
 * Todo: 是否需要一个指向 parent 的指针？
 */
export function getParser(tokenList: TokenItem[]): () => NodeData | null {
  let tokenCursor = 0
  const rParenEndList: number[] = []

  function parseTokenList(): ReturnType<ReturnType<typeof getParser>> {
    let last: NodeData | null = null
    let first: NodeData | null = null
    let shouldEnd = false

    while (!shouldEnd && tokenList.length && tokenCursor < tokenList.length) {
      const { type, value, start, end } = tokenList[tokenCursor++]
      let currentData: NodeData | null = null

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
