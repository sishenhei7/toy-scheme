/**
 * 数据结构：
 * 简单数据：number、string、boolean、quote、symbol
 * 复杂数据：list
 * 其它数据：cont、proc
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

export class SchemeData extends ILocation {

}

/**
 * 基础数据结构：符号
 */
export class SchemeSym extends SchemeData {
  constructor(public value: string) {
    super()
  }

  public toString(): string {
    return this.value
  }

  static matches(item: SchemeData): item is SchemeSym {
    return item instanceof SchemeSym
  }
}

/**
 * 基础数据结构：number
 */
export class SchemeNumber extends SchemeData {
  constructor(public value: number) {
    super()
  }

  public toString(): string {
    return String(this.value)
  }

  static cast(item: SchemeData): SchemeNumber {
    assert(SchemeNumber.matches(item), 'Invalid SchemeNumber!')
    return item
  }

  static matches(item: SchemeData): item is SchemeNumber {
    return item instanceof SchemeNumber
  }
}

/**
 * 基础数据结构：string
 */
export class SchemeString extends SchemeData {
  constructor(public value: string) {
    super()
  }

  public toString(): string {
    return this.value
  }

  static matches(item: SchemeData): item is SchemeString {
    return item instanceof SchemeString
  }
}

/**
 * 基础数据结构：boolean
 */
export class SchemeBoolean extends SchemeData {
  constructor(public value: boolean) {
    super()
  }

  public toString(): string {
    return String(this.value)
  }

  static cast(item: SchemeData): SchemeBoolean {
    assert(SchemeBoolean.matches(item), 'Invalid SchemeBoolean!')
    return item
  }

  static matches(item: SchemeData): item is SchemeBoolean {
    return item instanceof SchemeBoolean
  }

  static isTrue(item: SchemeData): boolean {
    return SchemeBoolean.cast(item).value
  }
}

/**
 * 基础数据结构：quote
 */
export class SchemeQuote extends SchemeData {
  constructor(public value: string) {
    super()
  }

  public toString(): string {
    return this.value
  }

  static matches(item: SchemeData): item is SchemeQuote {
    return item instanceof SchemeQuote
  }
}

/**
 * Nil，仅用在空数组里面
 */
export class SchemeNil extends SchemeData {
  public toString(): string {
    return '()'
  }

  static isNil(item: SchemeData): item is SchemeNil {
    return item instanceof SchemeNil
  }
}

/**
 * 复杂数据结构：列表
 */
export class SchemeList extends SchemeData {
  constructor(private _car: SchemeData, private _cdr: SchemeData) {
    super()
  }

  public car(): SchemeData {
    return this._car
  }

  public cdr(): SchemeData {
    return this._cdr
  }

  public cadr(): SchemeData {
    return SchemeList.cast(this._car)._cdr
  }

  public setCar(item: SchemeData): void {
    this._car = item
  }

  public setCdr(item: SchemeData): void {
    this._cdr = item
  }

  static matches(item: SchemeData): item is SchemeList {
    return item instanceof SchemeList
  }

  public getLength(): SchemeNumber {
    let res = 1
    let list: SchemeData = this
    while (list) {
      res += 1

      if (!SchemeList.matches(list)) {
        break
      }

      list = list.cdr()
    }

    return new SchemeNumber(res)
  }

  public toString(): string {
    // TODO
    return ''
    // return String(this.value)
  }

  static cast(item: SchemeData): SchemeList {
    assert(SchemeList.matches(item), 'Invalid SchemeList!')
    return item
  }

  static cons(_car: SchemeData, _cdr: SchemeData): SchemeList {
    return new SchemeList(_car, _cdr)
  }
}


/**
 * 其它数据结构：continuation 是一等公民
 */
export class Continuation extends SchemeData {
  static Identity = new Continuation(x => x)

  constructor(private f: (node: SchemeData) => SchemeData) {
    super()
  }

  public call(node: SchemeData): SchemeData {
    return this.f(node)
  }
}


/**
 * 其它数据结构：proc 是一等公民
 */
export class SchemeProc extends SchemeData {
  constructor(
    public name: string,
    public params: SchemeData | null,
    public body: SchemeData,
    public envClosure: Env
  ) {
    super()
  }

  public toString(): string {
    return '<<function>>'
  }

  static matches(item: SchemeData): item is SchemeProc {
    return item instanceof SchemeProc
  }
}

/**
 * 把一段 token 数组解析成 data 单链表
 * Todo: 是否需要一个指向 parent 的指针？
 */
export function getParser(tokenList: TokenItem[]): () => SchemeData | null {
  let tokenCursor = 0
  const rParenEndList: number[] = []

  function parseTokenList(): ReturnType<ReturnType<typeof getParser>> {
    let last: SchemeData | null = null
    let first: SchemeData | null = null
    let shouldEnd = false

    while (!shouldEnd && tokenList.length && tokenCursor < tokenList.length) {
      const { type, value, start, end } = tokenList[tokenCursor++]
      let currentData: SchemeData | null = null

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
          console.log(first, last)
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
