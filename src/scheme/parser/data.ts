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

  static matches(item: SchemeData): item is SchemeNil {
    return item instanceof SchemeNil
  }

  static isNil(item: SchemeData): item is SchemeNil {
    return item instanceof SchemeNil
  }
}

/**
 * 复杂数据结构：列表
 */
export class SchemeList extends SchemeData {
  constructor(
    private _car: SchemeData = new SchemeNil(),
    private _cdr: SchemeData = new SchemeNil(),
    public isQuote: boolean = false
  ) {
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

  public setQuote(value: boolean = true): SchemeList {
    this.isQuote = value
    return this
  }

  private getLengthBase(): number {
    if (SchemeNil.matches(this.car())) {
      return 0
    }

    if (SchemeNil.matches(this.cdr())) {
      return 1
    }

    return 1 + SchemeList.cast(this.cdr()).getLengthBase()
  }

  public getLength(): SchemeNumber {
    return new SchemeNumber(this.getLengthBase())
  }

  public toString(): string {
    // TODO
    return ''
    // return String(this.value)
  }

  static buildFromArray(args: SchemeData[]): SchemeList {
    let res = new SchemeList()
    let current = res
    for (const data of args) {
      if (SchemeNil.matches(current.car())) {
        current._car = data
      } else {
        const next = new SchemeList(data)
        current._cdr = next
        current = next
      }
    }
    return res
  }

  static matches(item: SchemeData): item is SchemeList {
    return item instanceof SchemeList
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
 * 解析数据结构
 */
export default function parseToken(tokenList: TokenItem[]): SchemeList {
  let tokenCursor = 0

  function parseTokenList(): SchemeData[] {
    let list: SchemeData[] = []
    while (tokenList.length && tokenCursor < tokenList.length) {
      const { type, value, start, end } = tokenList[tokenCursor++]
      switch (type) {
        case TokenType.Boolean:
          list.push(new SchemeBoolean(value === '#f').setLocationInfo(start, end))
          break
        case TokenType.Number:
          list.push(new SchemeNumber(Number(value)).setLocationInfo(start, end))
          break
        case TokenType.String:
          list.push(new SchemeString(value).setLocationInfo(start, end))
          break
        case TokenType.Quote:
          list.push(SchemeList.buildFromArray(parseTokenList()).setQuote().setLocationInfo(start, end))
          break
        case TokenType.Symbol:
          list.push(new SchemeSym(value).setLocationInfo(start, end))
          break
        case TokenType.LParen:
          list.push(SchemeList.buildFromArray(parseTokenList()).setLocationInfo(start, end))
          break
        case TokenType.RParen:
          return list
        default:
          assert(true, `Parsing Error: Unexpected TokenType: ${type}`)
          break
      }
    }

    assert(true, 'Parsing Error: right paren is less than left paren')
    return null as never
  }

  return SchemeList.buildFromArray(parseTokenList())
}
