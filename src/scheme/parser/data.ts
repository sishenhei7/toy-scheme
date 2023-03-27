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

  public toStringBase(): string {
    return this.value
  }

  static matches(item: SchemeData): item is SchemeSym {
    return item instanceof SchemeSym
  }

  static cast(item: SchemeData): SchemeSym {
    assert(SchemeSym.matches(item), 'Invalid SchemeSym!')
    return item
  }
}

/**
 * 基础数据结构：number
 */
export class SchemeNumber extends SchemeData {
  constructor(public value: number) {
    super()
  }

  public toStringBase(): string {
    return String(this.value)
  }

  static matches(item: SchemeData): item is SchemeNumber {
    return item instanceof SchemeNumber
  }

  static cast(item: SchemeData): SchemeNumber {
    assert(SchemeNumber.matches(item), 'Invalid SchemeNumber!')
    return item
  }
}

/**
 * 基础数据结构：string
 */
export class SchemeString extends SchemeData {
  constructor(public value: string) {
    super()
  }

  public toStringBase(): string {
    return this.value
  }

  static matches(item: SchemeData): item is SchemeString {
    return item instanceof SchemeString
  }

  static cast(item: SchemeData): SchemeString {
    assert(SchemeString.matches(item), 'Invalid SchemeString!')
    return item
  }
}

/**
 * 基础数据结构：boolean
 */
export class SchemeBoolean extends SchemeData {
  constructor(public value: boolean) {
    super()
  }

  public toStringBase(): string {
    return String(this.value)
  }

  static matches(item: SchemeData): item is SchemeBoolean {
    return item instanceof SchemeBoolean
  }

  static cast(item: SchemeData): SchemeBoolean {
    assert(SchemeBoolean.matches(item), 'Invalid SchemeBoolean!')
    return item
  }

  static isTrue(item: SchemeData): boolean {
    return SchemeBoolean.cast(item).value
  }
}

/**
 * 复杂数据结构：列表
 * 注意：把 null 收拢在 SchemeList 内部，不让它向外暴露
 */
export class SchemeList extends SchemeData {
  public shouldEval: boolean = true
  static Nil = new SchemeList(null, null)

  constructor(
    private _car: SchemeData | null,
    private _cdr: SchemeList | null,
  ) {
    super()
  }

  public cdr(): SchemeList {
    return this._cdr || SchemeList.Nil
  }

  public cddr(): SchemeList {
    return this.cdr().cdr()
  }

  public cdddr(): SchemeList {
    return this.cdr().cdr().cdr()
  }

  public car(): SchemeData {
    assert(this._car, `Car error: ${this} is nil!`)
    return this._car
  }

  public cadr(): SchemeData {
    return this.cdr().car()
  }

  public caddr(): SchemeData {
    return this.cdr().cdr().car()
  }

  public cadddr(): SchemeData {
    return this.cdr().cdr().cdr().car()
  }

  public setCar(item: SchemeData): SchemeList {
    this._car = item
    return this
  }

  public setCdr(item: SchemeData): SchemeList {
    this._cdr = SchemeList.buildOne(item)
    return this
  }

  public setEval(value: boolean = false): SchemeList {
    this.shouldEval = value
    return this
  }

  private getLengthBase(): number {
    if (SchemeList.isNil(this)) {
      return 0
    }

    return 1 + this.cdr().getLengthBase()
  }

  public getLength(): SchemeNumber {
    return new SchemeNumber(this.getLengthBase())
  }

  public toStringBase(): string {
    if (SchemeList.isNil(this)) {
      return "'()"
    }

    // TODO
    return ''
  }

  static buildOne(arg: SchemeData): SchemeList {
    return SchemeList.matches(arg) ? arg : SchemeList.buildFromAtom(arg)
  }

  static buildFromAtom(arg: Exclude<SchemeData, SchemeList>): SchemeList {
    return new SchemeList(arg, SchemeList.Nil)
  }

  static buildFromArray(args: SchemeData[]): SchemeList {
    let res = SchemeList.Nil
    let current = res
    for (const arg of args) {
      if (SchemeList.isNil(current)) {
        current.setCar(arg)
      } else {
        const next = SchemeList.buildOne(arg)
        current.setCdr(next)
        current = next
      }
    }
    return res
  }

  static isNil(item: SchemeList): boolean {
    return item._car === null && item._cdr === null
  }

  static matches(item: SchemeData): item is SchemeList {
    return item instanceof SchemeList
  }

  static cast(item: SchemeData): SchemeList {
    assert(SchemeList.matches(item), 'Invalid SchemeList!')
    return item
  }

  static cons(car: SchemeData, cdr: SchemeData): SchemeList {
    let carList = SchemeList.buildOne(car)
    const cdrList = SchemeList.buildOne(cdr)

    if (SchemeList.isNil(carList)) {
      return cdrList
    }

    while (!SchemeList.isNil(carList.cdr())) {
      carList = carList.cdr()
    }
    return carList.setCdr(cdrList)
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

  static matches(item: SchemeData): item is Continuation {
    return item instanceof Continuation
  }
}


/**
 * 其它数据结构：proc 是一等公民
 */
export class SchemeProc extends SchemeData {
  constructor(
    public name: string,
    public params: SchemeList,
    public body: SchemeData,
    public envClosure: Env
  ) {
    super()
  }

  public toStringBase(): string {
    return '<<function>>'
  }

  static matches(item: SchemeData): item is SchemeProc {
    return item instanceof SchemeProc
  }
}

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
          list.push(SchemeList.buildFromArray(parseTokenList()).setEval().setLocationInfo(start, end))
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
          assert(false, `Parsing Error: Unexpected TokenType: ${type}`)
      }
    }

    return list
  }

  return SchemeList.buildFromArray(parseTokenList())
}
