/**
 * 数据结构：
 * 简单数据：number、string、boolean、quote、symbol
 * 复杂数据：list、cont、proc
 */
import { type TokenItem, TokenType } from './token'
import type { Env } from '../env'
import { assert, deleteDoubleQuote } from '../utils'

export interface ILocationRange {
  lineStart: number
  columnStart: number
  lineEnd: number
  columnEnd: number
}
export class ILocation {
  public range: ILocationRange | null = null

  public setLocationInfo(range: ILocationRange | null): this {
    this.range = range
    return this
  }
}

export class SchemeData extends ILocation {
  private env: Env | null = null

  public setEnv(env: Env): this {
    this.env = env
    return this
  }

  public getEnv(): Env | null {
    return this.env
  }
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

  public toString(): string {
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

  public toString(): string {
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

  public toString(): string {
    return this.value ? '#t' : '#f'
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

  constructor(
    private _car: SchemeData | null,
    private _cdr: SchemeList | null,
  ) {
    super()
  }

  public cdr(): SchemeList {
    return this._cdr || SchemeList.buildSchemeNil()
  }

  public cddr(): SchemeList {
    return this.cdr().cdr()
  }

  public cdddr(): SchemeList {
    return this.cdr().cdr().cdr()
  }

  public car(): SchemeData {
    assert(this._car, `car error: caller is nil!`)
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

  public setNotEval(): this {
    this.shouldEval = false
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

  public toString(): string {
    let res = ''
    let current: SchemeList = this
    while (!SchemeList.isNil(current)) {
      res += ' ' + current.car().toString()
      current = current.cdr()
    }
    return `(${res.trim()})`
  }

  public toDisplay(): string {
    let res = ''
    let current: SchemeList = this
    while (!SchemeList.isNil(current)) {
      res += ' ' + current.car().toString()
      current = current.cdr()
    }
    return `${res.trim()}`
  }

  static buildSchemeNil(): SchemeList {
    return new SchemeList(null, null)
  }

  static buildOne(arg: SchemeData): SchemeList {
    return SchemeList.matches(arg) ? arg : SchemeList.buildFromAtom(arg)
  }

  static buildFromAtom(arg: Exclude<SchemeData, SchemeList>): SchemeList {
    return new SchemeList(arg, SchemeList.buildSchemeNil())
  }

  static buildFromArray(args: SchemeData[]): SchemeList {
    const len = args.length
    let res = SchemeList.buildSchemeNil()
    for (let i = len - 1; i >= 0; i -= 1) {
      const node = SchemeList.buildFromAtom(args[i])
      node.setCdr(res)
      res = node
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

  // 注意：cons 不需要遍历到 car 的末尾，它是一个 O(1) 的操作
  // 而且，cons 不能改变 car
  static cons(car: SchemeData, cdr: SchemeData): SchemeList {
    return new SchemeList(car, SchemeList.buildOne(cdr))
  }
}

/**
 * 其它数据结构：continuation 是一等公民
 */
export class SchemeCont extends SchemeData {
  private value: SchemeData = SchemeList.buildSchemeNil()

  static Identity = new SchemeCont(x => x)

  constructor(private f: (node: SchemeData) => SchemeData) {
    super()
  }

  get fString(): string {
    return this.f.toString()
  }

  public setValue(value: SchemeData): this {
    this.value = value
    return this
  }

  public toString(): string {
    return `<<continuation>>`
  }

  public call(): SchemeData {
    return this.f(this.value)
  }

  static cast(item: SchemeData): SchemeCont {
    assert(SchemeCont.matches(item), 'Invalid SchemeCont!')
    return item
  }

  static matches(item: SchemeData): item is SchemeCont {
    return item instanceof SchemeCont
  }
}


/**
 * 其它数据结构：proc 是一等公民
 */
export class SchemeProc extends SchemeData {
  constructor(
    public name: string,
    public params: SchemeList,
    public body: SchemeList,
    public envClosure: Env
  ) {
    super()
  }

  public toString(): string {
    return `<<function ${this.name}>>`
  }

  static matches(item: SchemeData): item is SchemeProc {
    return item instanceof SchemeProc
  }
}

export default function parseToken(tokenList: TokenItem[]): SchemeList {
  let tokenCursor = 0

  function parseTokenList(onlyOne: boolean = false): [SchemeData[], ILocationRange | null] {
    let list: SchemeData[] = []
    let startRange: ILocationRange | null = null
    let endRange: ILocationRange | null = null

    while (tokenList.length && tokenCursor < tokenList.length) {
      const { type, value, range } = tokenList[tokenCursor++]

      if (!startRange) {
        startRange = range
      }
      endRange = range

      let shouldBreakLoop = onlyOne
      switch (type) {
        case TokenType.Boolean:
          list.push(new SchemeBoolean(value === '#t').setLocationInfo(range))
          break
        case TokenType.Number:
          list.push(new SchemeNumber(Number(value)).setLocationInfo(range))
          break
        case TokenType.String:
          list.push(new SchemeString(deleteDoubleQuote(value)).setLocationInfo(range))
          break
        case TokenType.Quote: {
          // 'xxxx 的 quote 只需要 parse 一个
          const [tokenList, listRange] = parseTokenList(!value.startsWith("'("))
          list.push(SchemeList.buildFromArray(tokenList).setNotEval().setLocationInfo(listRange))
          break
        }
        case TokenType.Symbol:
          list.push(new SchemeSym(value).setLocationInfo(range))
          break
        case TokenType.LParen: {
          const [tokenList, listRange] = parseTokenList()
          list.push(SchemeList.buildFromArray(tokenList).setLocationInfo(listRange))
          break
        }
        case TokenType.RParen:
          shouldBreakLoop = true
          break
        default:
          assert(false, `Parsing Error: Unexpected TokenType: ${type}`)
      }

      if (shouldBreakLoop) {
        break
      }
    }

    let newRange: ILocationRange | null = null
    if (startRange && endRange) {
      newRange = {
        lineStart: startRange.lineStart,
        columnStart: startRange.columnStart - 1,
        lineEnd: endRange.lineEnd,
        columnEnd: endRange.columnEnd
      }
    }
    return [list, newRange]
  }

  const [list, listRange] = parseTokenList()
  return SchemeList.buildFromArray(list).setLocationInfo(listRange)
}
