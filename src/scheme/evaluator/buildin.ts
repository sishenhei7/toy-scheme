import { type SchemeData, SchemeCont, SchemeSym, SchemeList, SchemeBoolean, SchemeNumber, SchemeString } from '../parser/data';
import type { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'
import { assert, guessNumber } from '../utils'

type BaseBuildInEvaluator = (node: SchemeList, env: Env, cont: SchemeCont) => SchemeData
type OneArgsWrapper = (first: SchemeData) => SchemeData
type TwoArgsWrapper = (first: SchemeData, second: SchemeData) => SchemeData
type BuildInTraverseFunc = (node: SchemeList) => SchemeData

/**
 * 内置语法：
 * 1. 内置变量：'()
 * 2. 内置方法：cons、null?、car、cadr、cdr等
 */
export interface BuildInEvaluatorOptions {
  log?: Function
  prompt?: Function
}
export default class BuildInEvaluator implements IEvaluator {
  log: Function
  prompt: Function
  evaluatorMap: Map<string, BaseBuildInEvaluator> = new Map()

  constructor(private evaluator: Evaluator, options?: BuildInEvaluatorOptions) {
    this.evaluator = evaluator
    this.log = options?.log || console.log
    this.prompt = options?.prompt || prompt.bind(window)

    this.register('cons', this.twoArgsWrap(this.cons.bind(this)))
    this.register('null?', this.oneArgsWrap(this.isNull.bind(this)))
    this.register('car', this.oneArgsWrap(this.car.bind(this)))
    this.register('cdr', this.oneArgsWrap(this.cdr.bind(this)))
    this.register('cadr', this.oneArgsWrap(this.cadr.bind(this)))
    this.register('=', this.twoArgsWrap(this.isEqual.bind(this)))
    this.register('>', this.twoArgsWrap(this.isMoreThan.bind(this)))
    this.register('<', this.twoArgsWrap(this.isLessThan.bind(this)))
    this.register('+', this.twoArgsWrap(this.add.bind(this)))
    this.register('-', this.twoArgsWrap(this.minus.bind(this)))
    this.register('*', this.twoArgsWrap(this.multiply.bind(this)))
    this.register('/', this.twoArgsWrap(this.divide.bind(this)))
    this.register('min', this.twoArgsWrap(this.min.bind(this)))
    this.register('max', this.twoArgsWrap(this.max.bind(this)))
    this.register('abs', this.oneArgsWrap(this.abs.bind(this)))
    this.register('zero?', this.oneArgsWrap(this.isZero.bind(this)))
    this.register('length', this.oneArgsWrap(this.length.bind(this)))
    this.register('not', this.oneArgsWrap(this.not.bind(this)))
    this.register('and', this.and)
    this.register('or', this.or)
    this.register('ask', this.ask)
    this.register('display', this.display)
  }

  public matches(node: SchemeData): boolean {
    return SchemeSym.matches(node) && this.evaluatorMap.has(node.value)
  }

  public evaluate(node: SchemeList, env: Env, cont: SchemeCont): SchemeData {
    const evaluator = this.evaluatorMap.get(SchemeSym.cast(node.car()).value)
    assert(evaluator, 'buildin evaluator evaluates error!')
    return evaluator(node, env, cont)
  }

  private register(name: string, proc: BaseBuildInEvaluator): void {
    this.evaluatorMap.set(name, proc.bind(this))
  }

  private oneArgsWrap(evaluator: OneArgsWrapper): BaseBuildInEvaluator {
    return (node: SchemeList, env: Env, cont: SchemeCont) => this.evaluator.evaluate(
      node.cadr(),
      env,
      new SchemeCont((first: SchemeData) => cont.setValue(evaluator(first)))
    )
  }

  private twoArgsWrap(evaluator: TwoArgsWrapper): BaseBuildInEvaluator {
    return (node: SchemeList, env: Env, cont: SchemeCont) => this.evaluator.evaluate(
      node.cadr(),
      env,
      new SchemeCont(
        (first: SchemeData) => this.evaluator.evaluate(
          node.caddr(),
          env,
          new SchemeCont((second: SchemeData) => cont.setValue(evaluator(first, second)))
        )
      )
    )
  }

  private cons(first: SchemeData, second: SchemeData): SchemeData {
    return SchemeList.cons(first, second)
  }

  private isNull(first: SchemeData): SchemeData {
    return new SchemeBoolean(SchemeList.isNil(SchemeList.cast(first)))
  }

  private car(first: SchemeData): SchemeData {
    return SchemeList.cast(first).car()
  }

  private cdr(first: SchemeData): SchemeData {
    return SchemeList.cast(first).cdr()
  }

  private cadr(first: SchemeData): SchemeData {
    return SchemeList.cast(first).cadr()
  }

  private isEqual(first: SchemeData, second: SchemeData): SchemeData {
    return new SchemeBoolean(SchemeNumber.cast(first).value === SchemeNumber.cast(second).value)
  }

  private isLessThan(first: SchemeData, second: SchemeData): SchemeData {
    return new SchemeBoolean(SchemeNumber.cast(first).value < SchemeNumber.cast(second).value)
  }

  private isMoreThan(first: SchemeData, second: SchemeData): SchemeData {
    return new SchemeBoolean(SchemeNumber.cast(first).value > SchemeNumber.cast(second).value)
  }

  private add(first: SchemeData, second: SchemeData): SchemeData {
    return new SchemeNumber(SchemeNumber.cast(first).value + SchemeNumber.cast(second).value)
  }

  private minus(first: SchemeData, second: SchemeData): SchemeData {
    return new SchemeNumber(SchemeNumber.cast(first).value - SchemeNumber.cast(second).value)
  }

  private multiply(first: SchemeData, second: SchemeData): SchemeData {
    return new SchemeNumber(SchemeNumber.cast(first).value * SchemeNumber.cast(second).value)
  }

  private divide(first: SchemeData, second: SchemeData): SchemeData {
    return new SchemeNumber(SchemeNumber.cast(first).value / SchemeNumber.cast(second).value)
  }

  private min(first: SchemeData, second: SchemeData): SchemeData {
    return new SchemeNumber(Math.min(SchemeNumber.cast(first).value, SchemeNumber.cast(second).value))
  }

  private max(first: SchemeData, second: SchemeData): SchemeData {
    return new SchemeNumber(Math.max(SchemeNumber.cast(first).value, SchemeNumber.cast(second).value))
  }

  private abs(first: SchemeData): SchemeData {
    return new SchemeNumber(Math.abs(SchemeNumber.cast(first).value))
  }

  private isZero(first: SchemeData): SchemeData {
    return new SchemeBoolean(SchemeNumber.cast(first).value === 0)
  }

  private length(first: SchemeData): SchemeData {
    return SchemeList.cast(first).getLength()
  }

  private not(first: SchemeData): SchemeData {
    return new SchemeBoolean(!SchemeBoolean.cast(first).value)
  }

  private and(node: SchemeList, env: Env, cont: SchemeCont): SchemeData {
    if (SchemeList.isNil(node.cdr())) {
      return cont.setValue(new SchemeBoolean(true))
    }
    return this.evaluator.evaluate(node.cadr(), env, new SchemeCont((first: SchemeData) => {
      if (!SchemeBoolean.isTrue(first)) {
        return cont.setValue(new SchemeBoolean(false))
      }
      return this.and(node.cdr(), env, cont)
    }))
  }

  private or(node: SchemeList, env: Env, cont: SchemeCont): SchemeData {
    if (SchemeList.isNil(node.cdr())) {
      return cont.setValue(new SchemeBoolean(false))
    }
    return this.evaluator.evaluate(node.cadr(), env, new SchemeCont((data: SchemeData) => {
      if (SchemeBoolean.isTrue(data)) {
        return cont.setValue(new SchemeBoolean(true))
      }
      return this.or(node.cdr(), env, cont)
    }))
  }

  private ask(node: SchemeList, env: Env, cont: SchemeCont): SchemeData {
    let msg = ''
    const traverse: BuildInTraverseFunc = (node: SchemeList) => {
      if (SchemeList.isNil(node)) {
        msg = msg.trim()
        const answer = this.prompt(msg)
        this.log(answer + '\n')
        return cont.setValue(guessNumber(answer) ? new SchemeNumber(Number(answer)) : new SchemeString(answer))
      }

      return this.evaluator.evaluate(
        node.car(),
        env,
        new SchemeCont((data: SchemeData) => {
          msg += ` ${data.toString()}`
          return traverse(node.cdr())
        })
      )
    }
    return traverse(node.cdr())
  }

  private display(node: SchemeList, env: Env, cont: SchemeCont): SchemeData {
    let res = ''
    const traverse: BuildInTraverseFunc = (node: SchemeList) => {
      if (SchemeList.isNil(node)) {
        this.log(res)
        return cont.setValue(new SchemeString(res))
      }

      return this.evaluator.evaluate(
        node.car(),
        env,
        new SchemeCont((data: SchemeData) => {
          res += `${res ? ' ' : ''}${data.toString()}`
          return traverse(node.cdr())
        })
      )
    }
    return traverse(node.cdr())
  }
}
