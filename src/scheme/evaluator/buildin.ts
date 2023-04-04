import { type Thunk, type SchemeData, SchemeCont, SchemeSym, SchemeList, SchemeBoolean, SchemeNumber } from '../parser/data';
import type { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'
import { assert } from '../utils'

// makeProc('ask', (args: Sv) => {
//   let msg = "";
//   while (!SvCons.isNil(args)) {
//     msg += SvCons.car(args).toDisplayString();
//     args = SvCons.cdr(args);
//   }

//   log("> " + msg + " ");
//   const answer = parseInt(prompt(msg), 10);
//   log(answer + "\n");
//   return new SvNumber(answer);
// });


type BaseBuildInEvaluator = (node: SchemeList, env: Env, cont: SchemeCont) => Thunk
type OneArgsWrapper = (first: SchemeData) => SchemeData
type TwoArgsWrapper = (first: SchemeData, second: SchemeData) => SchemeData

/**
 * 内置语法：
 * 1. 内置变量：'()
 * 2. 内置方法：cons、null?、car、cadr、cdr等
 */
export default class BuildInEvaluator implements IEvaluator {
  evaluatorMap: Map<string, BaseBuildInEvaluator> = new Map()

  constructor(private evaluator: Evaluator) {
    this.evaluator = evaluator
    this.register('cons', this.twoArgsWrap(this.cons))
    this.register('null?', this.oneArgsWrap(this.isNull))
    this.register('car', this.oneArgsWrap(this.car))
    this.register('cdr', this.oneArgsWrap(this.cdr))
    this.register('cadr', this.oneArgsWrap(this.cadr))
    this.register('=', this.twoArgsWrap(this.isEqual))
    this.register('>', this.twoArgsWrap(this.isMoreThan))
    this.register('<', this.twoArgsWrap(this.isLessThan))
    this.register('+', this.twoArgsWrap(this.add))
    this.register('-', this.twoArgsWrap(this.minus))
    this.register('*', this.twoArgsWrap(this.multiply))
    this.register('/', this.twoArgsWrap(this.divide))
    this.register('min', this.twoArgsWrap(this.min))
    this.register('max', this.twoArgsWrap(this.max))
    this.register('abs', this.oneArgsWrap(this.abs))
    this.register('zero?', this.oneArgsWrap(this.isZero))
    this.register('length', this.oneArgsWrap(this.length))
    this.register('not', this.oneArgsWrap(this.not))
    this.register('and', this.and)
    this.register('or', this.or)
    this.register('display', this.oneArgsWrap(this.display))
  }

  public matches(node: SchemeData): boolean {
    return SchemeSym.matches(node) && this.evaluatorMap.has(node.value)
  }

  public evaluate(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
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
      new SchemeCont((first: SchemeData) => cont.call(evaluator(first)))
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
          new SchemeCont((second: SchemeData) => cont.call(evaluator(first, second)))
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

  private and(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    if (SchemeList.isNil(node.cdr())) {
      return cont.call(new SchemeBoolean(true))
    }
    return this.evaluator.evaluate(node.cadr(), env, new SchemeCont((first: SchemeData) => {
      if (!SchemeBoolean.isTrue(first)) {
        return cont.call(new SchemeBoolean(false))
      }
      return this.and(node.cdr(), env, cont)
    }))
  }

  private or(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    if (SchemeList.isNil(node.cdr())) {
      return cont.call(new SchemeBoolean(false))
    }
    return this.evaluator.evaluate(node.cadr(), env, new SchemeCont((first: SchemeData) => {
      if (SchemeBoolean.isTrue(first)) {
        return cont.call(new SchemeBoolean(true))
      }
      return this.or(node.cdr(), env, cont)
    }))
  }

  private display(first: SchemeData): SchemeData {
    console.log(first.toString())
    return first
  }
}
