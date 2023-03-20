import { type SchemeData, type Cont, type SchemeSym, type NodeData, SchemeList, SchemeBoolean, SchemeNumber } from '../parser/data';
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

interface Callback {
  (node: NodeData, env: Env): SchemeData
}

/**
 * 内置语法：
 * 1. 内置变量：'()
 * 2. 内置方法：cons、null?、car、cadr、cdr等
 */
export default class BuildInEvaluator implements IEvaluator {
  evaluatorMap: Map<string, Callback> = new Map()

  constructor(private evaluator: Evaluator) {
    this.evaluator = evaluator
    this.register('cons', this.cons)
    this.register('null?', this.isNull)
    this.register('car', this.car)
    this.register('cdr', this.cdr)
    this.register('cadr', this.cadr)
    this.register('=', this.isEqual)
    this.register('>', this.isMoreThan)
    this.register('<', this.isLessThan)
    this.register('+', this.add)
    this.register('-', this.minus)
    this.register('*', this.multiply)
    this.register('/', this.divide)
    this.register('min', this.min)
    this.register('max', this.max)
    this.register('abs', this.abs)
    this.register('zero?', this.isZero)
    this.register('length', this.length)
    this.register('not', this.not)
    this.register('and', this.and)
    this.register('or', this.or)
    this.register('display', this.display)
  }

  public matches(tag: string): boolean {
    return this.evaluatorMap.has(tag)
  }

  public evaluate(node: SchemeSym, env: Env, cont: Cont): SchemeData {
    const evaluator = this.evaluatorMap.get(node.tag)
    assert(evaluator, 'buildin evaluator evaluates error!')
    return cont(evaluator(node, env))
  }

  private register(name: string, proc: Callback) {
    this.evaluatorMap.set(name, proc.bind(this))
  }

  private evaluateFirstSentence(args: NodeData, env: Env): SchemeData {
    assert(args.next, 'buildin evaluator evaluates error: should have first sentence!')
    return this.evaluator.evaluate(args.next, env, x => x)
  }

  private evaluateSecondSentence(args: NodeData, env: Env): SchemeData {
    assert(args?.next?.next, 'buildin evaluator evaluates error: should have second sentence!')
    return this.evaluator.evaluate(args.next.next, env, x => x)
  }

  private cons(args: NodeData, env: Env) {
    return SchemeList.cons(
      this.evaluateFirstSentence(args, env),
      this.evaluateSecondSentence(args, env)
    )
  }

  private isNull(args: NodeData, env: Env) {
    return new SchemeBoolean(SchemeList.isNil(this.evaluateFirstSentence(args, env)))
  }

  private car(args: NodeData, env: Env) {
    return SchemeList
      .cast(this.evaluateFirstSentence(args, env))
      .car()
  }

  private cdr(args: NodeData, env: Env) {
    return SchemeList
      .cast(this.evaluateFirstSentence(args, env))
      .cdr()
  }

  private cadr(args: NodeData, env: Env) {
    return SchemeList
      .cast(this.evaluateFirstSentence(args, env))
      .cadr()
  }

  private isEqual(args: NodeData, env: Env) {
    const first = this.evaluateFirstSentence(args, env)
    const second = this.evaluateSecondSentence(args, env)
    return new SchemeBoolean(SchemeNumber.cast(first).value === SchemeNumber.cast(second).value)
  }

  private isLessThan(args: NodeData, env: Env) {
    const first = this.evaluateFirstSentence(args, env)
    const second = this.evaluateSecondSentence(args, env)
    return new SchemeBoolean(SchemeNumber.cast(first).value < SchemeNumber.cast(second).value)
  }

  private isMoreThan(args: NodeData, env: Env) {
    const first = this.evaluateFirstSentence(args, env)
    const second = this.evaluateSecondSentence(args, env)
    return new SchemeBoolean(SchemeNumber.cast(first).value > SchemeNumber.cast(second).value)
  }

  private add(args: NodeData, env: Env) {
    const first = this.evaluateFirstSentence(args, env)
    const second = this.evaluateSecondSentence(args, env)
    return new SchemeNumber(SchemeNumber.cast(first).value + SchemeNumber.cast(second).value)
  }

  private minus(args: NodeData, env: Env) {
    const first = this.evaluateFirstSentence(args, env)
    const second = this.evaluateSecondSentence(args, env)
    return new SchemeNumber(SchemeNumber.cast(first).value - SchemeNumber.cast(second).value)
  }

  private multiply(args: NodeData, env: Env) {
    const first = this.evaluateFirstSentence(args, env)
    const second = this.evaluateSecondSentence(args, env)
    return new SchemeNumber(SchemeNumber.cast(first).value * SchemeNumber.cast(second).value)
  }

  private divide(args: NodeData, env: Env) {
    const first = this.evaluateFirstSentence(args, env)
    const second = this.evaluateSecondSentence(args, env)
    return new SchemeNumber(SchemeNumber.cast(first).value / SchemeNumber.cast(second).value)
  }

  private min(args: NodeData, env: Env) {
    const first = this.evaluateFirstSentence(args, env)
    const second = this.evaluateSecondSentence(args, env)
    return new SchemeNumber(Math.min(SchemeNumber.cast(first).value, SchemeNumber.cast(second).value))
  }

  private max(args: NodeData, env: Env) {
    const first = this.evaluateFirstSentence(args, env)
    const second = this.evaluateSecondSentence(args, env)
    return new SchemeNumber(Math.max(SchemeNumber.cast(first).value, SchemeNumber.cast(second).value))
  }

  private abs(args: NodeData, env: Env) {
    const first = this.evaluateFirstSentence(args, env)
    return new SchemeNumber(Math.abs(SchemeNumber.cast(first).value))
  }

  private isZero(args: NodeData, env: Env) {
    const first = this.evaluateFirstSentence(args, env)
    return new SchemeBoolean(SchemeNumber.cast(first).value === 0)
  }

  private length(args: NodeData, env: Env) {
    const first = this.evaluateFirstSentence(args, env)
    return SchemeList.cast(first).getLength()
  }

  private not(args: NodeData, env: Env) {
    const first = this.evaluateFirstSentence(args, env)
    return new SchemeBoolean(!SchemeBoolean.cast(first).value)
  }

  private and(args: NodeData, env: Env) {
    let node: NodeData | null = args.next

    while (node) {
      const value = this.evaluator.evaluate(node, env, x => x)
      if (!SchemeBoolean.isTrue(value)) {
        return new SchemeBoolean(false)
      }
      node = node.next
    }

    return new SchemeBoolean(true)
  }

  private or(args: NodeData, env: Env) {
    let node: NodeData | null = args.next

    while (node) {
      const value = this.evaluator.evaluate(node, env, x => x)
      if (SchemeBoolean.isTrue(value)) {
        return new SchemeBoolean(true)
      }
      node = node.next
    }

    return new SchemeBoolean(false)
  }

  private display(args: NodeData, env: Env) {
    const first = this.evaluateFirstSentence(args, env)
    return first
  }
}
