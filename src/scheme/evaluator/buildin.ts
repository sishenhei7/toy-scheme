import { type SchemeData, type Cont, type SchemeSym, type NodeData, SchemeList, SchemeBoolean, SchemeNumber } from '../parser/data';
import type { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'
import { assert } from '../utils'

// makeProc('cons', (args: Sv) => new SvCons(SvCons.car(args), SvCons.cadr(args)));
// makeProc('null?', (args: Sv) => SvBool.fromBoolean(SvCons.isNil(SvCons.car(args))));
// makeProc('car', (args: Sv) => SvCons.car(SvCons.car(args)));
// makeProc('cadr', (args: Sv) => SvCons.cadr(SvCons.car(args)));
// makeProc('cdr', (args: Sv) => SvCons.cdr(SvCons.car(args)));
// makeProc('=', (args: Sv) => SvBool.fromBoolean(SvNumber.val(SvCons.car(args)) === SvNumber.val(SvCons.cadr(args))));
// makeProc('>', (args: Sv) => SvBool.fromBoolean(SvNumber.val(SvCons.car(args)) > SvNumber.val(SvCons.cadr(args))));
// makeProc('<', (args: Sv) => SvBool.fromBoolean(SvNumber.val(SvCons.car(args)) < SvNumber.val(SvCons.cadr(args))));
// makeProc('*', (args: Sv) => new SvNumber(SvNumber.val(SvCons.car(args)) * SvNumber.val(SvCons.cadr(args))));
// makeProc('-', (args: Sv) => new SvNumber(SvNumber.val(SvCons.car(args)) - SvNumber.val(SvCons.cadr(args))));
// makeProc('+', (args: Sv) => new SvNumber(SvNumber.val(SvCons.car(args)) + SvNumber.val(SvCons.cadr(args))));
// makeProc('/', (args: Sv) => new SvNumber(SvNumber.val(SvCons.car(args)) / SvNumber.val(SvCons.cadr(args))));
// makeProc('min', (args: Sv) => new SvNumber(Math.min(SvNumber.val(SvCons.car(args)), SvNumber.val(SvCons.cadr(args)))));
// makeProc('max', (args: Sv) => new SvNumber(Math.max(SvNumber.val(SvCons.car(args)), SvNumber.val(SvCons.cadr(args)))));
// makeProc('abs', (args: Sv) => new SvNumber(Math.abs(SvNumber.val(SvCons.car(args)))));
// makeProc('zero?', (args: Sv) => SvBool.fromBoolean(SvNumber.val(SvCons.car(args)) === 0));
// makeProc('length', (args: Sv) => SvCons.lengthI(SvCons.car(args)));
// makeProc('not', (args: Sv) => SvBool.not(SvCons.car(args)));
// makeProc('and', (args: Sv) => SvBool.and(args));
// makeProc('or', (args: Sv) => SvBool.or(args));
// makeProc('display', (args: Sv) => {
//   while (!SvCons.isNil(args)) {
//     log(SvCons.car(args).toDisplayString());
//     args = SvCons.cdr(args);
//   }
//   return SvCons.Nil;
// });

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
  (node: NodeData, env: Env): unknown
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
    this.evaluatorMap.set('cons', this.cons)
    this.evaluatorMap.set('null?', this.isNull)
    this.evaluatorMap.set('car', this.car)
    this.evaluatorMap.set('cdr', this.cdr)
    this.evaluatorMap.set('cadr', this.cadr)
    this.evaluatorMap.set('=', this.isEqual)
    this.evaluatorMap.set('+', this.add)
    this.evaluatorMap.set('-', this.minus)
    this.evaluatorMap.set('*', this.multiply)
    this.evaluatorMap.set('/', this.divide)
    this.evaluatorMap.set('min', this.min)
    this.evaluatorMap.set('max', this.max)
    this.evaluatorMap.set('abs', this.abs)
    this.evaluatorMap.set('zero?', this.isZero)
    this.evaluatorMap.set('length', this.length)
    this.evaluatorMap.set('not', this.not)
    this.evaluatorMap.set('and', this.and)
    this.evaluatorMap.set('or', this.or)
    this.evaluatorMap.set('display', this.display)
  }

  public matches(tag: string): boolean {
    return this.evaluatorMap.has(tag)
  }

  public evaluate(node: SchemeSym, env: Env, cont: Cont): SchemeData {
    return cont
  }

  // cons x y
  private cons(args: NodeData, env: Env) {
    const first = this.evaluator.evaluate(args, env, x => x)
    const second = this.evaluator.evaluate(args.next, env, x => x)
    return SchemeList.cons(first, second)
  }

  private isNull(args: NodeData, env: Env) {
    const first = this.evaluator.evaluate(args, env, x => x)
    return new SchemeBoolean(SchemeList.isNil(first))
  }

  private car(args: NodeData, env: Env) {
    const first = this.evaluator.evaluate(args, env, x => x)
    return SchemeList.cast(first).car()
  }

  private cdr(args: NodeData, env: Env) {
    const first = this.evaluator.evaluate(args, env, x => x)
    return SchemeList.cast(first).cdr()
  }

  private cadr(args: NodeData, env: Env) {
    const first = this.evaluator.evaluate(args, env, x => x)
    return SchemeList.cast(first).cadr()
  }

  private isEqual(args: NodeData, env: Env) {
    const first = this.evaluator.evaluate(args, env, x => x)
    const second = this.evaluator.evaluate(args.next, env, x => x)
    return new SchemeBoolean(SchemeNumber.cast(first).value === SchemeNumber.cast(second).value)
  }

  private isLessThan(args: NodeData, env: Env) {
    const first = this.evaluator.evaluate(args, env, x => x)
    const second = this.evaluator.evaluate(args.next, env, x => x)
    return new SchemeBoolean(SchemeNumber.cast(first).value < SchemeNumber.cast(second).value)
  }

  private isMoreThan(args: NodeData, env: Env) {
    const first = this.evaluator.evaluate(args, env, x => x)
    const second = this.evaluator.evaluate(args.next, env, x => x)
    return new SchemeBoolean(SchemeNumber.cast(first).value > SchemeNumber.cast(second).value)
  }

  private add(args: NodeData, env: Env) {
    const first = this.evaluator.evaluate(args, env, x => x)
    const second = this.evaluator.evaluate(args.next, env, x => x)
    return new SchemeNumber(SchemeNumber.cast(first).value + SchemeNumber.cast(second).value)
  }

  private minus(args: NodeData, env: Env) {
    const first = this.evaluator.evaluate(args, env, x => x)
    const second = this.evaluator.evaluate(args.next, env, x => x)
    return new SchemeNumber(SchemeNumber.cast(first).value - SchemeNumber.cast(second).value)
  }

  private multiply(args: NodeData, env: Env) {
    const first = this.evaluator.evaluate(args, env, x => x)
    const second = this.evaluator.evaluate(args.next, env, x => x)
    return new SchemeNumber(SchemeNumber.cast(first).value * SchemeNumber.cast(second).value)
  }

  private divide(args: NodeData, env: Env) {
    const first = this.evaluator.evaluate(args, env, x => x)
    const second = this.evaluator.evaluate(args.next, env, x => x)
    return new SchemeNumber(SchemeNumber.cast(first).value / SchemeNumber.cast(second).value)
  }

  private min(args: NodeData, env: Env) {
    const first = this.evaluator.evaluate(args, env, x => x)
    const second = this.evaluator.evaluate(args.next, env, x => x)
    return new SchemeNumber(Math.min(SchemeNumber.cast(first).value, SchemeNumber.cast(second).value))
  }

  private max(args: NodeData, env: Env) {
    const first = this.evaluator.evaluate(args, env, x => x)
    const second = this.evaluator.evaluate(args.next, env, x => x)
    return new SchemeNumber(Math.max(SchemeNumber.cast(first).value, SchemeNumber.cast(second).value))
  }

  private abs(args: NodeData, env: Env) {
    const first = this.evaluator.evaluate(args, env, x => x)
    return new SchemeNumber(Math.abs(SchemeNumber.cast(first).value))
  }

  private isZero(args: NodeData, env: Env) {
    const first = this.evaluator.evaluate(args, env, x => x)
    return new SchemeBoolean(SchemeNumber.cast(first).value === 0)
  }

  private length(args: NodeData, env: Env) {
    const first = this.evaluator.evaluate(args, env, x => x)
    return SchemeList.cast(first).getLength()
  }

  private not(args: NodeData, env: Env) {
    const first = this.evaluator.evaluate(args, env, x => x)
    return new SchemeBoolean(!SchemeBoolean.cast(first).value)
  }

  private and(args: NodeData, env: Env) {
    let node: NodeData | null = args

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
    let node: NodeData | null = args

    while (node) {
      const value = this.evaluator.evaluate(node, env, x => x)
      if (SchemeBoolean.isTrue(value)) {
        return new SchemeBoolean(true)
      }
      node = node.next
    }

    return new SchemeBoolean(false)
  }

  private display(args: NodeData) {
    console.log('display', args)
    return args.toString()
  }
}
