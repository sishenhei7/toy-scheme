import { type SchemeData, type Cont, type SchemeSym, type NodeData, SchemeList } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'

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
    this.evaluatorMap.set('cons', (args: NodeData, env: Env) => new SchemeList(evaluator.evaluate(args, env, x => x), evaluator.evaluate(args.next, env, x => x)))
    this.evaluatorMap.set('null?', (args: NodeData, env: Env) => SchemeList.isNil(evaluator.evaluate(args, env, x => x)))
    // this.evaluatorMap.set('display', (args: NodeData) => )
  }

  public matches(tag: string): boolean {
    return this.evaluatorMap.has(tag)
  }

  public evaluate(node: SchemeSym, env: Env, cont: Cont): SchemeData {
    return cont
  }
}
