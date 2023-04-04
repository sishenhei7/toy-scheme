import { type Thunk, SchemeList, SchemeCont, type SchemeData, SchemeSym } from '../parser/data'
import { Env, StackFrame } from '../env'
import type { IEvaluator, Evaluator } from './index'

/**
 * 语法：
 * 1.let 表达式：
 * (let ((i 1) (j 2))
 *    (+ i j))
 * 2.let* 表达式(可以在定义中引用之前定义的变量)：
 * (let* ((i 1) (j (+ i 2)))
 *    (* i j))
 * 3.letrec 表达式(可以在定义中递归的引用自己)：
 * (letrec ((iter (lambda (ls0 n)
 *    (if (null? ls0)
 *    n
 *    (iter (cdr ls0) (+ (car ls0) n))))))
 *  (iter ls 0))
 */
export default class LetEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(node: SchemeData): boolean {
    return SchemeSym.matches(node)
      && (this.isLet(node.value) || this.isLetStar(node.value) || this.isLetRec(node.value))
  }

  public evaluate(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    const newEnv = new Env(env, new StackFrame(env.getStackFrame()))
    return this.evaluateDefination(
      SchemeList.cast(node.cadr()),
      newEnv,
      new SchemeCont((_: SchemeData) => this.evaluator.evaluate(node.caddr(), newEnv, cont)))
  }

  // Remember that let computes the initial values of variables,
  // then initializes all of the variables' storage,
  // and only then do any of the bindings become visible
  private isLet(value: string): boolean {
    return value === 'let'
  }
  private isLetStar(value: string): boolean {
    return value === 'let*'
  }
  private isLetRec(value: string): boolean {
    return value === 'letrec'
  }
  private evaluateDefination(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    if (!SchemeList.isNil(node)) {
      const defination = SchemeList.cast(node.car())
      const name = SchemeSym.cast(defination.car()).value
      const body = defination.cdr()
      return this.evaluator.evaluateList(body, env, new SchemeCont((data: SchemeData) => {
        env.setCurrent(name, data)
        return this.evaluateDefination(node.cdr(), env, cont)
      }))
    }
    return cont.call(SchemeList.buildSchemeNil())
  }
}
