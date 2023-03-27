import { SchemeList, Continuation, type SchemeData, SchemeSym } from '../parser/data'
import { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'
import { assert } from '../utils'

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

  public matches(value: string): boolean {
    return this.isLet(value) || this.isLetStar(value) || this.isLetRec(value)
  }

  public evaluate(node: SchemeList, env: Env, cont: Continuation): SchemeData {
    this.evaluateDefination(SchemeList.cast(node.cadr()), env)
    return this.evaluator.evaluate(node.caddr(), new Env(env), cont)
  }

  private isLet(value: string): boolean {
    return value === 'let'
  }
  private isLetStar(value: string): boolean {
    return value === 'let*'
  }
  private isLetRec(value: string): boolean {
    return value === 'letrec'
  }
  private evaluateDefination(node: SchemeList, env: Env): void {
    while (!SchemeList.isNil(node)) {
      const defination = SchemeList.cast(node.car())
      const varNode = SchemeSym.cast(defination.car())
      env.define(varNode.value, this.evaluator.evaluate(defination.cdr(), env))
      node = node.cdr()
    }
  }
}
