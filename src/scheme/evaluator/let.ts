import { NodeData, SchemeExp, Continuation, type SchemeData, SchemeSym } from '../parser/data'
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

  public matches(tag: string): boolean {
    return this.isLet(tag) || this.isLetStar(tag) || this.isLetRec(tag)
  }

  public evaluate(node: SchemeSym, env: Env, cont: Continuation): SchemeData {
    assert(node.next && node.next.next, 'Let evaluting error: should follow 2 expressions!')
    const defination = node.next
    const body = node.next.next
    const newEnv = new Env(env)
    assert(SchemeExp.matches(defination), 'Let evaluting error: should follow SchemeExp!')
    this.evaluateDefination(defination.body, env)
    return this.evaluator.evaluate(body, newEnv, cont)
  }

  private isLet(tag: string) {
    return tag === 'let'
  }
  private isLetStar(tag: string) {
    return tag === 'let*'
  }
  private isLetRec(tag: string) {
    return tag === 'letrec'
  }
  private evaluateDefination(node: NodeData | null, env: Env): void {
    while (node && SchemeExp.matches(node) && node.body) {
      assert(SchemeSym.matches(node.body), 'Let evaluting error: defs should be SchemeSym!')
      const varNode = node.body
      env.define(varNode.tag, this.evaluator.evaluate(varNode.next, env))
      node = node.next
    }
  }
}
