import type { Cont, SchemeData, SchemeSym } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'

/**
 * 语法：
 * 1.let 表达式：
 * (let binds body)
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
export class LetEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(tag: string): boolean {
    return this.isLet(tag) || this.isLetStar(tag) || this.isLetRec(tag)
  }

  public evaluate(node: SchemeSym, env: Env, cont: Cont): SchemeData {
    // TODO: ts-error
    // @ts-expect-error
    return node
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
}
