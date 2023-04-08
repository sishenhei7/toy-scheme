import { SchemeList, SchemeCont, type SchemeData, SchemeSym } from '../parser/data'
import { Env, StackFrame } from '../env'
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
type LetStarTraverseFunc = (defNode: SchemeList, newEnv: Env) => SchemeData
export default class LetEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(node: SchemeData): boolean {
    return this.isLet(node) || this.isLetStar(node) || this.isLetRec(node)
  }

  public evaluate(node: SchemeList, env: Env, cont: SchemeCont): SchemeData {
    const peek = node.car()
    const defNode = SchemeList.cast(node.cadr())
    const bodyNode = node.caddr()
    const newEnv = new Env(env, new StackFrame(env.getStackFrame()))
    const newCont = new SchemeCont((_: SchemeData) => this.evaluator.evaluate(bodyNode, newEnv, cont))

    if (this.isLet(peek)) {
      return this.evaluateDefinition(defNode, newEnv, newCont)
    }

    if (this.isLetStar(peek)) {
      return this.evaluateLetStar(node, newEnv, newCont)
    }

    if (this.isLetRec(peek)) {
      this.traverseDefinition(defNode, newEnv)
      return this.evaluateDefinition(defNode, newEnv, newCont)
    }

    assert(false, 'Evaluate let error!')
  }

  // Remember that let computes the initial values of variables,
  // then initializes all of the variables' storage,
  // and only then do any of the bindings become visible
  private isLet(node: SchemeData): boolean {
    return SchemeSym.matches(node) && node.value === 'let'
  }
  private isLetStar(node: SchemeData): boolean {
    return SchemeSym.matches(node) && node.value === 'let*'
  }
  private isLetRec(node: SchemeData): boolean {
    return SchemeSym.matches(node) && node.value === 'letrec'
  }
  private evaluateDefinition(node: SchemeList, env: Env, cont: SchemeCont): SchemeData {
    if (!SchemeList.isNil(node)) {
      const defination = SchemeList.cast(node.car())
      const name = SchemeSym.cast(defination.car()).value
      const body = defination.cdr()
      return this.evaluator.evaluateList(body, env, new SchemeCont((data: SchemeData) => {
        env.setCurrent(name, data)
        return this.evaluateDefinition(node.cdr(), env, cont)
      }))
    }
    return cont.setValue(SchemeList.buildSchemeNil())
  }

  private evaluateLetStar(node: SchemeList, env: Env, cont: SchemeCont): SchemeData {
    const traverse: LetStarTraverseFunc = (defNode: SchemeList, newEnv: Env) => {
      if (!SchemeList.isNil(defNode)) {
        const defination = SchemeList.cast(defNode.car())
        const name = SchemeSym.cast(defination.car()).value
        const body = defination.cdr()
        return this.evaluator.evaluateList(body, newEnv, new SchemeCont((data: SchemeData) => {
          newEnv = new Env(newEnv, new StackFrame(newEnv.getStackFrame()))
          newEnv.setCurrent(name, data)
          return traverse(defNode.cdr(), newEnv)
        }))
      }

      const bodyNode = node.caddr()
      return this.evaluator.evaluate(bodyNode, newEnv, cont)
    }

    return traverse(SchemeList.cast(node.cadr()), new Env(env, new StackFrame(env.getStackFrame())))
  }

  private traverseDefinition(node: SchemeList, env: Env): void {
    if (!SchemeList.isNil(node)) {
      const defination = SchemeList.cast(node.car())
      const name = SchemeSym.cast(defination.car()).value
      env.setCurrent(name, SchemeList.buildSchemeNil())
      this.traverseDefinition(node.cdr(), env)
    }
  }
}
