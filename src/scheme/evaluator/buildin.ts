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

interface Callback {
  (node: SchemeList, env: Env, cont: SchemeCont): Thunk
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
    // this.register('or', this.or)
    this.register('display', this.display)
  }

  public matches(value: string): boolean {
    return this.evaluatorMap.has(value)
  }

  public evaluate(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    const evaluator = this.evaluatorMap.get(SchemeSym.cast(node.car()).value)
    assert(evaluator, 'buildin evaluator evaluates error!')
    return evaluator(node, env, cont)
  }

  private register(name: string, proc: Callback): void {
    this.evaluatorMap.set(name, proc.bind(this))
  }

  private evaluateFirstArgs(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluator.evaluate(node.cadr(), env, cont)
  }

  private evaluateSecondArgs(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluator.evaluate(node.caddr(), env, cont)
  }

  private cons(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluateFirstArgs(node, env, new SchemeCont((first: SchemeData) => () => {
      return this.evaluateSecondArgs(node, env, new SchemeCont((second: SchemeData) => () => {
        return cont.call(SchemeList.cons(first, second))
      }))
    }))
  }

  private isNull(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluateFirstArgs(node, env, new SchemeCont((first: SchemeData) => () => {
      return cont.call(new SchemeBoolean(SchemeList.isNil(SchemeList.cast(first))))
    }))
  }

  private car(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluateFirstArgs(node, env, new SchemeCont((first: SchemeData) => () => {
      return cont.call(SchemeList.cast(first).car())
    }))
  }

  private cdr(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluateFirstArgs(node, env, new SchemeCont((first: SchemeData) => () => {
      return cont.call(SchemeList.cast(first).cdr())
    }))
  }

  private cadr(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluateFirstArgs(node, env, new SchemeCont((first: SchemeData) => () => {
      return cont.call(SchemeList.cast(first).cadr())
    }))
  }

  private isEqual(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluateFirstArgs(node, env, new SchemeCont((first: SchemeData) => () => {
      return this.evaluateSecondArgs(node, env, new SchemeCont((second: SchemeData) => () => {
        return cont.call(new SchemeBoolean(SchemeNumber.cast(first).value === SchemeNumber.cast(second).value))
      }))
    }))
  }

  private isLessThan(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluateFirstArgs(node, env, new SchemeCont((first: SchemeData) => () => {
      return this.evaluateSecondArgs(node, env, new SchemeCont((second: SchemeData) => () => {
        return cont.call(new SchemeBoolean(SchemeNumber.cast(first).value < SchemeNumber.cast(second).value))
      }))
    }))
  }

  private isMoreThan(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluateFirstArgs(node, env, new SchemeCont((first: SchemeData) => () => {
      return this.evaluateSecondArgs(node, env, new SchemeCont((second: SchemeData) => () => {
        return cont.call(new SchemeBoolean(SchemeNumber.cast(first).value > SchemeNumber.cast(second).value))
      }))
    }))
  }

  private add(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluateFirstArgs(node, env, new SchemeCont((first: SchemeData) => () => {
      return this.evaluateSecondArgs(node, env, new SchemeCont((second: SchemeData) => () => {
        return cont.call(new SchemeNumber(SchemeNumber.cast(first).value + SchemeNumber.cast(second).value))
      }))
    }))
  }

  private minus(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluateFirstArgs(node, env, new SchemeCont((first: SchemeData) => () => {
      return this.evaluateSecondArgs(node, env, new SchemeCont((second: SchemeData) => () => {
        return cont.call(new SchemeNumber(SchemeNumber.cast(first).value - SchemeNumber.cast(second).value))
      }))
    }))
  }

  private multiply(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluateFirstArgs(node, env, new SchemeCont((first: SchemeData) => () => {
      return this.evaluateSecondArgs(node, env, new SchemeCont((second: SchemeData) => () => {
        return cont.call(new SchemeNumber(SchemeNumber.cast(first).value * SchemeNumber.cast(second).value))
      }))
    }))
  }

  private divide(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluateFirstArgs(node, env, new SchemeCont((first: SchemeData) => () => {
      return this.evaluateSecondArgs(node, env, new SchemeCont((second: SchemeData) => () => {
        return cont.call(new SchemeNumber(SchemeNumber.cast(first).value / SchemeNumber.cast(second).value))
      }))
    }))
  }

  private min(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluateFirstArgs(node, env, new SchemeCont((first: SchemeData) => () => {
      return this.evaluateSecondArgs(node, env, new SchemeCont((second: SchemeData) => () => {
        return cont.call(new SchemeNumber(Math.min(SchemeNumber.cast(first).value, SchemeNumber.cast(second).value)))
      }))
    }))
  }

  private max(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluateFirstArgs(node, env, new SchemeCont((first: SchemeData) => () => {
      return this.evaluateSecondArgs(node, env, new SchemeCont((second: SchemeData) => () => {
        return cont.call(new SchemeNumber(Math.max(SchemeNumber.cast(first).value, SchemeNumber.cast(second).value)))
      }))
    }))
  }

  private abs(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluateFirstArgs(node, env, new SchemeCont((first: SchemeData) => () => {
      return cont.call(new SchemeNumber(Math.abs(SchemeNumber.cast(first).value)))
    }))
  }

  private isZero(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluateFirstArgs(node, env, new SchemeCont((first: SchemeData) => () => {
      return cont.call(new SchemeBoolean(SchemeNumber.cast(first).value === 0))
    }))
  }

  private length(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluateFirstArgs(node, env, new SchemeCont((first: SchemeData) => () => {
      return cont.call(SchemeList.cast(first).getLength())
    }))
  }

  private not(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluateFirstArgs(node, env, new SchemeCont((first: SchemeData) => () => {
      return cont.call(new SchemeBoolean(!SchemeBoolean.cast(first).value))
    }))
  }

  private and(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    if (SchemeList.isNil(node.cdr())) {
      return cont.call(new SchemeBoolean(true))
    }
    return this.evaluator.evaluate(node.cadr(), env, new SchemeCont((first: SchemeData) => () => {
      if (!SchemeBoolean.isTrue(first)) {
        return cont.call(new SchemeBoolean(false))
      }
      return this.and(node.cdr(), env, cont)
    }))
    // while (!SchemeList.isNil(node.cdr())) {
    //   node = node.cdr()
    //   const value = this.evaluator.evaluate(node.car(), env)
    //   if (!SchemeBoolean.isTrue(value)) {
    //     return new SchemeBoolean(false)
    //   }
    // }

    // return new SchemeBoolean(true)
  }

  // private or(node: SchemeList, env: Env, cont: SchemeCont): SchemeBoolean {
  //   while (!SchemeList.isNil(node.cdr())) {
  //     node = node.cdr()
  //     const value = this.evaluator.evaluate(node.car(), env)
  //     if (SchemeBoolean.isTrue(value)) {
  //       return new SchemeBoolean(true)
  //     }
  //   }

  //   return new SchemeBoolean(false)
  // }

  private display(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluateFirstArgs(node, env, new SchemeCont((first: SchemeData) => () => {
      console.log(first.toString())
      return cont.call(first)
    }))
  }
}
