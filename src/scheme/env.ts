import type { SchemeData } from './parser/data'
import { assert } from './utils'

export class StackFrame {
  constructor(private parentStackFrame: StackFrame | null) {}

  public getParent(): StackFrame | null {
    return this.parentStackFrame
  }
}

export class Env {
  private obj: Map<string, any> = new Map()

  constructor(
    private parentEnv: Env | null = null,
    private stackFrame: StackFrame | null = null
  ) {}

  public getParent(): Env | null {
    return this.parentEnv
  }

  public getStackFrame(): StackFrame | null {
    return this.stackFrame
  }

  public get(key: string): SchemeData {
    if (this.obj.has(key)) {
      return this.obj.get(key)
    }

    assert(this.parentEnv, `${key} is not defined!`)
    return this.parentEnv.get(key)
  }

  public set(key: string, value: SchemeData): SchemeData {
    if (this.obj.has(key)) {
      this.obj.set(key, value)
      return value
    }

    assert(this.parentEnv, `${key} is not defined in parent env!`)
    this.parentEnv.set(key, value)
    return value
  }

  public setCurrent(key: string, value: SchemeData): SchemeData {
    this.obj.set(key, value)
    return value
  }

  public define(key: string, value: SchemeData): SchemeData {
    assert(!this.obj.has(key), `${key} is already defined in this env!`)
    this.obj.set(key, value)
    return value
  }
}
