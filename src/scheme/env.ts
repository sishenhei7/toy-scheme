import type { SchemeData } from './parser/data'
import { assert } from './utils'

export class StackFrame {
  constructor(private parentStackFrame: StackFrame) {}

  public getParent(): StackFrame {
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

    assert(this.parentEnv, `${key} is not defined!`)
    this.parentEnv.set(key, value)
    return value
  }

  public define(key: string, value: any): void {
    this.obj.set(key, value)
  }
}
