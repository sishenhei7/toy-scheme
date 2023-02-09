import type { BaseData } from './parser/data'

export class StackFrame {}

export class Env {
  private obj: Map<string, any> = new Map()

  constructor(private parentEnv: Env | null = null, private stackFrame: StackFrame | null = null) {}

  public get(key: string): BaseData {
    if (this.obj.has(key)) {
      return this.obj.get(key)
    }

    if (!this.parentEnv) {
      throw Error('${key} is not defined!')
    }

    return this.parentEnv.get(key)
  }

  public set(key: string, value: any): void {
    if (this.obj.has(key)) {
      this.obj.set(key, value)
    }

    if (!this.parentEnv) {
      throw Error('${key} is not defined!')
    }

    this.parentEnv.set(key, value)
  }

  public define(key: string, value: any): void {
    this.obj.set(key, value)
  }
}
