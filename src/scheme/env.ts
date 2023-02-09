export class StackFrame {}

export class Env {
  private obj: Map<string, any> = new Map()

  constructor(private parentEnv: Env | null = null, private stackFrame: StackFrame | null = null) {}

  // TODO: add return type
  public get(key: string): any {
    if (this.obj.has(key)) {
      return this.obj.get(key)
    }

    if (!this.parentEnv) {
      throw Error('${key} is not defined!')
    }

    return this.parentEnv.get(key)
  }

  // TODO: add type
  public set(key: string, value: any): void {
    if (this.obj.has(key)) {
      this.obj.set(key, value)
    }

    if (!this.parentEnv) {
      throw Error('${key} is not defined!')
    }

    this.parentEnv.set(key, value)
  }

  // TODO: add type
  public define(key: string, value: any): void {
    this.obj.set(key, value)
  }
}
