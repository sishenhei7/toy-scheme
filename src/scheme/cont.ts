import type { BaseData } from "./parser/data"

/**
 * continuation 是一等公民
 */

export type ContValue = (node: BaseData) => BaseData

export class Cont {
  constructor(private value: ContValue) {}

  call(val: BaseData) {
    return this.value.call(val)
  }
}