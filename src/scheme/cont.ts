import type { BaseData } from "./parser/data"

/**
 * TODO：怎么把 cont 变成一等公民，然后在 js 执行的时候，消除函数的执行栈。
 * continuation 是一等公民
 */

export type Cont = (node: BaseData) => BaseData

// export class Cont {
//   constructor(private value: ContValue) {}

//   call(val: BaseData) {
//     return this.value.call(val)
//   }
// }