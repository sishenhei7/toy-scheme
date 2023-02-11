// type Falsy = null | undefined | false | 0 | ''; // No NaN type

// export function assert(value: Falsy, message: string): never;
// export function assert<T>(value: T, message: string): T;
// export function assert<T>(value: Falsy | T, message: string): never | T {
//   if (!value) throw new Error(`AssertError: ${message}`);
//   return value;
// }

export function assert(condition: unknown, msg: string): asserts condition {
  if (!condition) {
    throw new Error(msg)
  }
}
