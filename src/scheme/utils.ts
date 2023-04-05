export function assert(condition: unknown, msg: string): asserts condition {
  if (!condition) {
    throw new Error(msg)
  }
}

export function guessNumber(str: string): boolean {
  return parseFloat(str).toString() === str
}

export function deleteDoubleQuote(st: string): string {
  const matched = /^"(.*)"$/.exec(st)
  return matched ? matched[1] : st
}
