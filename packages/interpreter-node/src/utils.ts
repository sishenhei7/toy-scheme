export function assert(condition: unknown, msg: string): asserts condition {
  if (!condition) {
    throw new Error(msg)
  }
}

export function guessNumber(str: string): boolean {
  return parseFloat(str).toString() === str
}

export function deleteDoubleQuote(st: string): string {
  const matched = /^"([\s\S]*)"$/.exec(st)
  return matched ? matched[1] : st
}

export function nextTick(fn: Function): void {
  if (typeof setImmediate === 'function') {
    setImmediate(() => fn())
  } else if (typeof MessageChannel !== 'undefined') {
    const ch = new MessageChannel()
    const port1 = ch.port1
    const port2 = ch.port2
    port2.onmessage = (() => fn())
    port1.postMessage(1)
  } else {
    setTimeout(() => fn(), 0)
  }
}
