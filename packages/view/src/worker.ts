import Interpreter from './scheme'

globalThis.onmessage = (event) => {
  const interpreter = new Interpreter(event.data.program, {
    log: (res: string) => {
      globalThis.postMessage({
        type: 'log',
        data: res
      })
    },
    prompt: (data: number | string) =>
      globalThis.postMessage({
        type: 'prompt',
        data: data
      })
  })

  interpreter.run()

  globalThis.postMessage({ type: 'over' })
}
