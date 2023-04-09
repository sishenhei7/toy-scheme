import Interpreter from './scheme'

globalThis.onmessage = (event) => {
  const program = event.data.program

  const interpreter = new Interpreter(program, {
    log: (res: string) => {
      globalThis.postMessage({
        type: 'log',
        data: res
      })
    },
    prompt: (data: number | string) => globalThis.postMessage({
      type: 'prompt',
      data: data
    })
  })

  interpreter.run()

  globalThis.postMessage({ type: 'over' })
};