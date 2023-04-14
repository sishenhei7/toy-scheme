<template>
  <div class="app-container">
    <AppTitle title="Toy-Scheme" class="mg-0 mb-xxxl" />
    <AppBar
      :program-name="programName"
      :program-name-list="programNameList"
      :is-running="isRunning"
      :step="step"
      class="mb-m"
      @run="handleRun"
      @stop="handleStop"
      @step="handleStep"
      @continue="handleContinue"
      @program="handleSelectProgram"
    />
    <MonacoEditor
      v-model="program"
      :program-name="programName"
      :highlight-range="highlightRange"
      class="mb-m"
    />
    <ProgramInner :output="output" :call-stack="callStack" :var-scope="varScope" :show-tips="step === 0" />
  </div>
</template>

<script setup lang="ts">
import { ref, watch } from 'vue'
import programMap from '@toy-scheme/programs'
import Interpreter, { type StepResponse } from '@toy-scheme/interpreter-node'
import AppTitle from './components/AppTitle.vue'
import AppBar from './components/AppBar.vue'
import MonacoEditor from './components/MonacoEditor.vue'
import ProgramInner from './components/ProgramInner.vue'
import MyWorker from './worker?worker'

// program
const programNameList = Object.keys(programMap)
const programName = ref<string>('')
const program = ref<string>('')

const handleSelectProgram = (name: string) => {
  programName.value = name
  program.value = programMap[name]
}
handleSelectProgram(programNameList[0])

let interpreter: Interpreter | null = null
let cachedRange: StepResponse['range'] = null
const highlightRange = ref<[number, number, number, number] | null>(null)
const output = ref<string[]>([])
const callStack = ref<string[]>([])
const varScope = ref<string[]>([])
const isRunning = ref<boolean>(false)
const step = ref<number>(0)
const shouldStop = ref<boolean>(false)

const cleanProgram = () => {
  isRunning.value = false
  shouldStop.value = false

  highlightRange.value = null
  callStack.value = []
  varScope.value = []
  step.value = 0
  interpreter = null
}

watch(
  () => program.value,
  () => {
    interpreter = null
    step.value = 0
  }
)

// monaco 会把 \n 解析为换行，所以这里要转几次
const createInterpreter = () =>
  new Interpreter(program.value.replace(/\n"/g, '\\n"'), {
    log: (res: string) => {
      const list = res.split('\\n')
      for (let i = 0; i < list.length; i += 1) {
        const item = list[i]
        if (i === 0 && output.value.length > 0) {
          const last = output.value.pop()
          output.value.push(`${last}${item}`)
        } else {
          output.value.push(item)
        }
      }
    }
  })

let worker: Worker | null
let isUsingWorker = false
const getWorker = () => {
  if (worker) {
    return worker
  }

  const newWorker = new MyWorker()
  newWorker.onmessage = (event) => {
    if (event.data.type === 'over') {
      cleanProgram()
    } else if (event.data.type === 'log' && isRunning.value) {
      const res = event.data.data
      const list = res.split('\\n')
      for (let i = 0; i < list.length; i += 1) {
        const item = list[i]
        if (i === 0 && output.value.length > 0) {
          const last = output.value.pop()
          output.value.push(`${last}${item}`)
        } else {
          output.value.push(item)
        }
      }
    } else if (event.data.type === 'prompt') {
      prompt(event.data.data)
    }
  }
  worker = newWorker
  return newWorker
}

const handleRun = () => {
  output.value = []
  isRunning.value = true

  // woker 里面 prompt 不好实现，回退到 smoothRun
  isUsingWorker = !program.value.includes('ask')
  if (isUsingWorker) {
    getWorker().postMessage({ program: program.value.replace(/\n"/g, '\\n"') })
  } else {
    createInterpreter().smoothRun(
      () => cleanProgram(),
      () => shouldStop.value
    )
  }
}

const handleStop = () => {
  if (isUsingWorker) {
    cleanProgram()
    worker && worker.terminate()
    worker = null
  } else {
    shouldStop.value = true
  }
}

const isRangeSame = (r1: StepResponse['range'], r2: StepResponse['range']) => {
  if (r1 && r2) {
    return (
      r1.lineStart === r2.lineStart &&
      r1.columnStart === r2.columnStart &&
      r1.lineEnd === r2.lineEnd &&
      r1.columnEnd === r2.columnEnd
    )
  }
  return false
}

const handleStep = () => {
  if (!interpreter) {
    output.value = []
    interpreter = createInterpreter()
  }
  const stepRes = interpreter.step()
  const { range, stack, scope } = stepRes

  // 有些地方的 schemeList 是包裹性质的，所以没有 range
  // 有些地方由于是包裹性质的，所以 range 和上一个一样
  if (!range || isRangeSame(cachedRange, range)) {
    handleStep()
    return
  }

  const { lineStart, columnStart, lineEnd, columnEnd } = range
  highlightRange.value = [lineStart, columnStart, lineEnd, columnEnd]
  callStack.value = stack
  varScope.value = scope
  step.value += 1
  cachedRange = range
}

const handleContinue = () => {
  if (interpreter) {
    interpreter.smoothRun()
    cleanProgram()
  }
}
</script>

<style scoped>
.app-container {
  margin: 0 auto;
  width: 1200px;
}
</style>
