<template>
  <div class="app-container">
    <AppTitle
      title="Toy-Scheme"
      class="mb-xl"
    />
    <AppBar
      :program-name="programName"
      :program-name-list="programNameList"
      :is-running="isRunning"
      :step="step"
      class="mb-m"
      @run="handleRun"
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
    <ProgramInner
      :output="output"
      :call-stack="callStack"
      :var-scope="varScope"
    />
  </div>
</template>

<script setup lang="ts">
import { ref, watch } from 'vue'
import AppTitle from './components/AppTitle.vue'
import AppBar from './components/AppBar.vue'
import MonacoEditor from './components/MonacoEditor.vue'
import ProgramInner from './components/ProgramInner.vue'
import programMap from './scheme/programs'
import Interpreter from './scheme'

// program
const programNameList = Object.keys(programMap)
const programName = ref<string>('')
const program = ref<string>('')

const handleSelectProgram = (name: string) => {
  programName.value = name
  program.value = programMap[name]
}
handleSelectProgram(programNameList[0])

let interpreter: Interpreter | null
const highlightRange = ref<[number, number, number, number] | null>(null)
const output = ref<string[]>([])
const callStack = ref<string[]>([])
const varScope = ref<string[]>([])
const isRunning = ref<boolean>(false)
const step = ref<number>(0)

watch(() => program.value, () => {
  interpreter = null
  step.value = 0
})

const createInterpreter = () => new Interpreter(program.value, {
  log: (res: string) => {
    console.log(res)
    output.value.push(res)
  }
})

const handleRun = () => {
  output.value = []
  isRunning.value = true
  createInterpreter().smoothRun(() => {
    isRunning.value = false
  })
}

const handleStep = () => {
  if (!interpreter) {
    interpreter = createInterpreter()
  }
  const stepRes = interpreter.step()
  const { range, stack, scope } = stepRes
  highlightRange.value = range
    ? [range.lineStart, range.columnStart, range.lineEnd, range.columnEnd]
    : null
  callStack.value = stack
  varScope.value = scope
  step.value += 1
}

const handleContinue = () => {
  if (interpreter) {
    highlightRange.value = null
    callStack.value = []
    varScope.value = []
    interpreter.smoothRun()
  }
}
</script>

<style scoped>
.app-container {
  margin: 0 auto;
  width: 1200px;
}
</style>
