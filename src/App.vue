<template>
  <div class="app-container">
    <AppTitle
      title="Toy-Scheme"
      class="mb-xl"
    />
    <AppBar
      :program-name="programName"
      :program-name-list="programNameList"
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

watch(() => program.value, () => {
  interpreter = null
})

const createInterpreter = (interval: number = 0) => {
  let times = 0
  output.value = []
  return new Interpreter(program.value, {
    log: (res: string) => {
      times += 1
      setTimeout(() => {
        console.log(res, times)
        output.value.push(res)
      }, times * interval)
    }
  })
}

const handleRun = () => {
  createInterpreter(60).run()
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
}

const handleContinue = () => {
  if (interpreter) {
    interpreter.run()
  }
}
</script>

<style scoped>
.app-container {
  margin: 0 auto;
  width: 1200px;
}
</style>
