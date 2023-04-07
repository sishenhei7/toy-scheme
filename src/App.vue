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
import { ref } from 'vue'
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

// interpreter test
let interpreter: Interpreter
const output = ref<string>('')
const callStack = ref<string>('')
const varScope = ref<string>('')

const createInterpreter = () => {
  let times = 0
  const interval = 60
  return new Interpreter(program.value, {
    log: (res: string) => {
      times += 1
      setTimeout(() => {
        console.log(res, times)
        output.value += res
      }, times * interval)
    }
  })
}

const handleRun = () => {
  output.value = ''
  createInterpreter().run()
}

const handleStep = () => {

}

const handleContinue = () => {

}
</script>

<style scoped>
.app-container {
  margin: 0 auto;
  width: 1200px;
}
</style>
