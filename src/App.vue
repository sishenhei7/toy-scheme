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
    <ProgramInner />
  </div>
</template>

<script setup lang="ts">
import { ref } from 'vue'
import AppTitle from './components/AppTitle.vue'
import AppBar from './components/AppBar.vue'
import MonacoEditor from './components/MonacoEditor.vue'
import ProgramInner from './components/ProgramInner.vue'
import programMap from './scheme/programs'

// program
const programNameList = Object.keys(programMap)
const programName = ref<string>(programNameList[0])
const program = ref<string>(programMap[programName.value])
const handleSelectProgram = (name: string) => {
  programName.value = name
  program.value = programMap[name]
}

const handleRun = () => {

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
