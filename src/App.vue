<template>
  <div class="app-container">
    <AppTitle title="Toy-Scheme" class="mb-20" />
    <AppBar :program-list="['test1', 'test2']" class="mb-12" />
    <MonacoEditor />
    <ProgramInner />
  </div>
</template>

<script setup lang="ts">
import AppTitle from './components/AppTitle.vue'
import AppBar from './components/AppBar.vue'
import MonacoEditor from './components/MonacoEditor.vue'
import ProgramInner from './components/ProgramInner.vue'
</script>

<style scoped>
.app-container {
  margin: 0 auto;
  width: 1200px;
}
</style>
