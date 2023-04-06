<template>
  <div class="editor-container">
    <div id="monaco-editor" ref="monacoEditorRef"></div>
  </div>
</template>

<script setup lang="ts">
import { onMounted, ref } from 'vue'
import * as monaco from 'monaco-editor'
import 'monaco-editor/esm/vs/basic-languages/scheme/scheme.contribution'
import EditorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker'
import program from '../scheme/programs/lazy-generator'

self.MonacoEnvironment = {
  getWorker(_: string, _label: string) {
    return new EditorWorker()
  }
}

const monacoEditorRef = ref()
onMounted(() => {
  const editor = monaco.editor.create(monacoEditorRef.value, {
    scrollBeyondLastLine: false,
    minimap: { enabled: false },
    automaticLayout: false,
    language: 'scheme',
    value: program
  })

  let highlight = editor.createDecorationsCollection([
    {
      range: new monaco.Range(3, 3, 3, 10),
      options: {
        inlineClassName: "monaco-editor-highlight"
      }
    }
  ])

  setTimeout(() => {
    highlight.clear()
    highlight = editor.createDecorationsCollection([
      {
        range: new monaco.Range(5, 3, 6, 10),
        options: {
          inlineClassName: "monaco-editor-highlight"
        }
      }
    ])
  }, 3000)

  // monaco.languages.registerDocumentHighlightProvider.
  // monaco.editor.setModelMarkers(monacoInstance.value.getModel(), 'highlight', [])
})
</script>

<style scoped>
.editor-container {
  width: 1200px;
  height: 300px;
  border: 1px solid #ced4da;
}
#monaco-editor {
  width: 100%;
  height: 100%;
  min-height: 0;
  min-width: 0;
}
</style>
