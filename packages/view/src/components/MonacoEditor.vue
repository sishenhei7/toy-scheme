<template>
  <div class="editor-container">
    <div id="monaco-editor" ref="monacoEditorRef"></div>
  </div>
</template>

<script setup lang="ts">
import { type PropType, onMounted, onBeforeUnmount, ref, watch } from 'vue'
import * as monaco from 'monaco-editor'
import 'monaco-editor/esm/vs/basic-languages/scheme/scheme.contribution'
import EditorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker'

self.MonacoEnvironment = {
  getWorker(_: string, _label: string) {
    return new EditorWorker()
  }
}

const props = defineProps({
  modelValue: {
    type: String as PropType<string>,
    default: ''
  },
  programName: {
    type: String as PropType<string>,
    default: ''
  },
  highlightRange: {
    type: Object as PropType<[number, number, number, number] | null>,
    default: null
  }
})

const emit = defineEmits<{
  (e: 'update:modelValue', name: string): void
}>()

let editor: monaco.editor.IStandaloneCodeEditor
let highlight: monaco.editor.IEditorDecorationsCollection
const monacoEditorRef = ref()

onMounted(() => {
  editor = monaco.editor.create(monacoEditorRef.value, {
    scrollBeyondLastLine: false,
    minimap: { enabled: false },
    automaticLayout: false,
    language: 'scheme',
    value: props.modelValue
  })

  editor.onDidChangeModelContent(() => {
    const value = editor.getValue()
    // 把 \\n 换回来
    emit('update:modelValue', value.replace(/\\n"/g, '\n"'))
  })
})

onBeforeUnmount(() => {
  editor.dispose()
})

watch(
  () => props.programName,
  () => {
    // \n 在编辑器里面会被解释为换行符
    editor?.setValue(props.modelValue.replace(/\n"/g, '\\n"'))
  }
)

watch(
  () => props.highlightRange,
  (newValue) => {
    highlight?.clear()

    if (newValue) {
      highlight = editor?.createDecorationsCollection([
        {
          range: new monaco.Range(...newValue),
          options: {
            inlineClassName: 'monaco-editor-highlight'
          }
        }
      ])
    }
  }
)
</script>

<style lang="scss" scoped>
.editor-container {
  width: 100%;
  height: 350px;
  border: 1px solid var(--stroke-color);
  border-radius: var(--border-radius-l);
  overflow: hidden;
}
#monaco-editor {
  width: 100%;
  height: 100%;
  min-height: 0;
  min-width: 0;
}
</style>
