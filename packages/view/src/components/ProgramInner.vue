<template>
  <div class="program-inner">
    <section class="program-inner-block">
      <h3 class="program-inner-title">Output</h3>
      <div ref="outputRef" class="program-inner-content">
        <template v-for="item in output">
          <p v-if="item" :key="item" class="program-inner-row">{{ item }}</p>
        </template>
      </div>
    </section>
    <section class="program-inner-block">
      <h3 class="program-inner-title">Call Stack</h3>
      <div class="program-inner-content">
        <template v-for="item in callStack">
          <p v-if="item" :key="item" class="program-inner-row">{{ item }}</p>
        </template>
        <p v-if="showTips" class="program-inner-row">Not paused.</p>
      </div>
    </section>
    <section class="program-inner-block">
      <h3 class="program-inner-title">Scope</h3>
      <div class="program-inner-content">
        <template v-for="item in varScope">
          <p v-if="item" :key="item" class="program-inner-row">{{ item }}</p>
        </template>
        <p v-if="showTips" class="program-inner-row">Not paused.</p>
      </div>
    </section>
  </div>
</template>

<script setup lang="ts">
import { type PropType, watch, ref, nextTick } from 'vue'

const props = defineProps({
  output: {
    type: Array as PropType<string[]>,
    required: true
  },
  callStack: {
    type: Array as PropType<string[]>,
    required: true
  },
  varScope: {
    type: Array as PropType<string[]>,
    required: true
  },
  showTips: {
    type: Boolean as PropType<boolean>,
    required: true
  }
})

const outputRef = ref<HTMLElement>()
watch(
  () => props.output.length,
  async () => {
    await nextTick()
    outputRef.value!.scrollTop = outputRef.value!.scrollHeight
  }
)
</script>

<style lang="scss" scoped>
.program-inner {
  display: flex;
  align-items: center;
  justify-content: space-between;
  width: 100%;

  &-block {
    width: calc((100% - var(--margin-m) * 2) / 3);
    height: 100%;
    box-sizing: border-box;
    border: 1px solid var(--stroke-color);
    border-radius: var(--border-radius-l);
    background-color: #fff;
    overflow: hidden;
  }

  &-title {
    margin: 0;
    padding: 2px 10px;
    width: 100%;
    font-weight: normal;
    box-sizing: border-box;
    border-bottom: 1px solid var(--stroke-color);
    color: var(--paragraph-color);
    background-color: var(--background-title-color);
  }

  &-content {
    padding: 0 6px;
    height: 150px;
    color: #000;
    cursor: text;
    overflow-y: auto;
    white-space: pre-line;
  }

  &-row {
    margin: 0;
    padding: 4px;
    word-break: break-word;
    hyphens: auto;
    border-bottom: 1px dashed var(--stroke-color-secondary);
  }
}
</style>
