<template>
  <div class="controller-bar">
    <section class="controller-bar-section">
      <span
        v-for="name of programNameList"
        :key="name"
        :class="['button', 'program-button', (name === programName) && 'is-active']"
        @click="emit('program', name)"
      >
        {{ name }}
      </span>
    </section>
    <section class="controller-bar-section">
      <span
        class="button control-button"
        @click="emit('run')"
      >Run</span>
      <span
        class="button control-button"
        @click="emit('run')"
      >Step</span>
      <span
        class="button control-button"
        @click="emit('run')"
      >continue</span>
    </section>
  </div>
</template>

<script setup lang="ts">
defineProps({
  programNameList: {
    type: Array as PropType<string[]>,
    required: true
  },
  programName: {
    type: String as PropType<string>,
    required: true
  }
})
const emit = defineEmits<{
  (e: 'program', name: string): void
  (e: 'run'): void
  (e: 'step'): void
  (e: 'continue'): void
}>()
</script>

<style lang="scss" scoped>
.controller-bar {
  display: flex;
  align-items: center;
  justify-content: space-between;
  width: 100%;

  &-section {
    display: flex;
    align-items: center;
    justify-content: flex-start;
  }
}
.button {
  margin-right: 6px;
  padding: 4px;
  color: var(--stroke-color);
  font-size: 14px;
  border-radius: var(--border-radius-m);
  border: 1px solid var(--stroke-color);
  cursor: pointer;

  &:last-child {
    margin-right: 0;
  }
}
.program-button {
  background-color: var(--main-color);

  &:hover,
  &.is-active {
    background-color: var(--secondary-color);
  }
}
.control-button {
  background-color: var(--caution-color);
  opacity: 0.85;

  &:hover {
    opacity: 1;
  }
}
</style>
