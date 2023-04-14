<template>
  <div class="controller-bar">
    <section class="controller-bar-section">
      <span
        v-for="name of programNameList"
        :key="name"
        :class="['button', 'program-button', name === programName && 'is-active']"
        @click="emit('program', name)"
      >
        {{ name }}
      </span>
    </section>
    <section class="controller-bar-section">
      <span
        v-if="step === 0"
        :class="['button', 'control-button', isDisabled && 'disabled']"
        @click="handleControl('run')"
        >Run</span
      >
      <span
        v-else
        :class="['button', 'control-button', isDisabled && 'disabled']"
        @click="handleControl('continue')"
        >Continue</span
      >
      <span v-if="isRunning" :class="['button', 'control-button']" @click="handleControl('stop')">Stop</span>
      <span
        v-else
        :class="['button', 'control-button', isDisabled && 'disabled']"
        @click="handleControl('step')"
        >Step</span
      >
    </section>
  </div>
</template>

<script setup lang="ts">
import { computed } from 'vue'
const props = defineProps({
  programNameList: {
    type: Array as PropType<string[]>,
    required: true
  },
  programName: {
    type: String as PropType<string>,
    required: true
  },
  isRunning: {
    type: Boolean as PropType<boolean>,
    required: true
  },
  step: {
    type: Number as PropType<number>,
    required: true
  }
})
const emit = defineEmits<{
  (e: 'program', name: string): void
  (e: 'run' | 'stop' | 'step' | 'continue'): void
}>()
const isDisabled = computed(() => props.isRunning)
const handleControl = (name: 'run' | 'stop' | 'step' | 'continue') => {
  if (!isDisabled.value || name === 'stop') {
    emit(name)
  }
}
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
  margin-right: var(--margin-xs);
  padding: 4px;
  color: var(--stroke-color);
  font-size: var(--font-size-m);
  border-radius: var(--border-radius-m);
  border: 1px solid var(--stroke-color);
  cursor: pointer;

  &:last-child {
    margin-right: 0;
  }

  &.disabled {
    cursor: not-allowed;
  }
}
.program-button {
  background-color: #fff;

  &:hover,
  &.is-active {
    background-color: var(--main-color);
  }
}
.control-button {
  background-color: var(--secondary-color);
  opacity: 0.85;

  &:hover:not(.disabled) {
    opacity: 1;
  }
}
</style>
