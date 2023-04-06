<template>
  <div class="controller-bar">
    <div>
      <span
        v-for="name of programNameList"
        :key="name"
        :class="['button', 'program-button', (name === programName) && 'is-active']"
        @click="emit('program', name)"
      >
        {{ name }}
      </span>
    </div>
    <div>
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
    </div>
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
  width: 100%
}
.button {
  margin-right: 6px;
  padding: 4px;
  color: #001858;
  font-size: 14px;
  border-radius: 6px;
  border: 1px solid #001858;
  cursor: pointer;

  &:last-child {
    margin-right: 0;
  }
}
.program-button {
  background-color: #f3d2c1;

  &:hover {
    background-color: #8bd3dd;
  }

  &.is-active {
    background-color: #8bd3dd;
  }
}
.control-button {
  background-color: #f582ae;
  opacity: 0.85;

  &:hover {
    opacity: 1;
  }
}
</style>
