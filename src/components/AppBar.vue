<template>
  <div class="controller-bar">
    <div>
      <span
        v-for="program of programList"
        :key="program"
        class="button program-button"
      >
        {{ program }}
      </span>
    </div>
    <div>
      <span
        class="button control-button"
      >Run</span>
      <span
        class="button control-button"
      >Step</span>
      <span
        class="button control-button"
      >continue</span>
    </div>
  </div>
</template>

<script setup lang="ts">
defineProps<{
  programList: string[]
}>()
</script>

<style scoped>
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
  cursor: pointer;
  border: 1px solid #001858;

  &:last-child {
    margin-right: 0;
  }
}
.program-button {
  background-color: #f3d2c1;
}
.control-button {
  background-color: #8bd3dd;
}
</style>
