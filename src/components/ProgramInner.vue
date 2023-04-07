<template>
  <div class="program-inner">
    <section class="program-inner-block">
      <div class="program-inner-title">Output</div>
      <div class="program-inner-content">test</div>
    </section>
    <section class="program-inner-block">
      <div class="program-inner-title">Call Stack</div>
      <div class="program-inner-content">test</div>
    </section>
    <section class="program-inner-block">
      <div class="program-inner-title">Variables</div>
      <div class="program-inner-content">test</div>
    </section>
  </div>
</template>

<script setup lang="ts">

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
    padding: 2px 6px;
    width: 100%;
    box-sizing: border-box;
    border-bottom: 1px solid var(--stroke-color);
    color: var(--paragraph-color);
    background-color: var(--background-title-color);
  }

  &-content {
    padding: 6px;
    height: 150px;
    color: #000;
    cursor: text;
  }
}
</style>
