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
    width: 32%;
    height: 100%;
    border: 1px solid #001858;
    border-radius: 8px;
    background-color: #fff;
    overflow: hidden;
  }

  &-title {
    padding: 2px 6px;
    width: 100%;
    box-sizing: border-box;
    border-bottom: 1px solid #001858;
    color: #172c66;
    background-color: #fec7d7;
  }

  &-content {
    padding: 6px;
    height: 150px;
    color: #000;
    cursor: text;
  }
}
</style>
