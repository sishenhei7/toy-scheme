import type { PropType as VuePropType } from 'vue';

declare global {
  type PropType<T> = VuePropType<T>;
}