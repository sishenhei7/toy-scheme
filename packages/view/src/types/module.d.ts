declare module '*.vue' {
  import { DefineComponent } from 'vue'
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const Component: DefineComponent<{}, {}, any>
  export default Component
}
