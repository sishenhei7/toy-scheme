import { describe, it, expect } from 'vitest'

import { mount } from '@vue/test-utils'
import HelloWorld from '../AppTitle.vue'

describe('AppTitle', () => {
  it('renders properly', () => {
    const wrapper = mount(HelloWorld, { props: { title: 'Toy-Scheme' } })
    expect(wrapper.text()).toContain('Toy-Scheme')
  })
})
