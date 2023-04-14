// @ts-check
/* eslint-env node */
/* eslint-disable node/no-unpublished-require, @typescript-eslint/no-var-requires */
require('@rushstack/eslint-patch/modern-module-resolution')
const { defineConfig } = require('eslint-define-config')

module.exports = defineConfig({
  root: true,
  extends: [
    'eslint:recommended',
    'plugin:node/recommended',
    'plugin:@typescript-eslint/recommended',
    'plugin:regexp/recommended'
  ],
  plugins: ['import', 'regexp'],
  parser: '@typescript-eslint/parser',
  parserOptions: {
    sourceType: 'module',
    ecmaVersion: 2021
  },
  rules: {
    'node/no-unpublished-import': 'off',
    '@typescript-eslint/ban-types': 'off', // TODO: we should turn this on in a new PR
    '@typescript-eslint/no-non-null-assertion': 'off', // maybe we should turn this on in a new PR
    '@typescript-eslint/no-this-alias': 'off', // maybe we should turn this on in a new PR
    '@typescript-eslint/no-empty-function': 'off', // maybe we should turn this on in a new PR
    'node/no-missing-import': 'off',
    'node/no-unsupported-features/es-syntax': 'off',
    '@typescript-eslint/no-unused-vars': ['error', { argsIgnorePattern: '^_' }],
    'node/no-unsupported-features/es-builtins': [
      'error',
      {
        version: '^14.18.0 || >=16.0.0'
      }
    ]
  },
  overrides: [
    {
      files: ['packages/view/**'],
      parserOptions: {
        ecmaVersion: 'latest'
      },
      extends: [
        'plugin:vue/vue3-essential',
        'eslint:recommended',
        '@vue/eslint-config-typescript',
        '@vue/eslint-config-prettier'
      ]
    }
  ]
})
