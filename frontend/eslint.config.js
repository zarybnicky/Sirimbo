import nextVitals from 'eslint-config-next/core-web-vitals';
import nextTs from 'eslint-config-next/typescript';
import tailwind from 'eslint-plugin-tailwindcss';
import eslintPluginUnicorn from 'eslint-plugin-unicorn';
import { defineConfig, globalIgnores } from 'eslint/config';
import deMorgan from 'eslint-plugin-de-morgan';
import { fixupPluginRules } from '@eslint/compat';
import reactHookForm from 'eslint-plugin-react-hook-form';

// eslint-disable-next-line import/no-unused-modules
export default defineConfig([
  ...nextVitals,
  ...nextTs,
  ...tailwind.configs['flat/recommended'],
  eslintPluginUnicorn.configs.recommended,
  deMorgan.configs.recommended,

  {
    plugins: {
      'react-hook-form': fixupPluginRules(reactHookForm),
    },
    rules: {
      ...reactHookForm.configs.recommended.rules,
    },
  },

  {
    settings: {
      tailwindcss: {
        callees: ['classNames', 'ctl', 'cn'],
        cssFilesRefreshRate: 5_000_000,
      },
    },
    rules: {
      'no-extra-semi': 'off',
      'no-irregular-whitespace': 'off',
      'no-unused-vars': 'off',
      'prefer-const': 'warn',

      '@typescript-eslint/no-unused-vars': [
        'warn',
        {
          args: 'all',
          argsIgnorePattern: '^_',
          caughtErrors: 'all',
          caughtErrorsIgnorePattern: '^_',
          destructuredArrayIgnorePattern: '^_',
          varsIgnorePattern: '^_',
          ignoreRestSiblings: true,
        },
      ],

      'react-hooks/static-components': 'off',
      'react/function-component-definition': [
        'error',
        {
          namedComponents: 'function-declaration',
          unnamedComponents: 'arrow-function',
        },
      ],

      '@typescript-eslint/no-var-requires': 'off',
      '@typescript-eslint/no-explicit-any': 'off',
      '@typescript-eslint/no-non-null-asserted-optional-chain': 'off',
      '@typescript-eslint/no-require-imports': 'off',
      '@typescript-eslint/triple-slash-reference': 'off',
      'tailwindcss/classnames-order': 'off',

      'import/no-unused-modules': [
        'warn',
        {
          unusedExports: true,
          ignoreExports: ['pages/**'],
          ignoreUnusedTypeExports: false,
        },
      ],

      'unicorn/prevent-abbreviations': 'off',
      'unicorn/no-null': 'off',
      'unicorn/filename-case': 'off',
      'unicorn/no-array-callback-reference': 'off',
      'unicorn/no-array-reduce': 'off',
      'unicorn/no-negated-condition': 'off',
      'unicorn/no-nested-ternary': 'off',
      'unicorn/no-abusive-eslint-disable': 'off',
      'unicorn/switch-case-braces': 'off',
      'unicorn/prefer-module': 'warn',
      'unicorn/prefer-add-event-listener': 'warn',
      'unicorn/prefer-ternary': 'off',
      'unicorn/prefer-global-this': 'off',
      'import/named': 'off',
      'import/namespace': 'off',
      'import/default': 'off',
    },
  },

  globalIgnores([
    'public/sw.js',
    'graphql/index.ts',
    '.next/**',
    'out/**',
    'build/**',
    'next-env.d.ts',
  ]),
]);
