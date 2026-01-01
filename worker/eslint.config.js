import { defineConfig } from 'eslint/config';
import deMorgan from 'eslint-plugin-de-morgan';
import tseslint from 'typescript-eslint';
import eslint from '@eslint/js';
import importPlugin from 'eslint-plugin-import';

// eslint-disable-next-line import/no-unused-modules
export default defineConfig([
  eslint.configs.recommended,
  tseslint.configs.recommended,
  deMorgan.configs.recommended,
  importPlugin.flatConfigs.recommended,

  {
    rules: {
      'no-extra-semi': 'off',
      'no-irregular-whitespace': 'off',
      'no-unused-vars': 'off',
      'prefer-const': 'warn',

      '@typescript-eslint/no-unused-vars': [
        'warn',
        {
          argsIgnorePattern: '^_',
          caughtErrors: 'all',
          caughtErrorsIgnorePattern: '^_',
          destructuredArrayIgnorePattern: '^_',
          varsIgnorePattern: '^_',
          ignoreRestSiblings: true,
        },
      ],

      '@typescript-eslint/no-var-requires': 'off',
      '@typescript-eslint/no-explicit-any': 'off',
      '@typescript-eslint/no-non-null-asserted-optional-chain': 'off',
      '@typescript-eslint/no-require-imports': 'off',
      '@typescript-eslint/triple-slash-reference': 'off',

      'import/no-unused-modules': [
        'warn',
        {
          unusedExports: true,
          ignoreUnusedTypeExports: false,
        },
      ],
      'import/no-named-as-default-member': 'off',
      'import/no-unresolved': 'off',
    },
  },
]);
