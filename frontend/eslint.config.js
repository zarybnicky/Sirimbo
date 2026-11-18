import nextVitals from 'eslint-config-next/core-web-vitals';
import nextTs from 'eslint-config-next/typescript';
import tailwind from 'eslint-plugin-tailwindcss';
import eslintPluginUnicorn from 'eslint-plugin-unicorn';
import { defineConfig, globalIgnores } from 'eslint/config';
import deMorgan from 'eslint-plugin-de-morgan';
import reactHookForm from 'oxlint-plugin-react-hook-form';
import { importX } from 'eslint-plugin-import-x';
import tsParser from '@typescript-eslint/parser';

import * as graphqlPlugin from '@graphql-eslint/eslint-plugin';
import { Kind, validate } from 'graphql';
import { queryDepthRule } from '@rozpisovnik/graphql/plugins/query-depth';
import { cacheKeyRule } from './graphql/cache-keys-rule.ts';

const frontendConfig = defineConfig([
  ...nextVitals,
  ...nextTs,
  tailwind.configs.recommended,
  eslintPluginUnicorn.configs.recommended,
  deMorgan.configs.recommended,
  importX.flatConfigs.recommended,
  importX.flatConfigs.typescript,
  reactHookForm.configs['react-compiler'],

  {
    files: ['**/*.{js,mjs,cjs,jsx}'],
    languageOptions: {
      parser: tsParser,
      parserOptions: { ecmaFeatures: { jsx: true } },
    },
  },

  {
    settings: {
      react: { version: '19' },
      tailwindcss: {
        cssConfigPath: './style/index.css',
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
          varsIgnorePattern: '^_|Types',
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
      'tailwindcss/no-custom-classname': [
        'warn',
        {
          whitelist: [
            'content',
            'tabular',
            'rbc-.*',
            'ck-content',
            'not-prose',
          ],
        },
      ],

      'import/no-anonymous-default-export': 'off',
      'import/no-unused-modules': 'off',
      'import/named': 'off',
      'import/namespace': 'off',
      'import/default': 'off',

      'import-x/no-named-as-default': 'off',
      'import-x/no-named-as-default-member': 'off',

      'unicorn/catch-error-name': 'off',
      'unicorn/prevent-abbreviations': 'off',
      'unicorn/no-null': 'off',
      'unicorn/filename-case': 'off',
      'unicorn/no-array-callback-reference': 'off',
      'unicorn/no-array-reduce': 'off',
      'unicorn/no-negated-condition': 'off',
      'unicorn/no-nested-ternary': 'off',
      'unicorn/no-abusive-eslint-disable': 'off',
      'unicorn/number-literal-case': 'off',
      'unicorn/switch-case-braces': 'off',
      'unicorn/prefer-module': 'warn',
      'unicorn/prefer-add-event-listener': 'warn',
      'unicorn/prefer-ternary': 'off',
      'unicorn/prefer-global-this': 'off',
    },
  },
]);

export default defineConfig([
  {
    files: ['**/*.{js,mjs,cjs,jsx,ts,tsx}'],
    extends: [frontendConfig],
  },
  {
    files: ['graphql/*.graphql'],
    languageOptions: {
      parser: graphqlPlugin.parser,
    },
    plugins: {
      '@graphql-eslint': graphqlPlugin,
      local: {
        rules: {
          'query-validation': {
            meta: { type: 'problem', schema: [] },
            create(context) {
              const schema = graphqlPlugin.requireGraphQLSchema(
                'local/query-validation',
                context,
              );
              const operations = graphqlPlugin.requireGraphQLOperations(
                'local/query-validation',
                context,
              );
              const reported = new Set();
              function check(node) {
                const definition = node.rawNode();
                const document = {
                  kind: Kind.DOCUMENT,
                  definitions: [definition, ...operations.getFragmentsInUse(definition)],
                };
                for (const error of validate(schema, document, [
                  queryDepthRule,
                  cacheKeyRule,
                ])) {
                  const source = error.nodes[0].loc;
                  if (source.source.body !== definition.loc.source.body) continue;
                  const key = `${source.start}:${error.message}`;
                  if (reported.has(key)) continue;
                  reported.add(key);
                  const location = error.locations[0];
                  context.report({
                    loc: { line: location.line, column: location.column - 1 },
                    message: error.message,
                  });
                }
              }
              return { OperationDefinition: check, FragmentDefinition: check };
            },
          },
        },
      },
    },
    rules: {
      ...graphqlPlugin.configs['operations-recommended'].rules,
      '@graphql-eslint/naming-convention': [
        'error',
        {
          ...graphqlPlugin.configs['operations-recommended'].rules['@graphql-eslint/naming-convention'][1],
          'DirectiveDefinition, Directive': { ignorePattern: '^_required$' },
        },
      ],
      '@graphql-eslint/known-type-names': 'error',
      '@graphql-eslint/no-one-place-fragments': 'off',
      '@graphql-eslint/require-selections': 'off',
      'local/query-validation': 'error',

      '@graphql-eslint/selection-set-depth': 'off',
    },
  },
  globalIgnores([
    'public/sw.js',
    'graphql/index.ts',
    'starlet/graphql/**',
    '.next/**',
    'out/**',
    'build/**',
    'next-env.d.ts',
  ]),
]);
