import * as graphqlPlugin from '@graphql-eslint/eslint-plugin';
import { defineConfig } from 'eslint/config';
import { Kind, validate } from 'graphql';
import { queryDepthRule } from '../backend/src/plugins/query-depth.ts';
import { cacheKeyRule } from './cache-keys-rule.ts';

export default defineConfig([
  {
    files: ['**/*.graphql'],
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
      '@graphql-eslint/known-type-names': 'error',
      '@graphql-eslint/no-one-place-fragments': 'off',
      '@graphql-eslint/require-selections': 'off',
      'local/query-validation': 'error',

      '@graphql-eslint/selection-set-depth': 'off',
    },
  },
]);
