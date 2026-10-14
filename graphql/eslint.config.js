import * as graphqlPlugin from '@graphql-eslint/eslint-plugin';
import { defineConfig } from "eslint/config";
import { Kind, validate } from 'graphql';
import { queryDepthRule } from '../backend/src/plugins/query-depth.ts';

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
          'query-depth': {
            meta: { type: 'problem', schema: [] },
            create(context) {
              const schema = graphqlPlugin.requireGraphQLSchema('local/query-depth', context);
              const operations = graphqlPlugin.requireGraphQLOperations('local/query-depth', context);
              return {
                OperationDefinition(node) {
                  const operation = node.rawNode();
                  const document = {
                    kind: Kind.DOCUMENT,
                    definitions: [operation, ...operations.getFragmentsInUse(operation)],
                  };
                  for (const error of validate(schema, document, [queryDepthRule])) {
                    context.report({ node, message: error.message });
                  }
                },
              };
            },
          },
        },
      },
    },
    rules: {
      ...graphqlPlugin.configs['operations-recommended'].rules,
      "@graphql-eslint/known-type-names": "error",
      "@graphql-eslint/no-one-place-fragments": "off",

      "@graphql-eslint/selection-set-depth": "off",
      "local/query-depth": "error",
    },
  },
]);
