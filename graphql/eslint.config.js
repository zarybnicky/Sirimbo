import js from "@eslint/js";
import * as graphqlPlugin from '@graphql-eslint/eslint-plugin';
import { defineConfig } from "eslint/config";

export default defineConfig([
  {
    files: ['**/*.{js,jsx,ts,tsx,mjs,cjs}'],
    rules: js.configs.recommended.rules,
  },

  {
    files: ['**/*.graphql'],
    languageOptions: {
      parser: graphqlPlugin.parser,
    },
    plugins: {
      '@graphql-eslint': graphqlPlugin
    },
    rules: {
      ...graphqlPlugin.configs['operations-recommended'].rules,
      "@graphql-eslint/known-type-names": "error",
      "@graphql-eslint/no-one-place-fragments": "warn",

      "@graphql-eslint/selection-set-depth": ["error", {
        maxDepth: 11,
      }],
    },
  },
]);
