module.exports = {
  overrides: [
    {
      files: ["*.graphql"],
      extends: "plugin:@graphql-eslint/operations-recommended",
      rules: {
        "@graphql-eslint/known-type-names": "error",
        "@graphql-eslint/no-one-place-fragments": "warn",
      },
      parserOptions: {
        operations: "./**/*.graphql",
        schema: "../schema.graphql"
      }
    }
  ]
};
