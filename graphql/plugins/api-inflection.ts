export const ApiInflectionPlugin: GraphileConfig.Plugin = {
  name: 'ApiInflectionPlugin',
  inflection: {
    replace: {
      _attributeName(previous, _options, details) {
        return previous!({ ...details, skipRowId: true });
      },
      enumValue(previous, _options, value, codec) {
        return this.coerceToGraphQLName(this.constantCase(previous!(value, codec)));
      },
      orderByType(_previous, _options, typeName) {
        return this.upperCamelCase(`${this.pluralize(typeName)}-order-by`);
      },
    },
  },
};
