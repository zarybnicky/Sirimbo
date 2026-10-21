export const ApiInflectionPlugin: GraphileConfig.Plugin = {
  name: 'ApiInflectionPlugin',
  inflection: {
    replace: {
      _schemaPrefix(_previous, _options, { serviceName }) {
        return serviceName === 'main' ? '' : `${serviceName}_`;
      },
      enumValue(previous, _options, value, codec) {
        return this.coerceToGraphQLName(this.constantCase(previous!(value, codec)));
      },
      orderByType(_previous, _options, typeName) {
        return this.upperCamelCase(`${this.pluralize(typeName)}-order-by`);
      },
      functionMutationResultFieldName(_previous, _options, details) {
        const { resource, returnGraphQLTypeName } = details;
        if (resource.extensions?.tags?.resultFieldName) {
          return resource.extensions.tags.resultFieldName;
        }
        const typeName = returnGraphQLTypeName === 'Int' ? 'integer' : returnGraphQLTypeName;
        const name = this.camelCase(
          resource.extensions?.singleOutputParameterName ??
            (resource.codec.isAnonymous ? 'result' : typeName),
        );
        return !resource.isUnique || resource.codec.arrayOfCodec ? this.pluralize(name) : name;
      },
    },
  },
};
