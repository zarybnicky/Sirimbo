import assert from 'node:assert';

const legacyFields: Record<string, Record<string, string>> = {
  // Map old field names to current fields. Aliases are deprecated automatically.
  // Query: { eventInstance: 'event' },
};

export const LegacyRelationAliasesPlugin: GraphileConfig.Plugin = {
  name: 'LegacyRelationAliasesPlugin',
  schema: {
    hooks: {
      GraphQLObjectType_fields: {
        after: ['PgRelationsPlugin', 'PgMutationUpdateDeletePlugin', 'PgCustomTypeFieldPlugin', 'PersonCouplesPlugin'],
        callback(fields, build, { Self }) {
          const { GraphQLInputObjectType, GraphQLNonNull, getNamedType, isInputObjectType } =
            build.graphql;
          for (const [legacy, canonical] of Object.entries(legacyFields[Self.name] ?? {})) {
            if (!fields[canonical]) continue;
            const field = fields[canonical];
            let args = field.args;
            if (Self.name === 'Mutation') {
              const input = getNamedType(args!.input!.type);
              assert(isInputObjectType(input));
              // Keep variable declarations using the old input name, including input-field plans.
              args = {
                ...args,
                input: {
                  ...args!.input,
                  type: new GraphQLNonNull(
                    new GraphQLInputObjectType({
                      ...input.toConfig(),
                      name: `${build.inflection.upperCamelCase(legacy)}Input`,
                    }),
                  ),
                },
              };
            }
            fields = {
              ...fields,
              [legacy]: {
                ...field,
                args,
                deprecationReason: `Use ${canonical}.`,
              },
            };
          }
          return fields;
        },
      },
    },
  },
};
