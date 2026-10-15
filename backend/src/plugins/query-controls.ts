import {
  GraphQLError,
  validate,
  TypeInfo,
  getArgumentValues,
  getOperationAST,
  getVariableValues,
  separateOperations,
  visit,
  visitWithTypeInfo,
  type DocumentNode,
  type GraphQLSchema,
} from 'graphql';
import { queryDepthRule } from './query-depth.ts';

function checkQueryLimits(
  schema: GraphQLSchema,
  document: DocumentNode,
  variables: Record<string, unknown> = {},
  operationName?: string | null,
): readonly GraphQLError[] {
  const operation = getOperationAST(document, operationName);
  if (!operation) return [new GraphQLError('Select a single operation.')];
  const selected = separateOperations(document)[operation.name?.value ?? ''];
  const errors = [...validate(schema, selected, [queryDepthRule])];
  if (errors.length) return errors;
  const values = getVariableValues(
    schema,
    operation.variableDefinitions ?? [],
    variables,
  );
  if (values.errors) return values.errors;
  const typeInfo = new TypeInfo(schema);
  visit(selected, visitWithTypeInfo(typeInfo, {
    Field(node) {
      const field = typeInfo.getFieldDef();
      if (!field?.extensions.boundedPagination) return;
      const args = getArgumentValues(field, node, values.coerced);
      if (
        (args.first == null && args.last == null) ||
        [args.first, args.last].some(
          (value) => value != null && (typeof value !== 'number' || value < 0 || value > 300),
        )
      ) {
        errors.push(new GraphQLError(
          `${typeInfo.getParentType()?.name}.${field.name} requires first or last between 0 and 300.`,
          { nodes: node },
        ));
      }
    },
  }));
  return errors;
}

export const QueryControlsPlugin: GraphileConfig.Plugin = {
  name: 'QueryControlsPlugin',
  schema: {
    hooks: {
      GraphQLObjectType_fields_field(field, _build, { scope }) {
        const table = (scope.pgFieldCodec ?? scope.pgFieldResource?.codec)?.extensions
          ?.pg;
        if (table?.schemaName === 'federated' && scope.isPgFieldConnection) {
          return {
            ...field,
            extensions: { ...field.extensions, boundedPagination: true },
          };
        }
        return field;
      },
    },
  },
  grafast: {
    middleware: {
      execute(next, { args }) {
        const errors = checkQueryLimits(
          args.schema,
          args.document,
          args.variableValues ?? {},
          args.operationName,
        );
        return errors.length ? { errors } : next();
      },
    },
  },
};
