import { costLimitRule } from '@escape.tech/graphql-armor-cost-limit';
import { maxDepthRule } from '@escape.tech/graphql-armor-max-depth';
import {
  GraphQLError,
  Kind,
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
  type ValidationContext,
} from 'graphql';

const rejection = {
  propagateOnRejection: false,
  onReject: [
    (context: ValidationContext | null, error: GraphQLError) =>
      context?.reportError(error),
  ],
};
const queryCostRule = costLimitRule({
  ...rejection,
  maxCost: 50000,
  flattenFragments: true,
});

const queryValidationRules = [
  maxDepthRule({ ...rejection, n: 12, flattenFragments: true }),
  queryCostRule,
];

function checkQueryLimits(
  schema: GraphQLSchema,
  document: DocumentNode,
  variables: Record<string, unknown> = {},
  operationName?: string | null,
): readonly GraphQLError[] {
  const operation = getOperationAST(document, operationName);
  if (!operation) return [new GraphQLError('Select a single operation.')];
  const values = getVariableValues(
    schema,
    operation.variableDefinitions ?? [],
    variables,
  );
  if (values.errors) return values.errors;
  const selected = separateOperations(document)[operation.name?.value ?? ''];
  const typeInfo = new TypeInfo(schema);
  const errors: GraphQLError[] = [];
  let federation = false;
  const withPaginationValues = visit(
    selected,
    visitWithTypeInfo(typeInfo, {
      Field(node) {
        const field = typeInfo.getFieldDef();
        if (!field?.extensions.federationData) return;
        federation = true;
        if (!field.extensions.boundedPagination) return;
        const args = getArgumentValues(field, node, values.coerced);
        if (
          (args.first == null && args.last == null) ||
          [args.first, args.last].some(
            (value) =>
              value != null && (typeof value !== 'number' || value < 0 || value > 100),
          )
        ) {
          errors.push(
            new GraphQLError(
              `${typeInfo.getParentType()?.name}.${field.name} requires first or last between 0 and 100.`,
              { nodes: node },
            ),
          );
        }
      },
      Argument(node) {
        if (
          ['first', 'last'].includes(node.name.value) &&
          node.value.kind === Kind.VARIABLE
        ) {
          const value = values.coerced[node.value.name.value];
          if (typeof value === 'number')
            return { ...node, value: { kind: Kind.INT, value: String(value) } };
        }
        return undefined;
      },
    }),
  );
  if (!federation) return [];
  if (errors.length) return errors;
  return validate(schema, withPaginationValues, queryValidationRules);
}

export const QueryControlsPlugin: GraphileConfig.Plugin = {
  name: 'QueryControlsPlugin',
  schema: {
    hooks: {
      GraphQLObjectType_fields_field(field, _build, { scope }) {
        const schema = scope.pgFieldResource?.codec.extensions?.pg?.schemaName;
        if (schema === 'federated') {
          return {
            ...field,
            extensions: {
              ...field.extensions,
              federationData: true,
              boundedPagination: !!scope.isPgFieldConnection,
            },
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
