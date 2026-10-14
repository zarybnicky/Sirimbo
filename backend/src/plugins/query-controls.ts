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
  maxCost: 150000, // temporary, should be 50_000
  flattenFragments: true,
});

const queryValidationRules = [
  maxDepthRule({ ...rejection, n: 12, flattenFragments: true }),
  queryCostRule,
];

// Separate from the existing cost limit: stop collection multiplication without
// charging every scalar in a complete result table for the estimated row count.
const collectionCostRule = costLimitRule({
  ...rejection,
  maxCost: 150000, // temporary, should be 50_000
  flattenFragments: true,
  scalarCost: 0,
  objectCost: 1,
  depthCostFactor: 1,
});

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
  const queryErrors = validate(schema, withPaginationValues, queryValidationRules);
  if (queryErrors.length) return queryErrors;

  const collectionInfo = new TypeInfo(schema);
  const withCollectionSizes = visit(
    withPaginationValues,
    visitWithTypeInfo(collectionInfo, {
      Field(node) {
        const field = collectionInfo.getFieldDef();
        if (!field?.extensions.collectionCost) return;
        const args = getArgumentValues(field, node, values.coerced);
        if (args.first != null || args.last != null) return;
        return {
          ...node,
          arguments: [
            ...(node.arguments ?? []).filter(
              (arg) => !['first', 'last'].includes(arg.name.value),
            ),
            {
              kind: Kind.ARGUMENT,
              name: { kind: Kind.NAME, value: 'first' },
              value: { kind: Kind.INT, value: String(field.extensions.collectionCost) },
            },
          ],
        };
      },
    }),
  );
  return validate(schema, withCollectionSizes, [collectionCostRule]);
}

export const QueryControlsPlugin: GraphileConfig.Plugin = {
  name: 'QueryControlsPlugin',
  schema: {
    hooks: {
      GraphQLObjectType_fields_field(field, _build, { scope }) {
        const table = (scope.pgFieldCodec ?? scope.pgFieldResource?.codec)?.extensions
          ?.pg;
        const collection = scope.isPgFieldConnection || scope.isPgFieldSimpleCollection;
        if (table?.schemaName === 'federated' || collection) {
          return {
            ...field,
            extensions: {
              ...field.extensions,
              federationData: table?.schemaName === 'federated',
              boundedPagination:
                table?.schemaName === 'federated' && !!scope.isPgFieldConnection,
              collectionCost: collection
                ? table?.name === 'judge_score'
                  ? 10000
                  : 50
                : undefined,
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
