import { LIST_TYPES, TYPES, sql, sqlValueWithCodec } from 'postgraphile/@dataplan/pg';
import { addPgTableCondition, extendSchema } from 'postgraphile/utils';

type SQL = ReturnType<typeof sql>;
type DateCondition = {
  since?: string | null;
  until?: string | null;
};

export function postingAccountPredicate(row: SQL, ids: readonly string[] | null) {
  if (ids == null) return null;
  return ids.length
    ? sql`${row}.account_id = any(${sqlValueWithCodec(ids, LIST_TYPES.bigint)})`
    : sql`false`;
}

export function transactionDatePredicate(row: SQL, value: DateCondition | null) {
  if (value == null) return null;
  const predicates: SQL[] = [];
  if (value.since != null)
    predicates.push(
      sql`${row}.effective_date >= ${sqlValueWithCodec(value.since, TYPES.timestamptz)}`,
    );
  if (value.until != null)
    predicates.push(
      sql`${row}.effective_date < ${sqlValueWithCodec(value.until, TYPES.timestamptz)}`,
    );
  return predicates.length ? sql`(${sql.join(predicates, ' and ')})` : null;
}

export function transactionSearchPredicate(row: SQL, value: string | null) {
  const search = value?.trim();
  if (!search) return null;
  const pattern = sqlValueWithCodec(
    `%${search.replaceAll(/[\\%_]/g, '\\$&')}%`,
    TYPES.text,
  );
  return sql`(
      ${row}.description ilike ${pattern}
      or exists (
        select 1 from public.payment payment where payment.id = ${row}.payment_id
          and (payment.variable_symbol ilike ${pattern} or payment.specific_symbol ilike ${pattern})
      )
    )`;
}

const plugins: GraphileConfig.Plugin[] = [
  extendSchema({
    typeDefs: `
      input TransactionDateCondition {
        """Inclusive effective date."""
        since: Datetime
        """Exclusive effective date."""
        until: Datetime
      }
    `,
  }),
  ...(['transaction', 'posting'] as const).map((tableName) =>
    addPgTableCondition(
      { schemaName: 'public', tableName },
      'accountIds',
      (build) => ({
        type: new build.graphql.GraphQLList(
          new build.graphql.GraphQLNonNull(build.getInputTypeByName('BigInt')),
        ),
      }),
      (value, { sqlTableAlias: row }) => {
        const predicate = postingAccountPredicate(
          tableName === 'posting' ? row : sql`p`,
          value as string[] | null,
        );
        return !predicate || tableName === 'posting'
          ? predicate
          : sql`exists (
        select 1 from public.posting p where p.transaction_id = ${row}.id and ${predicate}
      )`;
      },
    ),
  ),
  addPgTableCondition(
    { schemaName: 'public', tableName: 'transaction' },
    'sources',
    (build) => ({
      type: new build.graphql.GraphQLList(
        new build.graphql.GraphQLNonNull(build.getInputTypeByName('TransactionSource')),
      ),
    }),
    (value, { sqlTableAlias: row }) => {
      const sources = value as string[] | null;
      if (sources == null) return null;
      return sources.length
        ? sql`${row}.source = any(${sqlValueWithCodec(sources, LIST_TYPES.text)}::public.transaction_source[])`
        : sql`false`;
    },
  ),
  addPgTableCondition(
    { schemaName: 'public', tableName: 'transaction' },
    'date',
    (build) => ({ type: build.getInputTypeByName('TransactionDateCondition') }),
    (value, { sqlTableAlias }) =>
      transactionDatePredicate(sqlTableAlias, value as DateCondition | null),
  ),
  addPgTableCondition(
    { schemaName: 'public', tableName: 'transaction' },
    'search',
    (build) => ({ type: build.graphql.GraphQLString }),
    (value, { sqlTableAlias }) =>
      transactionSearchPredicate(sqlTableAlias, value as string | null),
  ),
];

export default plugins;
