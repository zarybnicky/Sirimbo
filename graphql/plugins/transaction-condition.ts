import { sql, sqlValueWithCodec, TYPES } from 'postgraphile/@dataplan/pg';
import { addPgTableCondition, extendSchema } from 'postgraphile/utils';
import { SQL } from 'postgraphile/pg-sql2';

type DateCondition = {
  since?: string | null;
  until?: string | null;
};

function transactionDatePredicate(row: SQL, value: DateCondition | null) {
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
    (value, { sqlTableAlias }) => {
      const search = (value as string | null)?.trim();
      if (!search) return null;
      return sql`${sqlTableAlias}.id in (select * from app_private.transaction_search_ids(${sqlValueWithCodec(search, TYPES.text)}))`;
    },
  ),
];

export default plugins;
