import { LIST_TYPES, TYPES } from 'postgraphile/@dataplan/pg';
import { addPgTableCondition } from 'postgraphile/utils';

const plugins: GraphileConfig.Plugin[] = [
  addPgTableCondition(
    { schemaName: 'public', tableName: 'payment' },
    'personIds',
    (build) => ({
      type: new build.graphql.GraphQLList(
        new build.graphql.GraphQLNonNull(build.getInputTypeByName('BigInt')),
      ),
    }),
    (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) => {
      const ids = value as string[] | null;
      if (ids == null) return null;
      return ids.length
        ? sql`exists (
          select 1 from public.payment_debtor d
          where d.payment_id = ${row}.id
            and d.person_id = any(${sqlValueWithCodec(ids, LIST_TYPES.bigint)})
        )`
        : sql`false`;
    },
  ),
  addPgTableCondition(
    { schemaName: 'public', tableName: 'payment' },
    'search',
    (build) => ({ type: build.graphql.GraphQLString }),
    (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) => {
      const search = (value as string | null)?.trim();
      if (!search) return null;
      return sql`${row}.id in (
        select * from app_private.payment_search_ids(${sqlValueWithCodec(search, TYPES.text)})
      )`;
    },
  ),
];

export default plugins;
