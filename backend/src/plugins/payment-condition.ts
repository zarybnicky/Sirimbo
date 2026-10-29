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
      const pattern = sqlValueWithCodec(
        '%' + search.replaceAll(/[\\%_]/g, '\\$&') + '%',
        TYPES.text,
      );
      return sql`(
        ${row}.id::text ilike ${pattern}
        or ${row}.variable_symbol ilike ${pattern}
        or ${row}.specific_symbol ilike ${pattern}
        or exists (
          select 1 from public.event e where e.id = ${row}.event_id and e.name ilike ${pattern}
        )
        or exists (
          select 1 from public.cohort_subscription cs
          join public.cohort c on c.id = cs.cohort_id
          where cs.id = ${row}.cohort_subscription_id and c.name ilike ${pattern}
        )
        or exists (
          select 1 from public.payment_debtor d
          join public.person p on p.id = d.person_id
          where d.payment_id = ${row}.id
            and concat_ws(' ', p.first_name, p.last_name) ilike ${pattern}
        )
      )`;
    },
  ),
];

export default plugins;
