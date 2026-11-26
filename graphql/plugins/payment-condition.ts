import { TYPES, type PgSelectSingleStep } from 'postgraphile/@dataplan/pg';
import { addPgTableCondition, extendSchema } from 'postgraphile/utils';

const plugins: GraphileConfig.Plugin[] = [
  extendSchema((build) => ({
    typeDefs: `
      extend type Account { balance: BigFloat }
    `,
    objects: {
      Account: {
        plans: {
          balance($account: PgSelectSingleStep) {
            const $balances = build.input.pgRegistry.pgResources.account_balances!.find({
              id: $account.get('id'),
            });
            $balances.setInliningForbidden();
            return $balances.single().get('balance');
          },
        },
      },
    },
  }), 'PaymentFieldsPlugin'),
  addPgTableCondition(
    { schemaName: 'public', tableName: 'payment_debtor' },
    'isUnpaid',
    (build) => ({
      type: build.graphql.GraphQLBoolean,
      description: 'Whether the related payment is unpaid. Null matches an inaccessible payment.',
    }),
    (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) => value == null
      ? sql`not exists (select from public.payment p where p.id = ${row}.payment_id)`
      : sql`${row}.payment_id in (
          select id from public.payment
          where (status = 'unpaid') = ${sqlValueWithCodec(value, TYPES.boolean)}
        )`,
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
