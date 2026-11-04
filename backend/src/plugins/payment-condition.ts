import { LIST_TYPES, TYPES, pgClassExpression, type PgSelectSingleStep } from 'postgraphile/@dataplan/pg';
import { addPgTableCondition, extendSchema } from 'postgraphile/utils';

const plugins: GraphileConfig.Plugin[] = [
  extendSchema((build) => ({
    typeDefs: `
      extend type Account { balance: BigFloat }
      extend type PaymentDebtor { isUnpaid: Boolean }
    `,
    objects: {
      Account: {
        plans: {
          balance($account: PgSelectSingleStep) {
            const $balances = build.input.pgRegistry.pgResources.account_balances.find({
              id: $account.get('id'),
            });
            $balances.setInliningForbidden();
            return $balances.single().get('balance');
          },
        },
      },
      PaymentDebtor: {
        plans: {
          isUnpaid($debtor: PgSelectSingleStep) {
            const $payment = build.input.pgRegistry.pgResources.payment.get({
              id: $debtor.get('payment_id'),
            });
            return pgClassExpression($payment, TYPES.boolean)`${$payment.get('status')} = 'unpaid'`;
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
      description: 'Checks for equality with the object’s `isUnpaid` field.',
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
