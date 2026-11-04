import { pgClassExpression, TYPES, type PgSelectSingleStep } from 'postgraphile/@dataplan/pg';
import { extendSchema } from 'postgraphile/utils';

export const AccessCredentialFieldsPlugin = extendSchema({
  typeDefs: `extend type AccessCredential { isAllowed: Boolean }`,
  objects: {
    AccessCredential: {
      plans: {
        isAllowed($credential: PgSelectSingleStep) {
          return pgClassExpression($credential, TYPES.boolean)`
            ${$credential.get('valid_range')} @> now()
            and ${$credential.get('person_id')} in (
              select person_id from public.current_tenant_membership
              union all select person_id from public.current_tenant_trainer
              union all select person_id from public.current_tenant_administrator
            )`;
        },
      },
    },
  },
}, 'AccessCredentialFieldsPlugin');
