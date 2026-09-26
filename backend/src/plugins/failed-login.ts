import { getOperationAST, Kind, valueFromASTUntyped, type FieldNode } from 'graphql';
import { pool } from '../db.ts';

export const FailedLoginPlugin: GraphileConfig.Plugin = {
  name: 'FailedLoginPlugin',
  grafast: {
    middleware: {
      async execute(next, { args }) {
        const result = await next();
        if (!('errors' in result) || !result.errors?.some((x) => x.message === 'INVALID_CREDENTIALS')) {
          return result;
        }

        const operation = getOperationAST(args.document, args.operationName);
        const field = operation?.operation === 'mutation'
          ? operation.selectionSet.selections.find(
              (selection): selection is FieldNode =>
                selection.kind === Kind.FIELD && ['login', 'otpLogin'].includes(selection.name.value),
            )
          : undefined;
        const inputArgument = field?.arguments?.find((argument) => argument.name.value === 'input');
        const input = inputArgument
          ? valueFromASTUntyped(inputArgument.value, args.variableValues)
          : undefined;
        if (!field || typeof input !== 'object' || input === null) return result;

        const method = field.name.value === 'login' ? 'password' : 'otp';
        const identifier = method === 'password' && 'login' in input
          ? input.login
          : 'token' in input
            ? input.token
            : undefined;
        if (typeof identifier !== 'string') return result;

        const pgSettings = (args.contextValue as { pgSettings?: Record<string, string> })?.pgSettings;
        try {
          await pool.query(
            `insert into security_event (tenant_id, user_id, actor_user_id, kind, method)
             values (
               $1,
               case when $2::security_event_method = 'password' then (
                 select id from users
                 where lower(u_login) = lower(trim($3)) or lower(u_email) = lower(trim($3))
                 limit 1
               ) else (
                 select user_id from otp_token where access_token::text = $3
               ) end,
               $4,
               'login_failed',
               $2
             )`,
            [
              pgSettings?.['jwt.claims.tenant_id'] ?? '1',
              method,
              identifier,
              pgSettings?.['jwt.claims.user_id'] || null,
            ],
          );
        } catch (error) {
          console.error('Failed to record login failure', error);
        }
        return result;
      },
    },
  },
};
