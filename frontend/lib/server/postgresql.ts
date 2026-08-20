import 'server-only';

import { SESSION_COOKIE } from '@/lib/session-cookies';
import { getRequestTenant } from '@/lib/server/tenant';
import type { PreparedQuery } from '@pgtyped/runtime';
import jwt from 'jsonwebtoken';
import { cookies } from 'next/headers';
import { Pool, type PoolClient } from 'pg';

declare global {
  var pool: Pool | undefined;
}

// eslint-disable-next-line import-x/no-unused-modules
export function getPool(): Pool {
  globalThis.pool ??= new Pool();
  return globalThis.pool;
}

export function runQuery<TResult>(
  query: PreparedQuery<void, TResult>,
): Promise<TResult[]>;
export function runQuery<TParams, TResult>(
  query: PreparedQuery<TParams, TResult>,
  params: TParams,
): Promise<TResult[]>;
export function runQuery<TParams, TResult>(
  query: PreparedQuery<TParams, TResult>,
  params?: TParams,
): Promise<TResult[]> {
  return query.run(params as TParams, getPool());
}

// eslint-disable-next-line import-x/no-unused-modules
export async function withTransaction<TResult>(
  callback: (client: PoolClient) => Promise<TResult>,
): Promise<TResult> {
  const client = await getPool().connect();
  try {
    await client.query('begin');
    const result = await callback(client);
    await client.query('commit');
    return result;
  } catch (error) {
    await client.query('rollback');
    throw error;
  } finally {
    client.release();
  }
}

export async function withRequestPgClient<TResult>(
  callback: (client: PoolClient, settings: Record<string, string>) => Promise<TResult>,
) {
  const cookieStore = await cookies();
  const token = cookieStore.get(SESSION_COOKIE)?.value;
  let claims: jwt.JwtPayload | undefined;

  if (token) {
    try {
      claims = jwt.verify(token, process.env.JWT_SECRET!, {
        algorithms: ['HS256'],
        ignoreExpiration: true,
      }) as jwt.JwtPayload;
    } catch (error) {
      if (!(error instanceof jwt.JsonWebTokenError)) throw error;
    }
  }

  const tenant = await getRequestTenant();
  const settings: Record<string, string> = {
    role: 'anonymous',
    'jwt.claims.tenant_id': tenant.id.toString(),
  };

  if (claims) {
    settings.role = claims.is_system_admin
      ? 'system_admin'
      : claims.admin_tenant_ids?.includes(tenant.id)
        ? 'administrator'
        : claims.trainer_tenant_ids?.includes(tenant.id)
          ? 'trainer'
          : claims.member_tenant_ids?.includes(tenant.id)
            ? 'member'
            : 'anonymous';

    for (const [key, value] of Object.entries(claims)) {
      if (!['exp', 'aud', 'iat', 'iss', 'tenant_id'].includes(key)) {
        settings[`jwt.claims.${key}`] = Array.isArray(value)
          ? `{${value.join(',')}}`
          : String(value);
      }
    }
  }

  return withTransaction(async (client) => {
    const entries = Object.entries(settings);
    await client.query(
      `select set_config(name, value, true)
       from unnest($1::text[], $2::text[]) as setting(name, value)`,
      [
        entries.map(([name]) => name),
        entries.map(([, value]) => value),
      ],
    );
    return callback(client, settings);
  });
}
