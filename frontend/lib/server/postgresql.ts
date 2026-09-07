import 'server-only';

import { getRequestContext } from '@/lib/server/tenant';
import type { PreparedQuery } from '@pgtyped/runtime';
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
  const { settings } = await getRequestContext();

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
