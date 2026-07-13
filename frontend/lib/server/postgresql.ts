import 'server-only';

import type { PreparedQuery } from '@pgtyped/runtime';
import { Pool, type PoolClient } from 'pg';

declare global {
  var pool: Pool | undefined;
}

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
