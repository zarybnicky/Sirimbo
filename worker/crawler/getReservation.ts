import { reserveRequest } from './crawler.queries.ts';
import type { PoolClient } from 'pg';

export const getReservation = async (url: string, client: PoolClient) => {
  const { host, pathname } = new URL(url);
  const prefixes = buildPrefixes(pathname);
  const [{ granted, allowed_at }] = await reserveRequest.run({ host, prefixes }, client);
  if (!granted) {
    return { proceed: false, runAt: allowed_at || new Date(Date.now() + 5_000) } as const;
  }
  return { proceed: true } as const;
};

function buildPrefixes(path: string): string[] {
  if (!path.startsWith('/')) path = '/' + path;
  const parts = path.split('/').filter(Boolean);
  const prefixes: string[] = [];
  for (let i = parts.length; i >= 1; i--) {
    prefixes.push('/' + parts.slice(0, i).join('/'));
  }
  prefixes.push('/');
  return prefixes;
}
