import type { JobHelpers } from 'graphile-worker';
import {
  getFrontierForUpdate,
  markFrontierFetchError,
  rescheduleFrontier,
  reserveRequest,
} from './crawler.queries.ts';
import { LOADERS } from './handlers.ts';

export const getReservation = (frontierId: number, helpers: JobHelpers) => {
  return helpers.withPgClient(async (client) => {
    await client.query('BEGIN');

    const [frontier] = await getFrontierForUpdate.run({ id: frontierId }, client);
    if (!frontier) {
      await client.query('COMMIT');
      helpers.logger.info(`Frontier ${frontierId} not found, skipping`);
      return { proceed: false } as const;
    }

    if (frontier.fetch_status !== 'pending') {
      await client.query('COMMIT');
      helpers.logger.info(
        `Frontier ${frontierId} status=${frontier.fetch_status}, skipping`,
      );
      return { proceed: false } as const;
    }

    const handler = LOADERS[frontier.federation][frontier.kind];
    if (!handler) {
      await markFrontierFetchError.run({ id: frontierId }, client);
      await client.query('COMMIT');
      helpers.logger.error(
        `No handler for ${frontier.federation}/${frontier.kind}, skipping frontier ${frontierId}`,
      );
      return { proceed: false } as const;
    }

    const { url, init } = handler.buildRequest(frontier);
    const { host, pathname } = new URL(url);
    const prefixes = buildPrefixes(pathname);
    const [{ granted, allowed_at }] = await reserveRequest.run(
      { host, prefixes },
      client,
    );

    if (!granted) {
      const runAt = allowed_at || new Date(Date.now() + 5_000);
      await rescheduleFrontier.run({ id: frontierId, nextRetryAt: runAt }, client);
      await client.query('COMMIT');
      await helpers.addJob('fetch_frontier', { frontierId }, { runAt });
      return { proceed: false } as const;
    }

    await client.query('COMMIT');
    return { proceed: true, frontier, handler, url, init } as const;
  });
};

export function buildPrefixes(path: string): string[] {
  if (!path.startsWith('/')) path = '/' + path;
  const parts = path.split('/').filter(Boolean);
  const prefixes: string[] = [];
  for (let i = parts.length; i >= 1; i--) {
    prefixes.push('/' + parts.slice(0, i).join('/'));
  }
  prefixes.push('/');
  return prefixes;
}
