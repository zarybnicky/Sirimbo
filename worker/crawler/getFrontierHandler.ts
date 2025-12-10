import type { PoolClient } from 'pg';
import type { Logger } from 'graphile-worker';
import { getFrontierForUpdate, markFrontierFetchError } from './crawler.queries.ts';
import { LOADER_MAP } from './handlers.ts';

export async function getFrontierHandler(id: string, client: PoolClient, logger: Logger) {
  const [frontier] = await getFrontierForUpdate.run({ id }, client);
  if (!frontier) {
    logger.info(`Frontier ${id} not found`);
    return;
  }
  const { federation, kind } = frontier;

  const handler = LOADER_MAP[federation]?.[kind];
  if (!handler) {
    await markFrontierFetchError.run({ id }, client);
    logger.error(`Handler for frontier ${id} not found (${federation}/${kind})`);
    return;
  }
  return { frontier, handler } as const;
}
