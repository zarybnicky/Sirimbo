import type { Task } from 'graphile-worker';
import {
  countPendingProcess,
  getFrontierHtmlResponseForUpdate,
  getFrontierJsonResponseForUpdate,
  getNextPendingProcess,
  markFrontierFetchError,
  markFrontierProcessError,
  markFrontierProcessSuccess,
} from '../crawler/crawler.queries.ts';
import { LOADER_MAP } from '../crawler/handlers.ts';

type ProcessorStats = { count: number; ms: number; errors: number };

function bump(
  map: Map<string, ProcessorStats>,
  federation: string,
  kind: string,
  ms: number,
  isError: boolean,
) {
  const s = map.get(`${federation}:${kind}`) ?? { count: 0, ms: 0, errors: 0 };
  s.count += 1;
  s.ms += ms;
  if (isError) s.errors += 1;
  map.set(`${federation}:${kind}`, s);
}

const BUDGET_MS = 1000;

export const frontier_process: Task<'frontier_process'> = async (_ignored, helpers) => {
  const { logger, withPgClient } = helpers;

  let processed = 0;
  const byKind = new Map<string, ProcessorStats>();
  const started = performance.now();

  await withPgClient(async (client) => {
    while (performance.now() - started < BUDGET_MS) {
      const t0 = performance.now();

      await client.query('BEGIN');
      const [frontier] = await getNextPendingProcess.run(undefined, client);
      if (!frontier) {
        await client.query('ROLLBACK');
        break;
      }
      const { id, federation, kind } = frontier;

      const handler = LOADER_MAP[federation]?.[kind];
      if (!handler) {
        await markFrontierFetchError.run({ id }, client);
        await client.query('COMMIT');
        logger.error(`Handler for frontier ${id} not found (${federation}/${kind})`);
        bump(byKind, federation, kind, performance.now() - t0, true);
        continue;
      }
      try {
        const [contentRow] =
          handler.mode === 'json'
            ? await getFrontierJsonResponseForUpdate.run({ id }, client)
            : await getFrontierHtmlResponseForUpdate.run({ id }, client);
        if (contentRow?.content) {
          const content =
            handler.mode === 'json'
              ? handler.schema.parse(contentRow.content, {
                  reportInput: true,
                })
              : contentRow.content;
          await handler.load(client, frontier, content);
        }
        await markFrontierProcessSuccess.run({ id }, client);
        await client.query('COMMIT');
        processed += 1;
        bump(byKind, federation, kind, performance.now() - t0, false);
      } catch (e) {
        await client.query('ROLLBACK');
        await markFrontierProcessError.run({ id }, client);

        const stack = e instanceof Error ? e.stack : '';
        logger.error(`Processing frontier ${id} failed: ${e} ${stack}`);
        bump(byKind, federation, kind, performance.now() - t0, true);
      }
    }

    const [countItems] = await countPendingProcess.run(undefined, client);
    const pendingItems = countItems?.count ?? 0;
    if (pendingItems > 0) {
      await helpers.addJob('frontier_process', {});
    }
    const pending = pendingItems > 0 ? ` (${pendingItems} pending)` : '';

    if (processed > 0) {
      const totalMs = Math.round(performance.now() - started);
      const lines = [...byKind.entries()]
        .sort((a, b) => b[1].ms - a[1].ms)
        .map(([k, s]) => `${k} (${s.count}x, ${Math.round(s.ms)}ms, err=${s.errors})`)
        .join(' | ');

      logger.info(`Processed ${processed} rows in ${totalMs}ms${pending} ${lines}`);
    }
  });
};

export default frontier_process;
