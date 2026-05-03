import type { Task } from 'graphile-worker';
import {
  getNextPendingProcess,
  markFrontierFetchError,
  markFrontierProcessError,
  markFrontiersProcessSuccess,
} from '../crawler/crawler.queries.ts';
import { loaderFor } from '../crawler/handlers.ts';
import { zx } from '@traversable/zod';

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
const PROCESS_BATCH_SIZE = 20;

export const frontier_process: Task<'frontier_process'> = async (payload, helpers) => {
  const { logger, withPgClient } = helpers;
  const { isFullRebuild = false } = payload;

  let processed = 0;
  let foundNoMoreRows = false;
  const byKind = new Map<string, ProcessorStats>();
  const started = performance.now();

  await withPgClient(async (client) => {
    while (performance.now() - started < BUDGET_MS) {
      await client.query('BEGIN');
      const frontiers = await getNextPendingProcess.run(
        { limit: PROCESS_BATCH_SIZE },
        client,
      );
      if (frontiers.length === 0) {
        foundNoMoreRows = true;
        await client.query('ROLLBACK');
        break;
      }

      const successfulBatch: Array<{ federation: string; kind: string; ms: number }> = [];
      let failedFrontier: (typeof frontiers)[number] | null = null;
      let failedAt = 0;

      try {
        for (const frontier of frontiers) {
          const { federation, kind } = frontier;
          failedFrontier = frontier;
          failedAt = performance.now();

          const loader = loaderFor(federation, kind);
          if (!loader) throw new Error(`Unknown loader ${federation}:${kind}`);

          let content = frontier.content;
          if (loader.mode === 'json') {
            content = zx.deepLoose(loader.schema).parse(content, {
              reportInput: true,
            });
          }
          await loader.load(client, content);

          successfulBatch.push({
            federation,
            kind,
            ms: performance.now() - failedAt,
          });
        }

        const ids = frontiers.map((f) => f.id);
        await markFrontiersProcessSuccess.run({ ids }, client);

        await client.query('COMMIT');
        processed += successfulBatch.length;
        for (const { federation, kind, ms } of successfulBatch) {
          bump(byKind, federation, kind, ms, false);
        }
      } catch (e) {
        await client.query('ROLLBACK');

        if (!failedFrontier) throw e;
        const { id, federation, kind } = failedFrontier;
        if (e instanceof Error && e.message.includes('Unknown loader')) {
          await markFrontierFetchError.run({ id }, client);
          logger.error(`Handler for frontier ${id} not found (${federation}/${kind})`);
        } else {
          await markFrontierProcessError.run({ id }, client);
          const stack = e instanceof Error ? e.stack : '';
          logger.error(`Processing frontier ${id} failed: ${e} ${stack}`);
        }
        bump(byKind, federation, kind, performance.now() - failedAt, true);
      }
    }

    if (processed > 0) {
      const totalMs = Math.round(performance.now() - started);
      const lines = [...byKind.entries()]
        .sort((a, b) => b[1].ms - a[1].ms)
        .map(([k, s]) => `${k} (${s.count}x, ${Math.round(s.ms)}ms, err=${s.errors})`)
        .join(' | ');

      logger.info(`Processed ${processed} rows in ${totalMs}ms ${lines}`);
    }

    if (!foundNoMoreRows) {
      await helpers.addJob('frontier_process', { isFullRebuild });
    }
  });
};

export default frontier_process;
