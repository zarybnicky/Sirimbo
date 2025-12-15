import type { Task, Logger } from 'graphile-worker';
import {
  getFrontierJsonResponseForUpdate,
  getFrontierHtmlResponseForUpdate,
  markFrontierProcessError,
  markFrontierProcessSuccess,
  getJobCountForTask,
} from '../crawler/crawler.queries.ts';

import { getFrontierHandler } from '../crawler/getFrontierHandler.ts';
import type { PoolClient } from 'pg';
import type { IDatabaseConnection } from '@pgtyped/runtime';

export const frontier_process: Task<'frontier_process'> = async ({ ids }, helpers) => {
  const { logger, withPgClient } = helpers;
  for (const id of ids) {
    await withPgClient(async (client) => {
      try {
        await client.query('BEGIN');
        // await client.query('SET LOCAL SEARCH_PATH = federated, pg_temp');
        await processSingle(id, client, logger);
        await client.query('COMMIT');
      } catch (e) {
        await client.query('ROLLBACK');
        logger.error(
          `Processing frontier ${id} failed: ${e} ${e instanceof Error ? e.stack : ''}`,
        );
        await markFrontierProcessError.run({ id }, client);
      }
    });
  }

  const jobs = await getJobCountForTask.run(
    { task: 'frontier_process' },
    helpers as IDatabaseConnection,
  );
  if ((jobs[0].count ?? 0) <= 1) {
    // Count the current job too!
    helpers.addJob('frontier_schedule', {});
  }
};

const processSingle = async (id: string, client: PoolClient, logger: Logger) => {
  const result = await getFrontierHandler(id, client, logger);
  if (!result) {
    await markFrontierProcessSuccess.run({ id }, client);
    return;
  }
  const { frontier, handler } = result;

  const [contentRow] =
    handler.mode === 'json'
      ? await getFrontierJsonResponseForUpdate.run({ id }, client)
      : await getFrontierHtmlResponseForUpdate.run({ id }, client);
  if (!contentRow?.content) {
    await markFrontierProcessSuccess.run({ id }, client);
    return;
  }

  const content =
    handler.mode === 'json'
      ? handler.schema.parse(contentRow.content, {
        reportInput: true
      })
      : contentRow.content;
  await handler.load(client, frontier, content);
  await markFrontierProcessSuccess.run({ id }, client);
};

export default frontier_process;
