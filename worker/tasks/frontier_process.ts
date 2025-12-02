import type { Task } from 'graphile-worker';
import {
  getFrontierJsonResponseForUpdate,
  getFrontierHtmlResponseForUpdate,
  markFrontierProcessError,
  markFrontierProcessSuccess,
} from '../crawler/crawler.queries.ts';

import { getFrontierHandler } from '../crawler/getFrontierHandler.ts';

export const frontier_process: Task<'frontier_process'> = async ({ id }, helpers) => {
  const { withPgClient, logger } = helpers;

  await withPgClient(async (client) => {
    await client.query('BEGIN');

    const result = await getFrontierHandler(id, client, logger);
    if (!result) {
      await markFrontierProcessSuccess.run({ id }, client);
      await client.query('COMMIT');
      return;
    }
    const { frontier, handler } = result;

    const [contentRow] =
      handler.mode === 'json'
        ? await getFrontierJsonResponseForUpdate.run({ id }, client)
        : await getFrontierHtmlResponseForUpdate.run({ id }, client);
    if (!contentRow?.content) {
      await markFrontierProcessSuccess.run({ id }, client);
      await client.query('COMMIT');
      return;
    }

    try {
      const content =
        handler.mode === 'json'
          ? handler.schema.parse(contentRow.content)
          : contentRow.content;
      await handler.load(client, frontier, content);
      await markFrontierProcessSuccess.run({ id }, client);
      await client.query('COMMIT');
    } catch (e) {
      await client.query('ROLLBACK');
      logger.error(
        `Processing frontier ${id} failed: ${e} ${e instanceof Error ? e.stack : ''}`,
      );
      await client.query('BEGIN');
      await markFrontierProcessError.run({ id }, client);
      await client.query('COMMIT');
    }
  });
};

export default frontier_process;
