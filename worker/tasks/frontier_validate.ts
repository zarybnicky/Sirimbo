import type { Task } from 'graphile-worker';
import { getLatestFrontierJsonResponses } from '../crawler/crawler.queries.ts';
import { LOADER_MAP } from '../crawler/handlers.ts';
import { zx } from '@traversable/zod';
import Cursor from 'pg-cursor';

export const frontier_validate: Task<'frontier_validate'> = async (
  { federation, kind },
  helpers,
) => {
  const { withPgClient, logger } = helpers;

  const handler = LOADER_MAP[federation]?.[kind];
  if (!handler) {
    logger.error(`Handler for ${federation}/${kind} not found`);
    return;
  }
  if (handler.mode !== 'json') {
    return;
  }
  const strictSchema = zx.deepStrict(handler.schema);

  await withPgClient(async (client) => {
    const cursor = getLatestFrontierJsonResponses.stream(
      { federation, kind },
      {
        query: client.query,
        stream: (query, bindings) => client.query(new Cursor(query, bindings)),
      },
    );

    let count = 0;

    outer: while (true) {
      const rows = await cursor.read(10);
      if (rows.length === 0) break;

      for (const row of rows) {
        try {
          strictSchema.parse(row.content, {
            reportInput: true,
          });
          count++;
        } catch (e) {
          console.error(e);
          await cursor.close();
          break outer;
        }
      }
    }

    helpers.logger.info(`Validated ${count} responses of type ${federation}/${kind}`);
  });
};

export default frontier_validate;
