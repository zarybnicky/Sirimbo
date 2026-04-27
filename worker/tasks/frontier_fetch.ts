import type { Task } from 'graphile-worker';
import {
  getFrontierForUpdate,
  getJobCountForTask,
  insertHtmlResponse,
  insertJsonResponse,
  markFrontierFetchError,
  markFrontierFetchSuccess,
  rescheduleFrontier,
  reserveRequest,
} from '../crawler/crawler.queries.ts';
import { fetchJsonResponse, fetchTextResponse } from '../crawler/fetch.ts';
import { LOADER_MAP } from '../crawler/handlers.ts';

export const frontier_fetch: Task<'frontier_fetch'> = async ({ id }, helpers) => {
  const { withPgClient, logger } = helpers;

  const result = await withPgClient(async (client) => {
    await client.query('BEGIN');

    const [frontier] = await getFrontierForUpdate.run({ id }, client);
    if (!frontier) {
      logger.info(`Frontier ${id} not found`);
      await client.query('COMMIT');
      return;
    }
    const { federation, kind } = frontier;

    const handler = LOADER_MAP[federation]?.[kind];
    if (!handler) {
      await markFrontierFetchError.run({ id }, client);
      logger.error(`Handler for frontier ${id} not found (${federation}/${kind})`);
      await client.query('COMMIT');
      return;
    }

    const { url, init } = handler.buildRequest(frontier.key);
    const { host } = url;
    const [{ granted, allowed_at }] = await reserveRequest.run({ host }, client);
    if (!granted) {
      logger.info(
        `Over-scheduled for host ${host}, rescheduling in ${allowed_at!.getTime() - Date.now()} ms`,
      );
      await rescheduleFrontier.run({ id, nextRetryAt: allowed_at }, client);
      await client.query('COMMIT');
      const jobKey = `fetch:${host}:${id}`;
      await helpers.addJob('frontier_fetch', { id }, { jobKey, runAt: allowed_at! });
      return;
    }
    await client.query('COMMIT');

    return { frontier, handler, url, init: init || {} };
  });
  if (!result) return;
  const { frontier, handler, url, init } = result;

  const { httpStatus, error, content, fetchStatus } =
    handler.mode === 'json'
      ? await fetchJsonResponse(handler, url, init)
      : await fetchTextResponse(handler, url, init);

  if (error) {
    logger.warn(`Fetch error in ${frontier.id}, URL ${url} (${error})`);
  }

  await withPgClient(async (client) => {
    await client.query('BEGIN');
    if (handler.mode === 'json') {
      await insertJsonResponse.run(
        { id, url: url.toString(), httpStatus, error, content: JSON.stringify(content) },
        client,
      );
    } else {
      await insertHtmlResponse.run(
        { id, url: url.toString(), httpStatus, error, content: String(content) },
        client,
      );
    }

    if (fetchStatus === 'error') {
      await markFrontierFetchError.run({ id }, client);
    } else {
      const { revalidatePeriod } = handler;
      await markFrontierFetchSuccess.run({ id, fetchStatus, revalidatePeriod }, client);
    }

    const jobs = await getJobCountForTask.run({ task: 'frontier_fetch' }, client);
    if ((jobs[0].count ?? 0) <= 1) {
      // Count the current job too!
      await helpers.addJob('frontier_schedule', {});
    }

    await client.query('COMMIT');
  });
};

export default frontier_fetch;
