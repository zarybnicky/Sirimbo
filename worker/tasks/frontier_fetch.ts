import type { Task } from 'graphile-worker';
import {
  getFrontierForUpdate,
  getJobCountForTask,
  insertResponse,
  markFrontierFetchError,
  markFrontierFetchSuccess,
  markFrontierTransient,
  rescheduleFrontier,
  reserveRequest,
} from '../crawler/crawler.queries.ts';
import { fetchResponse } from '../crawler/fetch.ts';
import { loaderFor } from '../crawler/handlers.ts';

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

    const loader = loaderFor(federation, kind);
    if (!loader) {
      await markFrontierFetchError.run({ id }, client);
      logger.error(`Handler for frontier ${id} not found (${federation}/${kind})`);
      await client.query('COMMIT');
      return;
    }

    const { url, init } = loader.buildRequest(frontier.key);
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

    return { frontier, handler: loader, url, init: init || {} };
  });
  if (!result) return;
  const { frontier, handler, url, init } = result;

  const { httpStatus, error, content, fetchStatus } = await fetchResponse(
    handler,
    url,
    init,
  );

  if (error) {
    logger.warn(`Fetch error in ${frontier.id}, URL ${url} (${error})`);
  }

  await withPgClient(async (client) => {
    await client.query('BEGIN');
    await insertResponse.run(
      { id, url: url.toString(), httpStatus, error, content: JSON.stringify(content) },
      client,
    );

    if (fetchStatus === 'transient') {
      await markFrontierTransient.run({ id }, client);
    } else if (fetchStatus === 'error') {
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
