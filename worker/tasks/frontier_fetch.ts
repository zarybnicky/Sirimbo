import type { Task } from 'graphile-worker';
import {
  getFrontierForUpdate,
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
    let committed = false;

    try {
      const [frontier] = await getFrontierForUpdate.run({ id }, client);
      if (!frontier) {
        logger.info(`Frontier ${id} not found`);
        await client.query('COMMIT');
        committed = true;
        return;
      }
      const { federation, kind } = frontier;

      const loader = loaderFor(federation, kind);
      if (!loader) {
        await markFrontierFetchError.run({ id }, client);
        logger.error(`Handler for frontier ${id} not found (${federation}/${kind})`);
        await client.query('COMMIT');
        committed = true;
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
        await client.query(
          "SELECT graphile_worker.add_job('frontier_fetch', json_build_object('id', $1::text), run_at => $2::timestamptz, job_key => $3::text, job_key_mode => 'replace')",
          [id, allowed_at, `fetch:${host}:${id}`],
        );
        await client.query('COMMIT');
        committed = true;
        return;
      }
      await client.query('COMMIT');
      committed = true;

      return { frontier, handler: loader, url, init: init || {} };
    } catch (e) {
      if (!committed) await client.query('ROLLBACK');
      throw e;
    }
  });
  if (!result) return;
  const { frontier, handler, url, init } = result;

  const { httpStatus, error, content, fetchStatus } = await fetchResponse(
    handler,
    url,
    init,
  );

  if (error && (fetchStatus === 'error' || fetchStatus === 'transient')) {
    logger.warn(`Fetch error in ${frontier.id}, URL ${url} (${error})`);
  }

  await withPgClient(async (client) => {
    await client.query('BEGIN');
    try {
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

      await client.query('COMMIT');
    } catch (e) {
      await client.query('ROLLBACK');
      throw e;
    }
  });
};

export default frontier_fetch;
