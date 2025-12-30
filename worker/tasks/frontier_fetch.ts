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
import {
  defaultMapResponseToStatus,
  type HtmlLoader,
  type JsonLoader,
} from '../crawler/types.ts';
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
      ? await fetchFrontierJson(handler, url, init)
      : await fetchFrontierHtml(handler, url, init);

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
        { id, url: url.toString(), httpStatus, error, content },
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

async function fetchFrontierJson(handler: JsonLoader, url: URL, init: RequestInit) {
  let httpStatus: number | null = null;
  let rawJson: unknown | null = null;
  let parsed: any | null = null;
  let error: string | null = null;

  const controller = new AbortController();
  const timeoutId = setTimeout(() => controller.abort(), 30_000); // 30s timeout
  try {
    const resp = await fetch(url, { ...init, signal: controller.signal });
    httpStatus = resp.status;

    rawJson = await resp.json();
    const parsedRes = handler.schema.safeParse(rawJson, {
      reportInput: true,
    });
    if (parsedRes.success) {
      parsed = parsedRes.data;
    } else {
      error = parsedRes.error.toString();
    }
  } catch (e) {
    error = e instanceof Error ? e.message : String(e);
  } finally {
    clearTimeout(timeoutId);
  }

  const mapperArgs = { httpStatus, parsed, rawJson, error };
  const mapper = handler.mapResponseToStatus || defaultMapResponseToStatus;
  const fetchStatus = mapper(mapperArgs) ?? defaultMapResponseToStatus(mapperArgs);

  let content: any = '';
  if (parsed != null) {
    content = handler.cleanResponse
      ? await handler.cleanResponse(url, parsed, rawJson)
      : parsed;
  } else if (rawJson != null) {
    content = rawJson;
  }

  return { httpStatus, error, content, fetchStatus };
}

async function fetchFrontierHtml(handler: HtmlLoader, url: URL, init: RequestInit) {
  let httpStatus: number | null = null;
  let body: string | null = null;
  let error: string | null = null;

  const controller = new AbortController();
  const timeoutId = setTimeout(() => controller.abort(), 30_000); // 30s timeout
  try {
    const resp = await fetch(url, { ...init, signal: controller.signal });
    httpStatus = resp.status;
    body = await resp.text();
  } catch (e) {
    error = e instanceof Error ? e.message : String(e);
  } finally {
    clearTimeout(timeoutId);
  }

  const mapperArgs = { httpStatus, body, error };
  const mapper = handler.mapResponseToStatus || defaultMapResponseToStatus;
  const fetchStatus = mapper(mapperArgs) ?? defaultMapResponseToStatus(mapperArgs);
  const content = handler.cleanResponse ? await handler.cleanResponse(url, body) : body;

  return { httpStatus, error, content, fetchStatus };
}

export default frontier_fetch;
