import type { Logger, Task } from 'graphile-worker';
import {
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
  type FetchStatus,
  type FrontierRow,
  type HtmlLoader,
  type JsonLoader,
} from '../crawler/types.ts';
import { getFrontierHandler } from '../crawler/getFrontierHandler.ts';

export const frontier_fetch: Task<'frontier_fetch'> = async ({ id }, helpers) => {
  const { withPgClient, logger } = helpers;

  const result = await withPgClient(async (client) => {
    await client.query('BEGIN');

    const withHandler = await getFrontierHandler(id, client, logger);
    if (!withHandler) {
      await client.query('COMMIT');
      return;
    }
    const { frontier, handler } = withHandler;
    const { url, init } = handler.buildRequest(frontier.key);
    const { host } = url;
    const [{ granted, allowed_at }] = await reserveRequest.run({ host }, client);
    if (!granted) {
      logger.info(`Over-scheduled for host ${host}, rescheduling in ${allowed_at!.getTime() - Date.now()} ms`);
      await rescheduleFrontier.run({ id, nextRetryAt: allowed_at }, client);
      await client.query('COMMIT');
      await helpers.addJob(
        'frontier_fetch',
        { id },
        { jobKey: `fetch:${host}:${id}`, runAt: allowed_at! },
      );
      return;
    }
    await client.query('COMMIT');

    return { frontier, handler, url, init };
  });
  if (!result) return;
  const { frontier, handler, url, init } = result;
  const { httpStatus, error, content, fetchStatus } = await fetchFrontier(
    frontier,
    handler,
    url,
    init,
    logger,
  );

  await withPgClient(async (client) => {
    await client.query('BEGIN');
    if (handler.mode === 'json') {
      await insertJsonResponse.run(
        { id, url: url.toString(), httpStatus, error, content },
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
    if ((jobs[0].count ?? 0) <= 1) { // Count the current job too!
      helpers.addJob('frontier_schedule', {});
    }

    await client.query('COMMIT');
  });
};

async function fetchFrontier(
  frontier: FrontierRow,
  handler: JsonLoader | HtmlLoader,
  url: URL,
  init: RequestInit | undefined,
  logger: Logger,
) {
  let httpStatus: number | null = null;
  let body: string | null = null;
  let rawJson: unknown | null = null;
  let parsed: any | null = null;
  let error: string | null = null;

  const controller = new AbortController();
  const timeoutId = setTimeout(() => controller.abort(), 30_000); // 30s timeout
  try {
    const resp = await fetch(url, { ...init, signal: controller.signal });
    httpStatus = resp.status;

    body = await resp.text();
    if (handler.mode === 'json' && body.trim().length > 0) {
      rawJson = JSON.parse(body);
      const parsedRes = handler.schema.safeParse(rawJson);
      if (parsedRes.success) {
        parsed = parsedRes.data;
      } else {
        error = parsedRes.error.toString();
        logger.warn(`Parse error in ${frontier.id}, URL ${url} (${error})`);
      }
    }
  } catch (e) {
    error = e instanceof Error ? e.message : String(e);
    logger.warn(`Fetch error in ${frontier.id}, URL ${url} (${error})`);
  } finally {
    clearTimeout(timeoutId);
  }

  let fetchStatus: FetchStatus;
  let content: any = null;
  if (handler.mode === 'json') {
    const mapper = handler.mapResponseToStatus || defaultMapResponseToStatus;
    fetchStatus = mapper({ httpStatus, parsed, rawJson, error });
    if (parsed != null) {
      content = handler.cleanResponse
        ? await handler.cleanResponse(url, parsed, rawJson)
        : parsed;
    } else if (rawJson != null) {
      content = rawJson;
    }
  } else {
    const mapper = handler.mapResponseToStatus || defaultMapResponseToStatus;
    fetchStatus = mapper({ httpStatus, body, error });
    if (body != null) {
      content = handler.cleanResponse ? await handler.cleanResponse(url, body) : body;
    }
  }

  return { httpStatus, error, content, fetchStatus };
}

export default frontier_fetch;
