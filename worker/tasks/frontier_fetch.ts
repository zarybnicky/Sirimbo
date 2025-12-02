import type { Logger, Task } from 'graphile-worker';
import {
  insertHtmlResponse,
  insertJsonResponse,
  markFrontierFetchError,
  markFrontierFetchSuccess,
  rescheduleFrontier,
} from '../crawler/crawler.queries.ts';
import {
  defaultMapResponseToStatus,
  type FetchStatus,
  type FrontierRow,
  type HtmlLoader,
  type JsonLoader,
} from '../crawler/types.ts';
import { getReservation } from '../crawler/getReservation.ts';
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
    const { url, init } = handler.buildRequest(frontier);

    const reservation = await getReservation(url, client);
    if (!reservation.proceed) {
      const { runAt } = reservation;
      await rescheduleFrontier.run({ id, nextRetryAt: runAt }, client);
      await client.query('COMMIT');
      await helpers.addJob('frontier_fetch', { id }, { jobKey: `fetch:${id}`, runAt });
      return;
    }
    await client.query('COMMIT');

    return { frontier, handler, url, init };
  });
  if (!result) {
    return;
  }
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
      await insertJsonResponse.run({ id, url, httpStatus, error, content }, client);
    } else {
      await insertHtmlResponse.run({ id, url, httpStatus, error, content }, client);
    }

    if (fetchStatus === 'error') {
      await markFrontierFetchError.run({ id }, client);
    } else {
      const { revalidatePeriod } = handler;
      await markFrontierFetchSuccess.run({ id, fetchStatus, revalidatePeriod }, client);
    }
    await client.query('COMMIT');
  });
};

async function fetchFrontier(
  frontier: FrontierRow,
  handler: JsonLoader | HtmlLoader,
  url: string,
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
      const parsedRes = handler.responseSchema.safeParse(rawJson);
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
      content = handler.transformResponse
        ? await handler.transformResponse(url, parsed, rawJson)
        : parsed;
    } else if (rawJson != null) {
      content = rawJson;
    }
  } else {
    const mapper = handler.mapResponseToStatus || defaultMapResponseToStatus;
    fetchStatus = mapper({ httpStatus, body, error });
    if (body != null) {
      content = handler.transformResponse
        ? await handler.transformResponse(url, body)
        : body;
    }
  }

  return { httpStatus, error, content, fetchStatus };
}

export default frontier_fetch;
