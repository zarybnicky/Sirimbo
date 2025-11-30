import type { Task } from 'graphile-worker';
import {
  insertHtmlResponse,
  insertJsonResponse,
  markFrontierFetchError,
  markFrontierFetchSuccess,
} from '../crawler/crawler.queries.ts';
import { defaultMapResponseToStatus, type FetchStatus } from '../crawler/types.ts';
import { getReservation } from '../crawler/getReservation.ts';

export const fetch_frontier: Task<'fetch_frontier'> = async ({ frontierId }, helpers) => {
  const { withPgClient, logger } = helpers;

  const reservation = await getReservation(frontierId, helpers);
  if (!reservation.proceed) return;
  const { frontier, handler, url, init } = reservation;

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
        logger.warn(`Parse error in ${frontierId}, URL ${url} (${error})`);
      }
    }
  } catch (e) {
    error = e instanceof Error ? e.message : String(e);
    logger.warn(`Fetch error in ${frontierId}, URL ${url} (${error})`);
  } finally {
    clearTimeout(timeoutId);
  }

  let newStatus: FetchStatus;
  let content: any = null;
  if (handler.mode === 'json') {
    const mapper = handler.mapResponseToStatus || defaultMapResponseToStatus;
    newStatus = mapper({ httpStatus, parsed, rawJson, error });
    if (parsed != null) {
      content = handler.transformResponse
        ? await handler.transformResponse(url, parsed, rawJson)
        : parsed;
    } else if (rawJson != null) {
      content = rawJson;
    }
  } else if (handler.mode === 'text') {
    const mapper = handler.mapResponseToStatus || defaultMapResponseToStatus;
    newStatus = mapper({ httpStatus, body, error });
    if (body != null) {
      content = handler.transformResponse
        ? await handler.transformResponse(url, body)
        : body;
    }
  }

  await withPgClient(async (client) => {
    await client.query('BEGIN');

    if (handler.mode === 'json') {
      await insertJsonResponse.run(
        { frontierId, url, httpStatus, error, content },
        client,
      );
    } else if (handler.mode === 'text') {
      await insertHtmlResponse.run(
        { frontierId, url, httpStatus, error, content },
        client,
      );
    }
    if (newStatus === 'error') {
      await markFrontierFetchError.run({ id: frontier.id }, client);
    } else {
      await markFrontierFetchSuccess.run(
        {
          id: frontier.id,
          fetchStatus: newStatus,
          processStatus: 'pending',
          revalidatePeriod: handler.revalidatePeriod,
        },
        client,
      );
    }
    await client.query('COMMIT');
  });

  await withPgClient(async (client) => {
    await handler.load(client, url, content);
  });
};

export default fetch_frontier;
