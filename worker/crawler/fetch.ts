import {
  defaultMapResponseToStatus,
  type FetchStatus,
  type HtmlLoader,
  type JsonLoader,
} from './types.ts';
import { zx } from '@traversable/zod';

export type FetchResult<T = unknown> = {
  httpStatus: number | null;
  error: string | null;
  content: T;
  fetchStatus: FetchStatus;
};

export async function fetchJsonResponse<T>(
  handler: JsonLoader<T>,
  url: URL,
  init: RequestInit = {},
  opts: { mode: 'strict' | 'loose' },
): Promise<FetchResult<T | unknown>> {
  const schema =
    opts.mode === 'strict' ? zx.deepStrict(handler.schema) : zx.deepLoose(handler.schema);

  let httpStatus: number | null = null;
  let rawJson: unknown | null = null;
  let parsed: T | null = null;
  let error: string | null = null;

  const controller = new AbortController();
  const timeoutId = setTimeout(() => controller.abort(), 30_000);
  try {
    const resp = await fetch(url, { ...init, signal: controller.signal });
    httpStatus = resp.status;

    rawJson = await resp.json();
    const parsedRes = schema.safeParse(rawJson, { reportInput: true });
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

  let content: unknown = '';
  if (parsed != null) {
    content = handler.cleanResponse
      ? await handler.cleanResponse(url, parsed, rawJson)
      : parsed;
  } else if (rawJson != null) {
    content = rawJson;
  }

  return { httpStatus, error, content, fetchStatus };
}

export async function fetchTextResponse(
  handler: HtmlLoader,
  url: URL,
  init: RequestInit = {},
): Promise<FetchResult<string | null>> {
  let httpStatus: number | null = null;
  let body: string | null = null;
  let error: string | null = null;

  const controller = new AbortController();
  const timeoutId = setTimeout(() => controller.abort(), 30_000);
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
