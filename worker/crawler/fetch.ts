import type { Loader } from './types.ts';
import { zx } from '@traversable/zod';
import type { fetch_status } from './crawler.queries.ts';
import * as Impit from 'impit';

const impit = new Impit.Impit({
  browser: "chrome",
  ignoreTlsErrors: true,
});

function classifyFetchError(e: unknown): {
  error: string;
  fetchStatus: 'transient' | 'error';
} {
  if (!(e instanceof Error)) return { error: String(e), fetchStatus: 'error' };

  if (['AbortError', 'TimeoutError'].includes(e.name) || (e as any).type === 'system') {
    return { error: e.message, fetchStatus: 'transient' };
  }
  return { error: e.message, fetchStatus: 'error' };
}

function classifyHttpStatus(status: number): fetch_status {
  if (status === 404 || status === 410) return 'gone';
  if (status === 429 || status >= 500) return 'transient';
  if (status >= 400) return 'error';
  return 'ok';
}

type ParseResult<T> =
  | { parsed: T; raw: unknown; error: null }
  | { parsed: null; raw: unknown; error: string };

async function parseResponse<T>(
  resp: Impit.ImpitResponse,
  handler: Loader<T>,
  opts: { mode: 'strict' | 'loose' },
): Promise<ParseResult<T>> {
  if (handler.mode === 'text') {
    const text = await resp.text();
    return { parsed: text as T, raw: text, error: null };
  }

  const body = await resp.text();
  let raw: unknown;
  try {
    raw = JSON.parse(body);
  } catch {
    return { parsed: null, raw: body, error: 'Response body is not valid JSON' };
  }

  const wrapSchema = opts.mode === 'strict' ? zx.deepStrict : zx.deepLoose;
  const result = wrapSchema(handler.schema).safeParse(raw, { reportInput: true });
  if (!result.success) {
    const error = result.error.toString();
    return { parsed: null, raw, error };
  } else {
    return { parsed: result.data, raw, error: null };
  }
}

export async function fetchResponse<T>(
  handler: Loader<T>,
  url: URL,
  init: Impit.RequestInit = {},
  opts: { mode: 'strict' | 'loose' } = { mode: 'loose' },
) {
  let httpStatus: number | null = null;
  let parsed: T | null = null;
  let raw: unknown = null;
  let error: string | null = null;
  let fetchStatus: fetch_status = 'ok';

  try {
    const resp = await impit.fetch(url, {
      ...init,
      signal: AbortSignal.timeout(30_000),
    });

    httpStatus = resp.status;
    fetchStatus = classifyHttpStatus(httpStatus);

    const result = await parseResponse<T>(resp, handler, opts);
    parsed = result.parsed;
    raw = result.raw;
    if (result.error) {
      error = result.error;
      if (fetchStatus === 'ok' || fetchStatus === 'gone') {
        // Schema/parse failure on non-transient responses = permanent (API changed or error page)
        fetchStatus = 'error';
      }
    }
  } catch (e) {
    const classified = classifyFetchError(e);
    error = classified.error;
    fetchStatus = classified.fetchStatus;
  }

  return {
    httpStatus,
    error,
    content: (parsed ? (handler.cleanResponse?.(url, parsed, raw) ?? parsed) : raw) as T,
    fetchStatus:
      handler.mapResponseToStatus?.({ parsed, raw, error, httpStatus, fetchStatus }) ?? fetchStatus,
  };
}
