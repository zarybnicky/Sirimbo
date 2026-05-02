import type { FetchStatus, Loader, MapperArgs } from './types.ts';
import { zx } from '@traversable/zod';

function fetchWithTimeout(
  url: Parameters<typeof fetch>[0],
  init: Omit<RequestInit, 'signal'> = {},
  timeoutMs = 30_000,
) {
  return fetch(url, {
    ...init,
    signal: AbortSignal.timeout(timeoutMs),
  });
}

type ParseResult<T> =
  | { parsed: T; raw: unknown; error: null }
  | { parsed: null; raw: unknown; error: string };

async function parseResponse<T>(
  resp: Response,
  handler: Loader<T>,
  opts: { mode: 'strict' | 'loose' },
): Promise<ParseResult<T>> {
  if (handler.mode === 'text') {
    const text = await resp.text();
    return { parsed: text as T, raw: text, error: null };
  }

  let raw: unknown;
  try {
    raw = await resp.json();
  } catch {
    return { parsed: null, raw: null, error: 'Response body is not valid JSON' };
  }

  const wrapSchema = opts.mode === 'strict' ? zx.deepStrict : zx.deepLoose;
  const result = wrapSchema(handler.schema).safeParse(raw, { reportInput: true });
  return result.success
    ? { parsed: result.data, raw, error: null }
    : {
        parsed: null,
        raw,
        error: result.error.toString(),
      };
}

export async function fetchResponse<T>(
  handler: Loader<T>,
  url: URL,
  init: RequestInit = {},
  opts: { mode: 'strict' | 'loose' } = { mode: 'loose' },
) {
  let httpStatus: number | null = null;
  let parsed: T | null = null;
  let raw: unknown = null;
  let error: string | null;

  try {
    const resp = await fetchWithTimeout(url, init);
    httpStatus = resp.status;

    const result = await parseResponse<T>(resp, handler, opts);
    parsed = result.parsed;
    raw = result.raw;
    error = result.error;
  } catch (e) {
    error = e instanceof Error ? e.message : String(e);
  }

  const mapperArgs: MapperArgs<T> = { httpStatus, parsed, raw, error };

  return {
    httpStatus,
    error,
    content: (parsed ? (handler.cleanResponse?.(url, parsed, raw) ?? parsed) : raw) as T,
    fetchStatus:
      handler.mapResponseToStatus?.(mapperArgs) ?? defaultMapResponseToStatus(mapperArgs),
  } satisfies {
    httpStatus: number | null;
    error: string | null;
    content: T;
    fetchStatus: FetchStatus;
  };
}

const defaultMapResponseToStatus = ({
  error,
  httpStatus,
}: {
  error?: unknown;
  httpStatus: number | null;
}): FetchStatus => {
  if (error) return 'error';
  if (httpStatus === 404) return 'gone';
  if (httpStatus && httpStatus >= 200 && httpStatus < 300) return 'ok';
  return 'error';
};
