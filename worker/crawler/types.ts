import { z } from 'zod';
import type { IGetFrontierForUpdateResult } from './crawler.queries.ts';
import type { PoolClient } from 'pg';

export type FetchStatus = 'pending' | 'ok' | 'gone' | 'error';

export type FrontierRow = IGetFrontierForUpdateResult;

export interface JsonLoader<T = any> {
  mode: 'json';
  schema: z.ZodType<T>;
  buildRequest: (key: string) => {
    url: URL;
    init?: RequestInit;
  };
  mapResponseToStatus?: (args: {
    httpStatus: number | null;
    parsed: T | null;
    rawJson: unknown | null;
    error?: unknown;
  }) => FetchStatus | undefined;
  cleanResponse?: (url: URL, parsed: T, rawJson: unknown) => Promise<T> | T;
  revalidatePeriod: string;
  load: (client: PoolClient, frontier: FrontierRow, parsed: T) => Promise<void>;
}

export interface HtmlLoader {
  mode: 'text';
  buildRequest: (key: string) => {
    url: URL;
    init?: RequestInit;
  };
  mapResponseToStatus?: (args: {
    httpStatus: number | null;
    body: string | null;
    error?: unknown;
  }) => FetchStatus | undefined;
  cleanResponse?: (url: URL, body: string | null) => Promise<string> | string;
  revalidatePeriod: string;
  load: (client: PoolClient, frontier: FrontierRow, body: string) => Promise<void>;
}

export const defaultMapResponseToStatus = ({
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
