import { z } from 'zod';
import type { PoolClient } from 'pg';

export type FetchStatus = 'pending' | 'ok' | 'gone' | 'error';

export interface FrontierRow {
  id: string;
  federation: string;
  kind: string;
  key: string;
  fetch_status: FetchStatus;
  error_count: number;
  meta: any;
}

export interface FetchRequestSpec {
  url: string;
  init?: RequestInit;
}

export interface JsonLoader<TStored = any, TParsed = TStored> {
  mode: 'json';
  schema: z.ZodType<TParsed>;
  buildRequest: (frontier: FrontierRow) => FetchRequestSpec;
  mapResponseToStatus?: (args: {
    httpStatus: number | null;
    parsed: TParsed | null;
    rawJson: unknown | null;
    error?: unknown;
  }) => FetchStatus;
  transformResponse?: (
    url: string,
    parsed: TParsed,
    rawJson: unknown,
  ) => Promise<TStored> | TStored;
  revalidatePeriod: string;
  load: (client: PoolClient, url: string, parsed: TStored) => Promise<void>;
}

export interface HtmlLoader<TStored = any> {
  mode: 'text';
  buildRequest: (frontier: FrontierRow) => FetchRequestSpec;
  mapResponseToStatus?: (args: {
    httpStatus: number | null;
    body: string | null;
    error?: unknown;
  }) => FetchStatus;
  transformResponse?: (url: string, body: string) => Promise<TStored> | TStored;
  revalidatePeriod: string;
  load: (client: PoolClient, url: string, parsed: TStored) => Promise<void>;
}

export const defaultMapResponseToStatus: NonNullable<
  JsonLoader['mapResponseToStatus'] & HtmlLoader['mapResponseToStatus']
> = ({ error, httpStatus }) => {
  if (error) return 'error';
  if (httpStatus === 404) return 'gone';
  if (httpStatus && httpStatus >= 200 && httpStatus < 300) return 'ok';
  return 'error';
};
