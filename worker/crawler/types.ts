import { z } from 'zod';
import type { IGetFrontierForUpdateResult } from './crawler.queries.ts';
import type { PoolClient } from 'pg';

export type FetchStatus = 'pending' | 'ok' | 'gone' | 'error';

export type FrontierRow = IGetFrontierForUpdateResult;

export interface JsonLoader<TStored = any, TParsed = TStored> {
  mode: 'json';
  responseSchema: z.ZodType<TParsed>;
  storedSchema: z.ZodType<TStored>;
  buildRequest: (frontier: FrontierRow) => {
    url: string;
    init?: RequestInit;
  };
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
  load: (client: PoolClient, frontier: FrontierRow, parsed: TStored) => Promise<void>;
}

export interface HtmlLoader {
  mode: 'text';
  buildRequest: (frontier: FrontierRow) => {
    url: string;
    init?: RequestInit;
  };
  mapResponseToStatus?: (args: {
    httpStatus: number | null;
    body: string | null;
    error?: unknown;
  }) => FetchStatus;
  transformResponse?: (url: string, body: string) => Promise<string> | string;
  revalidatePeriod: string;
  load: (client: PoolClient, frontier: FrontierRow, body: string) => Promise<void>;
}

export const defaultMapResponseToStatus: NonNullable<
  JsonLoader['mapResponseToStatus'] & HtmlLoader['mapResponseToStatus']
> = ({ error, httpStatus }) => {
  if (error) return 'error';
  if (httpStatus === 404) return 'gone';
  if (httpStatus && httpStatus >= 200 && httpStatus < 300) return 'ok';
  return 'error';
};
