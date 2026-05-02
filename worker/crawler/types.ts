import { z } from 'zod';
import type { IGetFrontierForUpdateResult } from './crawler.queries.ts';
import type { PoolClient } from 'pg';

export type FetchStatus = 'pending' | 'ok' | 'gone' | 'error';

export type FrontierRow = IGetFrontierForUpdateResult;

export type MapperArgs<T> = {
  httpStatus: number | null;
  error: string | null;
  parsed: T | null;
  raw: unknown;
};

interface LoaderBase<T> {
  buildRequest: (key: string) => { url: URL; init?: RequestInit };
  mapResponseToStatus?: (args: MapperArgs<T>) => FetchStatus | undefined;
  cleanResponse?: (url: URL, parsed: T, raw: unknown) => T;
  revalidatePeriod: string;
  load: (client: PoolClient, frontier: FrontierRow, parsed: T) => Promise<void>;
}

export interface JsonLoader<T = any> extends LoaderBase<T> {
  mode: 'json';
  schema: z.ZodType<T>;
}

export interface HtmlLoader<T = string> extends LoaderBase<T> {
  mode: 'text';
}

export type Loader<T = any> = JsonLoader<T> | HtmlLoader<T>;
