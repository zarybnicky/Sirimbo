import { z } from 'zod';
import type { fetch_status, IGetFrontierForUpdateResult } from './crawler.queries.ts';
import type { PoolClient } from 'pg';

export type FrontierRow = IGetFrontierForUpdateResult;

type MapperArgs<T> = {
  fetchStatus: fetch_status;
  error: string | null;
  parsed: T | null;
  raw: unknown;
};

interface LoaderBase<T> {
  buildRequest: (key: string) => { url: URL; init?: RequestInit };
  mapResponseToStatus?: (args: MapperArgs<T>) => fetch_status | undefined;
  cleanResponse?: (url: URL, parsed: T, raw: unknown) => T;
  revalidatePeriod: string;
  load: (client: PoolClient, parsed: T) => Promise<void>;
}

export interface JsonLoader<T = unknown> extends LoaderBase<T> {
  mode: 'json';
  schema: z.ZodType<T>;
}

export interface HtmlLoader<T = string> extends LoaderBase<T> {
  mode: 'text';
}

export type Loader<T = any> = JsonLoader<T> | HtmlLoader<T>;
