import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import process from 'node:process';
import { deepStrictEqual } from 'node:assert/strict';
import { cac } from 'cac';
import { Pool, type PoolClient } from 'pg';
import Cursor from 'pg-cursor';
import { zx } from '@traversable/zod';
import {
  getCrawlerBacklogStatus,
  getFrontierRefetchTarget,
  getFrontierKindStatus,
  getCrawlerControlJobStatus,
  getCrawlerFetchJobSummary,
  getCrawlerScheduleStatus,
  getFrontierDetail,
  getFrontierFailureGroups,
  getCrawlerFrontierJobs,
  getLatestFrontierFailures,
  getBacktestFrontierResponses,
  getFailedFrontierRefetchTargets,
  queueCrawlerKick,
  queueFrontierRefetch,
  queueFailedFrontierRefetch,
} from './crawler.queries.ts';
import type {
  IGetCrawlerBacklogStatusResult,
  IGetCrawlerFetchJobSummaryResult,
  IGetCrawlerScheduleStatusResult,
  IGetFrontierDetailResult,
  IGetFrontierFailureGroupsResult,
  IGetFrontierKindStatusResult,
} from './crawler.queries.ts';
import {
  buildStatusSnapshot,
  parseIntervalMs,
  type LoaderHealthMetadata,
} from './statusSnapshot.ts';
import { fetchResponse } from './fetch.ts';
import { loaderFor, LOADERS } from './handlers.ts';
import type { JsonLoader } from './types.ts';

const pool = new Pool();
const REQUEST_CACHE_DIR = resolve(import.meta.dirname, '..', '.requests');

type OutputOptions = { json?: boolean };
type ScopeTarget = {
  federation?: string;
  kind?: string;
};
type KindTarget = { federation: string; kind: string; key?: string };
type ExactTarget = { federation: string; kind: string; key: string };
type DetailLine = { label: string; value: unknown };

function formatDate(value: Date | string | null | undefined) {
  if (!value) return '-';
  const date = value instanceof Date ? value : new Date(value);
  if (Number.isNaN(date.getTime())) return String(value);
  return date.toISOString().replace(/\.\d{3}Z$/, 'Z');
}

function printTable(rows: Array<Record<string, unknown>>) {
  if (rows.length === 0) {
    console.log('(none)');
    return;
  }

  const columns = Object.keys(rows[0]);
  const widths = Object.fromEntries(
    columns.map((column) => [
      column,
      Math.max(column.length, ...rows.map((row) => String(row[column] ?? '').length)),
    ]),
  );

  const printRow = (row: Record<string, unknown>) => {
    console.log(
      columns
        .map((column) => String(row[column] ?? '').padEnd(widths[column]))
        .join('  '),
    );
  };

  printRow(Object.fromEntries(columns.map((column) => [column, column])));
  console.log(columns.map((column) => '-'.repeat(widths[column])).join('  '));
  for (const row of rows) printRow(row);
}

function printTableWithDetails(
  rows: Array<Record<string, unknown>>,
  detailForRow: (index: number) => DetailLine[],
) {
  if (rows.length === 0) {
    console.log('(none)');
    return;
  }

  const columns = Object.keys(rows[0]);
  const widths = Object.fromEntries(
    columns.map((column) => [
      column,
      Math.max(column.length, ...rows.map((row) => String(row[column] ?? '').length)),
    ]),
  );
  const rowText = (row: Record<string, unknown>) =>
    columns
      .map((column) => String(row[column] ?? '').padEnd(widths[column]))
      .join('  ');

  console.log(rowText(Object.fromEntries(columns.map((column) => [column, column]))));
  console.log(columns.map((column) => '-'.repeat(widths[column])).join('  '));

  rows.forEach((row, index) => {
    console.log(rowText(row));

    for (const { label, value } of detailForRow(index)) {
      const lines = String(value ?? '').split('\n');
      console.log(`  ${label}: ${lines[0] ?? ''}`);
      const indent = ' '.repeat(label.length + 4);
      for (const line of lines.slice(1)) console.log(`${indent}${line}`);
    }
  });
}

function printSection(title: string) {
  console.log();
  console.log(title);
}

function truncate(value: unknown, max: number) {
  const text = String(value ?? '');
  if (text.length <= max) return text;
  return `${text.slice(0, Math.max(0, max - 3))}...`;
}

function formatDetailText(value: unknown, full?: boolean, max = 500) {
  const text = String(value ?? '');
  return full ? text : truncate(text.replace(/\s+/g, ' '), max);
}

function detailLines(...lines: Array<DetailLine | null | undefined>) {
  return lines.filter((line): line is DetailLine => line != null);
}

function formatFrontierTarget(
  federation: string | null | undefined,
  kind: string | null | undefined,
  key?: string | null,
) {
  const federationText = String(federation ?? '');
  const kindText = String(kind ?? '');
  const keyText = String(key ?? '');
  return keyText ? `${federationText}:${kindText}:${keyText}` : `${federationText}:${kindText}`;
}

function fetchJobSummaryRow(row: IGetCrawlerFetchJobSummaryResult) {
  return {
    total: row.total ?? 0,
    ready: row.ready ?? 0,
    delayed: row.delayed ?? 0,
    locked: row.locked ?? 0,
    failed: row.failed ?? 0,
    oldest: formatDate(row.oldest_ready_at),
    next: formatDate(row.next_run_at),
    error: truncate(row.latest_error, 80) || '-',
  };
}

function scheduleRow(row: IGetCrawlerScheduleStatusResult) {
  return {
    host: row.host,
    limit: row.max_requests && row.per_interval ? `${row.max_requests}/${row.per_interval}` : '-',
    spacing: row.spacing ?? '-',
    queued: row.queued ?? 0,
    ready: row.ready ?? 0,
    delayed: row.delayed ?? 0,
    locked: row.locked ?? 0,
    available: formatDate(row.next_available_at),
    next: formatDate(row.next_run_at),
    tail: formatDate(row.queue_tail_at),
  };
}

function failureGroupRow(row: IGetFrontierFailureGroupsResult) {
  return {
    count: row.count,
    latest: formatDate(row.latest_failed_at),
    failure: row.failure,
    kind: formatFrontierTarget(row.federation, row.kind),
    http: row.http_status ?? '-',
    error: truncate(row.error_fingerprint, 90),
    samples: truncate(row.samples, 80),
  };
}

function parseLimit(value: unknown, fallback: number) {
  const limit = Number(value ?? fallback);
  if (!Number.isInteger(limit) || limit < 1) {
    throw new Error('Limit must be a positive integer');
  }
  return limit;
}

function parseOptionalLimit(value: unknown) {
  if (value == null || value === false || value === '') return null;
  return parseLimit(value, 1);
}

function parseHttpStatuses(value: unknown) {
  const codes = (Array.isArray(value) ? value : [value])
    .filter((part) => part != null && part !== false)
    .flatMap((part) => String(part).split(','))
    .map((part) => part.trim())
    .filter(Boolean);

  if (codes.length === 0) return null;

  return codes.map((code) => {
    const status = Number(code);
    if (!Number.isInteger(status) || status < 100 || status > 599) {
      throw new Error(`Invalid HTTP status code: ${code}`);
    }
    return status;
  });
}

function parseTargetParts(target: string | undefined, usage: string) {
  if (target == null) return undefined;

  const [federation, kind, ...keyParts] = target.split(':');
  if (!federation || kind === '') throw new Error(`Usage: ${usage}`);

  return { federation, kind, keyParts };
}

function parseScopeTarget(target: string | undefined, usage: string): ScopeTarget | undefined {
  const parsed = parseTargetParts(target, usage);
  if (!parsed) return undefined;
  if (parsed.keyParts.length > 0) throw new Error(`Usage: ${usage}`);
  return { federation: parsed.federation, kind: parsed.kind };
}

function parseKindTarget(target: string | undefined, usage: string): KindTarget {
  const parsed = parseTargetParts(target, usage);
  if (!parsed?.kind) throw new Error(`Usage: ${usage}`);
  return {
    federation: parsed.federation,
    kind: parsed.kind,
    key: parsed.keyParts.length > 0 ? parsed.keyParts.join(':') : undefined,
  };
}

function parseExactTarget(target: string, usage: string): ExactTarget {
  const parsed = parseKindTarget(target, usage);
  return { federation: parsed.federation, kind: parsed.kind, key: parsed.key ?? '' };
}

function parseExplainTarget(target: string) {
  if (/^\d+$/.test(target)) {
    return { id: target, federation: null, kind: null, key: null };
  }

  const { federation, kind, key } = parseExactTarget(
    target,
    'explain <id|federation:kind[:key]>',
  );
  return { id: null, federation, kind, key };
}

async function ensureCached(
  federation: string,
  kind: string,
  key: string,
  overwrite?: boolean,
) {
  const loader = loaderFor(federation, kind);
  if (!loader) throw new Error(`Unknown loader ${federation}:${kind}`);

  const path = resolve(REQUEST_CACHE_DIR, `${federation}:${kind}:${key}`);
  if (existsSync(path) && !overwrite) {
    console.log(`Using ${path}`);
    return { loader, path };
  }

  const { url, init = {} } = loader.buildRequest(key);
  const result = await fetchResponse(loader, url, init, { mode: 'strict' });
  console.log(`Fetched ${url} (${result.httpStatus}, ${result.fetchStatus})`);
  if (result.error) throw new Error(result.error);

  await mkdir(dirname(path), { recursive: true });
  await writeFile(path, `${JSON.stringify(result.content, null, 2)}`, 'utf8');
  return { loader, path };
}

async function backtestJsonResponses(
  federation: string,
  kind: string,
  loader: JsonLoader,
  options: { verbose?: boolean },
) {
  const strictSchema = zx.deepStrict(loader.schema);

  const conn = await pool.connect();
  let count = 0;
  try {
    const cursor = getBacktestFrontierResponses.stream(
      { federation, kind },
      {
        query: conn.query,
        stream: (query, bindings) => conn.query(new Cursor(query, bindings)),
      },
    );

    while (true) {
      const rows = await cursor.read(10);
      if (rows.length === 0) break;

      for (const row of rows) {
        let parsed: unknown;
        try {
          parsed = strictSchema.parse(row.content, { reportInput: true });
        } catch (e) {
          const message = [
            `Strict schema failed for ${federation}:${kind} frontier ${row.id} (${row.url})`,
            e instanceof Error ? e.stack || e.message : String(e),
            options.verbose ? JSON.stringify(row.content, null, 2) : undefined,
          ].join('\n');
          throw new Error(message);
        }

        try {
          const reparsed = strictSchema.parse(parsed, { reportInput: true });
          deepStrictEqual(reparsed, parsed);
        } catch (e) {
          const message = [
            `Strict schema is not idempotent for ${federation}:${kind} frontier ${row.id} (${row.url})`,
            e instanceof Error ? e.stack || e.message : String(e),
            options.verbose ? JSON.stringify(row.content, null, 2) : undefined,
          ].join('\n');
          throw new Error(message);
        }
        count++;
      }
    }
  } finally {
    conn.release();
  }
  return count;
}

async function backtestSchemas(
  federation: string | undefined,
  kind: string | undefined,
  options: { all?: boolean; verbose?: boolean },
) {
  if (!options.all) {
    if (!federation || !kind) {
      throw new Error('Usage: backtest <federation:kind> or backtest --all');
    }

    const loader = loaderFor(federation, kind);
    if (!loader) throw new Error(`Unknown loader ${federation}:${kind}`);
    if (loader.mode !== 'json') {
      throw new Error(`Backtest only supports JSON loaders (${federation}:${kind})`);
    }

    const count = await backtestJsonResponses(federation, kind, loader, {
      verbose: options.verbose,
    });
    console.log(`Validated ${count} responses of type ${federation}:${kind}`);
    return;
  }

  if (federation || kind) {
    throw new Error('Use either `backtest --all` or `backtest <federation:kind>`');
  }

  let total = 0;
  let jsonLoaders = 0;
  for (const row of await getFrontierKindStatus.run({ allowRefetch: false }, pool)) {
    const { federation, kind } = row;
    const loader = loaderFor(federation, kind);
    if (!loader) {
      console.log(`Skipping unknown loader ${federation}:${kind} (${row.total})`);
      continue;
    }
    if (loader.mode !== 'json') continue;

    const count = await backtestJsonResponses(federation, kind, loader, {
      verbose: options.verbose,
    });
    jsonLoaders++;
    total += count;
    console.log(`Validated ${count} responses of type ${federation}:${kind}`);
  }
  console.log(`Validated ${total} responses across ${jsonLoaders} JSON loaders`);
}

type FrontierStatusRow = IGetFrontierKindStatusResult & {
  has_loader: boolean;
  is_problem: boolean;
  problem_score: number;
};

type StatusOptions = OutputOptions & {
  limit?: string;
  unknown?: boolean;
  problems?: boolean;
  summary?: boolean;
  frontiers?: boolean;
  backlog?: boolean;
  failures?: boolean;
  jobs?: boolean;
  schedule?: boolean;
  commands?: boolean;
  excludeCode?: string | string[];
};

function enrichFrontierRows(rows: IGetFrontierKindStatusResult[]): FrontierStatusRow[] {
  return rows
    .map((row) => {
      const hasLoader = Boolean(loaderFor(row.federation, row.kind));
      const fetchTransient = row.fetch_transient ?? 0;
      const fetchError = row.fetch_error ?? 0;
      const processError = row.process_error ?? 0;
      const isProblem = !hasLoader || fetchTransient > 0 || fetchError > 0 || processError > 0;
      const problemScore =
        (hasLoader ? 0 : 1_000_000) + processError * 100 + fetchError * 10 + fetchTransient;

      return { ...row, has_loader: hasLoader, is_problem: isProblem, problem_score: problemScore };
    })
    .sort((a, b) => b.problem_score - a.problem_score);
}

function limitRows<T>(rows: T[], limit: number | null) {
  return limit == null ? rows : rows.slice(0, limit);
}

function statusSections(options: StatusOptions) {
  const selected = Boolean(
    options.summary ||
      options.problems ||
      options.frontiers ||
      options.backlog ||
      options.failures ||
      options.jobs ||
      options.schedule ||
      options.commands,
  );

  return {
    summary: !selected || Boolean(options.summary),
    problems: !selected || Boolean(options.problems),
    frontiers: !selected || Boolean(options.frontiers),
    backlog: !selected || Boolean(options.backlog),
    failures: !selected || Boolean(options.failures),
    jobs: !selected || Boolean(options.jobs),
    schedule: !selected || Boolean(options.schedule),
    commands: !selected || Boolean(options.commands),
  };
}

function frontierTableRows(rows: FrontierStatusRow[]) {
  return rows.map((row) => ({
    kind: formatFrontierTarget(row.federation, row.kind),
    loader: row.has_loader ? 'yes' : 'no',
    total: row.total ?? 0,
    'response rows': row.response_rows ?? 0,
    due: row.fetch_due ?? 0,
    'fetch p/t/o/g/e': [
      row.fetch_pending ?? 0,
      row.fetch_transient ?? 0,
      row.fetch_ok ?? 0,
      row.fetch_gone ?? 0,
      row.fetch_error ?? 0,
    ].join('/'),
    'process p/o/e': [
      row.process_pending ?? 0,
      row.process_ok ?? 0,
      row.process_error ?? 0,
    ].join('/'),
    latest: formatDate(row.latest),
    keys: row.keys ?? '-',
  }));
}

async function showFailures(
  federation: string | undefined,
  kind: string | undefined,
  options: OutputOptions & {
    limit?: string;
    full?: boolean;
    group?: boolean;
    excludeCode?: string | string[];
  },
) {
  if (kind && !federation) {
    throw new Error('Usage: failures [federation[:kind]]');
  }

  const excludeHttpStatuses = parseHttpStatuses(options.excludeCode);
  const limit = parseLimit(options.limit, options.group ? 50 : 20);

  if (options.group) {
    const groups = await getFrontierFailureGroups.run(
      { federation, kind, limit, excludeHttpStatuses },
      pool,
    );

    if (options.json) {
      console.log(JSON.stringify(groups, null, 2));
      return;
    }

    printTable(groups.map(failureGroupRow));
    return;
  }

  const rows = await getLatestFrontierFailures.run(
    { federation, kind, limit, excludeHttpStatuses },
    pool,
  );
  const displayRows = rows.map((row) => {
    const details = detailLines(
      row.response_error ? {
        label: 'fetch',
        value: formatDetailText(row.response_error, options.full),
      } : null,
      row.process_error ? {
        label: 'process',
        value: formatDetailText(row.process_error, options.full),
      } : null,
    );

    if (details.length === 0) {
      details.push({
        label: 'error',
        value: row.process_status === 'error' ? 'Processing failed without a stored error' : '-',
      });
    }

    return { ...row, details };
  });

  if (options.json) {
    console.log(JSON.stringify(displayRows, null, 2));
    return;
  }

  const short = (value: unknown, max: number) =>
    options.full ? String(value ?? '') : truncate(value, max);

  printTableWithDetails(
    displayRows.map((row) => ({
      failed: formatDate(row.failed_at),
      failure: row.failure,
      kind: `${row.federation}:${row.kind}`,
      id: row.id,
      key: short(row.key, 28),
      http: row.http_status ?? '-',
      errors: row.error_count,
      next: formatDate(row.next_fetch_at),
      url: short(row.url, 80) || '-',
    })),
    (index) => displayRows[index].details,
  );
}

type JobOptions = OutputOptions & {
  limit?: string;
  failed?: boolean;
  active?: boolean;
  all?: boolean;
  target?: string;
  full?: boolean;
};

function jobStateFromOptions(options: JobOptions): 'failed' | 'active' | 'all' | null {
  const selected = [options.failed, options.active, options.all].filter(Boolean).length;
  if (selected > 1) throw new Error('Use only one of --failed, --active, or --all');
  if (options.failed) return 'failed';
  if (options.active) return 'active';
  if (options.all) return 'all';
  return null;
}

async function getVisibleFrontierJobs(
  options: JobOptions,
  target: ScopeTarget | undefined,
) {
  const limit = parseLimit(options.limit, 20);
  const selectedMode = jobStateFromOptions(options);
  if (selectedMode) {
    const rows = await getCrawlerFrontierJobs.run(
      { federation: target?.federation, kind: target?.kind, mode: selectedMode, limit },
      pool,
    );
    return { mode: selectedMode, rows };
  }

  const failedRows = await getCrawlerFrontierJobs.run(
    { federation: target?.federation, kind: target?.kind, mode: 'failed', limit },
    pool,
  );
  if (failedRows.length > 0) return { mode: 'failed' as const, rows: failedRows };

  const activeRows = await getCrawlerFrontierJobs.run(
    { federation: target?.federation, kind: target?.kind, mode: 'active', limit },
    pool,
  );
  return { mode: 'active' as const, rows: activeRows };
}

async function showJobs(options: JobOptions) {
  const target = parseScopeTarget(options.target, 'jobs [--target federation[:kind]]');
  const [fetchJobRows, frontierJobs] = await Promise.all([
    getCrawlerFetchJobSummary.run(undefined, pool),
    getVisibleFrontierJobs(options, target),
  ]);
  const [fetchJob] = fetchJobRows;
  if (!fetchJob) throw new Error('GetCrawlerFetchJobSummary returned no rows');

  if (options.json) {
    console.log(JSON.stringify({ fetch_job: fetchJob, frontier_jobs: frontierJobs.rows }, null, 2));
    return;
  }

  printSection('Fetch queue');
  printTable([fetchJobSummaryRow(fetchJob)]);

  printSection(`Frontier jobs (${frontierJobs.mode})`);
  const short = (value: unknown, max: number) =>
    options.full ? String(value ?? '') : truncate(value, max);
  const formatContent = (value: unknown) => {
    if (value == null) return null;
    const text = typeof value === 'string' ? value : JSON.stringify(value);
    return formatDetailText(text, options.full);
  };
  const displayRows = frontierJobs.rows.map((row) => {
    const content = formatContent(row.response_content);
    const details = detailLines(
      row.job_error ? { label: 'job', value: formatDetailText(row.job_error, options.full) } : null,
      row.response_error ? {
        label: 'fetch',
        value: formatDetailText(row.response_error, options.full),
      } : null,
      row.process_error ? {
        label: 'process',
        value: formatDetailText(row.process_error, options.full),
      } : null,
      content ? {
        label: 'content',
        value: content,
      } : null,
    );

    return { ...row, details };
  });

  printTableWithDetails(
    displayRows.map((row) => ({
      state: row.state,
      job: row.job_id,
      target: short(formatFrontierTarget(row.federation, row.kind, row.frontier_key), 42),
      frontier: row.frontier_id,
      attempts: `${row.attempts}/${row.max_attempts}`,
      run: formatDate(row.run_at),
      locked: formatDate(row.locked_at),
      fetch: row.fetch_status,
      process: row.process_status,
      http: row.response_http_status ?? '-',
      updated: formatDate(row.job_updated_at),
    })),
    (index) => displayRows[index].details,
  );
}

function collectLoaderMetadata(
  statusRows: IGetFrontierKindStatusResult[],
  backlogRows: IGetCrawlerBacklogStatusResult[],
) {
  const sampleKeys = new Map<string, string | null>();
  for (const row of backlogRows) {
    sampleKeys.set(formatFrontierTarget(row.federation, row.kind), row.sample_due_key ?? null);
  }

  const seen = new Set<string>();
  const rows: LoaderHealthMetadata[] = [];
  for (const row of statusRows) {
    const id = `${row.federation}:${row.kind}`;
    if (seen.has(id)) continue;
    seen.add(id);

    const loader = loaderFor(row.federation, row.kind);
    if (!loader) {
      rows.push({
        target: id,
        has_loader: false,
        host: null,
        revalidate_period: null,
        revalidate_period_ms: null,
      });
      continue;
    }

    const sampleKey = sampleKeys.get(id);
    let host: string | null = null;
    let error: string | undefined;
    if (sampleKey != null) {
      try {
        host = loader.buildRequest(sampleKey).url.host;
      } catch (e) {
        error = e instanceof Error ? e.message : String(e);
      }
    }

    rows.push({
      target: id,
      has_loader: true,
      host,
      revalidate_period: loader.revalidatePeriod,
      revalidate_period_ms: parseIntervalMs(loader.revalidatePeriod),
      error,
    });
  }

  return rows;
}

function printStatusSummary(summary: {
  kinds: number;
  frontiers: number;
  fetch_due: number;
  unscheduled_fetch: number;
  process_ready: number;
  stale_kinds: number;
  unknown_loaders: number;
  fetch_errors: number;
  fetch_transient: number;
  process_errors: number;
  fetch_job_failures: number;
}) {
  printSection('Summary');
  printTable([
    {
      kinds: summary.kinds,
      frontiers: summary.frontiers,
      due: summary.fetch_due,
      unscheduled: summary.unscheduled_fetch,
      process: summary.process_ready,
      stale: summary.stale_kinds,
      unknown: summary.unknown_loaders,
      'fetch e/t': `${summary.fetch_errors}/${summary.fetch_transient}`,
      'process e': summary.process_errors,
      'fetch job e': summary.fetch_job_failures,
    },
  ]);
}

function printStatusProblems(
  rows: Array<{
    severity: string;
    code: string;
    kind?: string;
    task?: string | null;
    host?: string | null;
    count: number;
    at?: Date | string | null;
    sample_id?: string | null;
    message: string;
  }>,
) {
  printSection('Problems');
  printTable(
    rows.map((row) => ({
      severity: row.severity,
      code: row.code,
      kind: row.kind ?? row.task ?? '-',
      host: row.host ?? '-',
      count: row.count,
      at: formatDate(row.at),
      sample: row.sample_id ?? '-',
      message: truncate(row.message, 90),
    })),
  );
}

function printStatusBacklog(
  rows: Array<IGetCrawlerBacklogStatusResult & {
    host: string | null;
    deadline_at: Date | string | null;
    eta_at: Date | string | null;
    is_stale: boolean;
  }>,
) {
  printSection('Backlog');
  printTable(
    rows.map((row) => ({
      kind: formatFrontierTarget(row.federation, row.kind),
      host: row.host ?? '-',
      due: row.fetch_due ?? 0,
      unscheduled: row.unscheduled_fetch ?? 0,
      queued: row.queued_fetch ?? 0,
      locked: row.locked_fetch ?? 0,
      process: row.process_ready ?? 0,
      oldest: formatDate(row.oldest_due_at),
      deadline: formatDate(row.deadline_at),
      eta: formatDate(row.eta_at),
      stale: row.is_stale ? 'yes' : 'no',
    })),
  );
}

function printCommands(commands: string[]) {
  printSection('Next commands');
  if (commands.length === 0) {
    console.log('(none)');
    return;
  }
  for (const command of commands) console.log(command);
}

async function showStatus(
  federation: string | undefined,
  kind: string | undefined,
  options: StatusOptions,
) {
  if (kind && !federation) {
    throw new Error('Usage: status [federation[:kind]]');
  }

  const limit = parseOptionalLimit(options.limit);
  const excludeHttpStatuses = parseHttpStatuses(options.excludeCode);
  const allowRefetch = process.env.CRAWLER_DISABLE_REFETCH !== 'true';
  const generatedAt = new Date();

  const [
    statusRows,
    backlogRows,
    failureGroups,
    fetchJobRows,
    controlJobRows,
    scheduleRows,
  ] = await Promise.all([
    getFrontierKindStatus.run({ federation, kind, allowRefetch }, pool),
    getCrawlerBacklogStatus.run({ federation, kind, allowRefetch }, pool),
    getFrontierFailureGroups.run(
      { federation, kind, limit, excludeHttpStatuses },
      pool,
    ),
    getCrawlerFetchJobSummary.run(undefined, pool),
    getCrawlerControlJobStatus.run(undefined, pool),
    getCrawlerScheduleStatus.run(undefined, pool),
  ]);
  const [fetchJob] = fetchJobRows;
  if (!fetchJob) throw new Error('GetCrawlerFetchJobSummary returned no rows');

  const snapshot = buildStatusSnapshot({
    generated_at: generatedAt,
    filter: { federation, kind },
    status_rows: statusRows,
    backlog_rows: backlogRows,
    failure_groups: failureGroups,
    fetch_job: fetchJob,
    control_jobs: controlJobRows,
    schedule_rows: scheduleRows,
    loader_metadata: collectLoaderMetadata(statusRows, backlogRows),
  });

  const sections = statusSections(options);
  const frontiers = limitRows(
    enrichFrontierRows(snapshot.frontiers)
      .filter((row) => !options.unknown || !row.has_loader)
      .filter((row) => !options.problems || row.is_problem),
    limit,
  );
  const visibleBacklog = limitRows(
    snapshot.backlog
      .filter((row) => !options.unknown || !row.has_loader)
      .filter(
        (row) =>
          (row.fetch_due ?? 0) > 0 ||
          (row.process_ready ?? 0) > 0 ||
          row.is_stale ||
          (options.unknown && !row.has_loader),
      ),
    limit,
  );
  const visibleProblems = limitRows(
    snapshot.problems.filter((row) => !options.unknown || row.code === 'unknown_loader'),
    limit,
  );
  const visibleFailures = limitRows(snapshot.failures, limit);
  const visibleSchedule = limitRows(snapshot.schedule, limit);

  if (options.json) {
    console.log(JSON.stringify({
      ...snapshot,
      problems: visibleProblems,
      frontiers,
      backlog: visibleBacklog,
      failures: visibleFailures,
      schedule: visibleSchedule,
    }, null, 2));
    return;
  }

  console.log(
    [
      `Crawler status ${snapshot.generated_at}`,
      federation ? `filter ${federation}${kind ? `:${kind}` : ''}` : null,
    ]
      .filter(Boolean)
      .join('  '),
  );

  if (sections.summary) printStatusSummary(snapshot.summary);
  if (sections.problems) printStatusProblems(visibleProblems);
  if (sections.frontiers) {
    printSection('Frontiers');
    printTable(frontierTableRows(frontiers));
  }
  if (sections.backlog) printStatusBacklog(visibleBacklog);
  if (sections.failures) {
    printSection('Failure groups');
    printTable(visibleFailures.map(failureGroupRow));
  }
  if (sections.jobs) {
    printSection('Fetch queue');
    printTable([fetchJobSummaryRow(snapshot.fetch_job)]);
  }
  if (sections.schedule) {
    printSection('Schedule');
    printTable(visibleSchedule.map(scheduleRow));
  }
  if (sections.commands) printCommands(snapshot.suggested_commands);
}

function explainState(row: IGetFrontierDetailResult) {
  const hasLoader = Boolean(loaderFor(row.federation, row.kind));
  if (!hasLoader) return 'unknown loader';
  if (row.process_status === 'error') return 'processing failed';
  if (row.fetch_status === 'error') return 'fetch failed';
  if (row.fetch_status === 'transient') return 'transient fetch failure; retry scheduled';
  if (row.fetch_status === 'pending' && row.job_run_at) return 'fetch job scheduled';
  if (row.fetch_status === 'pending') return 'fetch pending without a queued job';
  if (row.fetch_status === 'gone') return 'gone; no processing needed';
  if (row.process_status === 'pending' && row.fetch_status === 'ok') {
    return 'waiting for processing';
  }
  if (row.fetch_status === 'ok' && row.process_status === 'ok') return 'healthy';
  return 'no obvious issue';
}

function explainNextAction(row: IGetFrontierDetailResult) {
  const target = formatFrontierTarget(row.federation, row.kind, row.key);
  const hasLoader = Boolean(loaderFor(row.federation, row.kind));
  if (!hasLoader) return `add or restore loader ${row.federation}:${row.kind}`;
  if (row.job_attempts != null && row.job_max_attempts != null && row.job_attempts >= row.job_max_attempts) {
    return `inspect job with pnpm crawler jobs --failed --target ${row.federation}:${row.kind}`;
  }
  if (row.fetch_status === 'error' || row.fetch_status === 'transient' || row.process_status === 'error') {
    return `pnpm crawler refetch ${target}`;
  }
  if (
    row.fetch_status === 'pending' &&
    !row.job_run_at &&
    (row.next_fetch_at == null || row.next_fetch_at <= new Date())
  ) {
    return 'pnpm crawler kick';
  }
  if (row.process_status === 'pending' && row.fetch_status === 'ok') {
    return 'pnpm crawler kick';
  }
  return '-';
}

async function explainFrontier(target: string, options: OutputOptions) {
  const parsed = parseExplainTarget(target);
  const [row] = await getFrontierDetail.run(parsed, pool);
  if (!row) throw new Error(`Frontier ${target} not found`);

  const loader = loaderFor(row.federation, row.kind);
  const expectedUrl = loader?.buildRequest(row.key).url.toString() ?? null;
  const result = {
    ...row,
    has_loader: Boolean(loader),
    expected_url: expectedUrl,
    state: explainState(row),
    next_action: explainNextAction(row),
  };

  if (options.json) {
    console.log(JSON.stringify(result, null, 2));
    return;
  }

  printTable([
    {
      id: result.id,
      kind: `${result.federation}:${result.kind}`,
      key: truncate(result.key, 42),
      loader: result.has_loader ? 'yes' : 'no',
      fetch: result.fetch_status,
      process: result.process_status,
      errors: result.error_count,
      next: formatDate(result.next_fetch_at),
      state: result.state,
      next_action: truncate(result.next_action, 70),
    },
  ]);

  printTable(
    [
      {
        response: formatDate(result.response_fetched_at),
        http: result.response_http_status ?? '-',
        job: result.job_id ?? '-',
        job_run: formatDate(result.job_run_at),
        job_attempts:
          result.job_attempts == null || result.job_max_attempts == null
            ? '-'
            : `${result.job_attempts}/${result.job_max_attempts}`,
      },
    ],
  );

  if (result.response_error) console.log(`response error: ${result.response_error}`);
  if (result.last_process_error) console.log(`process error: ${result.last_process_error}`);
  if (result.job_last_error) console.log(`job error: ${result.job_last_error}`);
  if (result.expected_url) console.log(`request: ${result.expected_url}`);
}

type RefetchOptions = OutputOptions & {
  failed?: boolean;
  httpStatus?: string | string[];
  errorContains?: string;
  limit?: string;
  dryRun?: boolean;
  commit?: boolean;
};

async function queueExactRefetch(target: string, options: RefetchOptions) {
  const { federation, kind, key } = parseExactTarget(
    target,
    'refetch <federation:kind[:key]>',
  );
  const loader = loaderFor(federation, kind);
  if (!loader) throw new Error(`Unknown loader ${federation}:${kind}`);

  const [frontier] = await getFrontierRefetchTarget.run({ federation, kind, key }, pool);
  if (!frontier) {
    throw new Error(
      `Frontier ${target} not found; use federation:kind:key, or --failed for filtered failed rows`,
    );
  }

  if (options.dryRun) {
    printTable([
      {
        action: 'would refetch',
        id: frontier.id,
        target: formatFrontierTarget(frontier.federation, frontier.kind, frontier.key),
      },
    ]);
    return;
  }

  const [row] = await queueFrontierRefetch.run({ id: frontier.id }, pool);
  if (!row) throw new Error(`Unable to queue ${target}`);

  console.log(
    [
      `Queued frontier_schedule job ${row.job_id} after marking frontier ${row.id} due`,
      formatFrontierTarget(row.federation, row.kind, row.key),
    ].join('\n'),
  );
}

async function queueFailedRefetch(target: string, options: RefetchOptions) {
  const parsed = parseKindTarget(
    target,
    'refetch <federation:kind[:key]> --failed [--http-status <codes>] [--error-contains <text>]',
  );
  if (options.commit && options.dryRun) {
    throw new Error('Use either --commit or --dry-run, not both');
  }

  const httpStatuses = parseHttpStatuses(options.httpStatus);
  const errorContains = options.errorContains?.trim() || null;
  const key = parsed.key ?? null;

  const limit = parseOptionalLimit(options.limit);
  const params = {
    federation: parsed.federation,
    kind: parsed.kind,
    key,
    httpStatuses,
    errorContains,
    limit,
  };
  const shouldCommit = Boolean(options.commit);
  const rows = shouldCommit
    ? await queueFailedFrontierRefetch.run(params, pool)
    : await getFailedFrontierRefetchTargets.run(params, pool);

  if (options.json) {
    console.log(JSON.stringify({ committed: shouldCommit, rows }, null, 2));
    return;
  }

  console.log(
    `${shouldCommit ? 'Queued' : 'Would refetch'} ${rows.length} failed frontier rows` +
      (limit == null ? '' : ` (limit ${limit})`),
  );
  const visibleRows = rows.slice(0, 50);
  printTable(
    visibleRows.map((row) => ({
      action: shouldCommit ? 'refetch queued' : 'would refetch',
      failed: formatDate(row.failed_at),
      id: row.id,
      target: formatFrontierTarget(row.federation, row.kind, row.key),
      http: row.http_status ?? '-',
      error: truncate(row.error, 90),
      job: 'job_id' in row ? row.job_id : '-',
    })),
  );
  if (rows.length > visibleRows.length) {
    console.log(`Showing ${visibleRows.length} of ${rows.length}; use --json for all rows.`);
  }
  if (!shouldCommit) {
    console.log('Dry run; add --commit to mark these rows due and trigger frontier_schedule.');
  }
}

async function queueRefetch(target: string, options: RefetchOptions) {
  if (options.failed) {
    await queueFailedRefetch(target, options);
    return;
  }

  await queueExactRefetch(target, options);
}

async function kickScheduler(options: OutputOptions & { dryRun?: boolean }) {
  if (options.dryRun) {
    const result = { action: 'would enqueue frontier_schedule' };
    if (options.json) console.log(JSON.stringify(result, null, 2));
    else printTable([result]);
    return;
  }

  const [row] = await queueCrawlerKick.run(undefined, pool);
  if (options.json) {
    console.log(JSON.stringify(row, null, 2));
    return;
  }
  console.log(`Queued frontier_schedule job ${row.job_id}`);
}

function queryText(query: unknown) {
  const text =
    typeof query === 'string'
      ? query
      : query && typeof query === 'object' && 'text' in query
        ? query.text
        : '<unknown query>';
  return String(text).replace(/\s+/g, ' ').trim();
}

function logClientQueries(client: PoolClient): PoolClient {
  return new Proxy(client, {
    get(target, prop, receiver) {
      if (prop !== 'query') {
        const value = Reflect.get(target, prop, receiver);
        return typeof value === 'function' ? value.bind(target) : value;
      }

      return async (...args: unknown[]) => {
        const started = performance.now();
        const text = queryText(args[0]);
        try {
          const result = await (
            target.query as (
              ...queryArgs: unknown[]
            ) => Promise<{ rowCount?: number | null }>
          )(...args);
          const ms = Math.round(performance.now() - started);
          console.log(`${ms}ms rows=${result.rowCount ?? 0} ${text}`);
          return result;
        } catch (e) {
          const ms = Math.round(performance.now() - started);
          const message = e instanceof Error ? e.message : String(e);
          console.error(`ERR ${ms}ms ${text} ${message}`);
          throw e;
        }
      };
    },
  });
}

async function loadCached(
  federation: string,
  kind: string,
  key: string,
  shouldCommit: boolean,
) {
  const { loader, path } = await ensureCached(federation, kind, key);
  const body = await readFile(path, 'utf8');
  const raw = JSON.parse(body);

  const content =
    loader.mode === 'json'
      ? zx.deepLoose(loader.schema).parse(raw, { reportInput: true })
      : raw;
  const client = logClientQueries(await pool.connect());
  try {
    await client.query('BEGIN');
    await loader.load(client, content);
    await client.query(shouldCommit ? 'COMMIT' : 'ROLLBACK');
    console.log(`${shouldCommit ? 'Committed' : 'Rolled back'} ${path}`);
  } catch (e) {
    await client.query('ROLLBACK');
    throw e;
  } finally {
    client.release();
  }
}

const cli = cac('crawler');

cli.command('list', 'List crawler loaders').action(() => {
  for (const [federation, kinds] of Object.entries(LOADERS)) {
    for (const kind of Object.keys(kinds).sort()) {
      console.log(`${federation}:${kind}`);
    }
    console.log();
  }
});

cli
  .command('status [target]', 'Show crawler health status')
  .option('--summary', 'Only show the summary section')
  .option('--problems', 'Only show the problems section and filter problem-aware sections')
  .option('--frontiers', 'Only show the frontier kind status section')
  .option('--backlog', 'Only show the backlog section')
  .option('--failures', 'Only show grouped failure summaries')
  .option('--jobs', 'Only show fetch queue health')
  .option('--schedule', 'Only show fetch scheduling and rate-limit state')
  .option('--commands', 'Only show suggested next commands')
  .option('--unknown', 'Only show unknown-loader rows in problem-aware sections')
  .option('-n, --limit <limit>', 'Maximum rows to show per list section')
  .option('--exclude-code <codes>', 'Hide HTTP status codes in failure groups, comma-separated')
  .option('--json', 'Print JSON')
  .action(
    (
      target: string | undefined,
      options: StatusOptions,
    ) => {
      const parsed = parseScopeTarget(target, 'status [federation[:kind]]');
      return showStatus(parsed?.federation, parsed?.kind, options);
    },
  );

cli
  .command('failures [target]', 'Show latest failed frontier rows')
  .option('-n, --limit <limit>', 'Maximum rows to show', { default: '20' })
  .option('--exclude-code <codes>', 'Hide HTTP status codes, comma-separated')
  .option('--group', 'Group failures by kind, stage, HTTP status, and error fingerprint')
  .option('--full', 'Do not truncate long keys or URLs')
  .option('--json', 'Print JSON')
  .action(
    (
      target: string | undefined,
      options: OutputOptions & {
        limit?: string;
        full?: boolean;
        group?: boolean;
        excludeCode?: string | string[];
      },
    ) => {
      const parsed = parseScopeTarget(target, 'failures [federation[:kind]]');
      return showFailures(parsed?.federation, parsed?.kind, options);
    },
  );

cli
  .command('jobs', 'Show crawler worker job health')
  .option('--failed', 'Show exhausted frontier_fetch jobs')
  .option('--active', 'Show ready, delayed, or locked frontier_fetch jobs')
  .option('--all', 'Show all frontier_fetch jobs')
  .option('--target <target>', 'Only show frontier jobs for federation[:kind]')
  .option('-n, --limit <limit>', 'Maximum frontier jobs to show', { default: '20' })
  .option('--full', 'Do not truncate long targets or errors')
  .option('--json', 'Print JSON')
  .action((options: JobOptions) => showJobs(options));

cli
  .command('kick', 'Wake the crawler scheduler')
  .option('--dry-run', 'Show what would be queued without changing jobs')
  .option('--json', 'Print JSON')
  .action((options: OutputOptions & { dryRun?: boolean }) => kickScheduler(options));

cli
  .command('explain <target>', 'Explain one frontier row by id or federation:kind[:key]')
  .option('--json', 'Print JSON')
  .action((target: string, options: OutputOptions) => explainFrontier(target, options));

cli
  .command('fetch <target>', 'Fetch and cache a response')
  .action((target: string) => {
    const { federation, kind, key } = parseExactTarget(
      target,
      'fetch <federation:kind[:key]>',
    );
    return ensureCached(federation, kind, key, true);
  });

cli
  .command('refetch <target>', 'Queue federation:kind[:key] for immediate re-fetch')
  .option('--failed', 'Select failed rows under the target instead of one exact frontier')
  .option('--http-status <codes>', 'Only include latest responses with these status codes')
  .option('--error-contains <text>', 'Only include failures whose stored error contains text')
  .option('-n, --limit <limit>', 'Maximum failed rows to include')
  .option('--dry-run', 'Show rows without changing frontier state')
  .option('--commit', 'Apply a failed-row refetch selection')
  .option('--json', 'Print JSON for failed-row selections')
  .action((target: string, options: RefetchOptions) => queueRefetch(target, options));

cli
  .command('backtest [target]', 'Strict-validate cached JSON responses for a loader')
  .option('--all', 'Strict-validate cached JSON responses for all JSON loaders')
  .option('-v, --verbose', 'Include full input in schema failure logs')
  .action(
    (
      target: string | undefined,
      options: { all?: boolean; verbose?: boolean },
    ) => {
      const parsed = parseScopeTarget(
        target,
        'backtest <federation:kind> or backtest --all',
      );
      return backtestSchemas(parsed?.federation, parsed?.kind, options);
    },
  );

cli
  .command('load <target>', 'Load a cached response')
  .option('--commit', 'Commit instead of rolling back')
  .action(
    (
      target: string,
      options: { commit?: boolean },
    ) => {
      const { federation, kind, key } = parseExactTarget(
        target,
        'load <federation:kind[:key]>',
      );
      return loadCached(federation, kind, key, Boolean(options.commit));
    },
  );

cli.help();

try {
  cli.parse(undefined, { run: false });
  await cli.runMatchedCommand();
} catch (e) {
  console.error(e instanceof Error ? e.stack || e.message : String(e));
  process.exitCode = 1;
} finally {
  await pool.end();
}
