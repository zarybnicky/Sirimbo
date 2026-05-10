import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import process from 'node:process';
import { deepStrictEqual } from 'node:assert/strict';
import { cac } from 'cac';
import { Pool, type PoolClient } from 'pg';
import Cursor from 'pg-cursor';
import { zx } from '@traversable/zod';
import type {
  IGetCrawlerScheduleStatusResult,
  IGetFrontierDetailResult,
  IGetFrontierFailureGroupsResult,
} from './crawler.queries.ts';
import {
  getBacktestFrontierKinds,
  getBacktestFrontierResponses,
  getCrawlerFrontierJobs,
  getCrawlerFrontierStatus,
  getCrawlerScheduleStatus,
  getCrawlerWorkerJobStatus,
  getFrontierDetail,
  getFrontierFailureGroups,
  getLatestFrontierFailures,
  getLatestFrontierResponse,
  queueCrawlerSchedule,
  queueFrontierRefetch,
} from './crawler.queries.ts';
import {
  buildStatusSnapshot,
  type CrawlerStatusRow,
  sortBacklogRows,
  type StatusProblem,
} from './statusSnapshot.ts';
import { fetchResponse } from './fetch.ts';
import { loaderFor, LOADERS } from './handlers.ts';
import type { JsonLoader } from './types.ts';
import { formatException } from './error.ts';

const pool = new Pool();
const REQUEST_CACHE_DIR = resolve(import.meta.dirname, '..', '.requests');

type OutputOptions = { json?: boolean; };
type ScopeTarget = {
  federation?: string;
  kind?: string;
};
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
    columns.map((column) => String(row[column] ?? '').padEnd(widths[column])).join('  ');

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

function formattedDetail(label: string, value: unknown, full?: boolean) {
  if (value == null || value === '') return null;
  const text = typeof value === 'string' ? value : JSON.stringify(value);
  return { label, value: formatDetailText(text, full) };
}

function formatFrontierTarget(
  federation: string | null | undefined,
  kind: string | null | undefined,
  key?: string | null,
) {
  const federationText = String(federation ?? '');
  const kindText = String(kind ?? '');
  const keyText = String(key ?? '');
  return keyText
    ? `${federationText}:${kindText}:${keyText}`
    : `${federationText}:${kindText}`;
}

function scheduleRow(row: IGetCrawlerScheduleStatusResult) {
  return {
    host: row.host,
    limit:
      row.max_requests && row.per_interval
        ? `${row.max_requests}/${row.per_interval}`
        : '-',
    spacing: row.spacing ?? '-',
    queued: row.queued,
    ready: row.ready,
    delayed: row.delayed,
    locked: row.locked,
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

function parseTarget(target: string | undefined) {
  if (target == null) return undefined;

  const [federation, kind, ...keyParts] = target.split(':');
  if (!federation || kind === '')
    throw new Error('Target must be federation[:kind[:key]]');

  return {
    federation,
    kind,
    key: keyParts.length > 0 ? keyParts.join(':') : undefined,
  };
}

function parseScopeTarget(target: string | undefined): ScopeTarget | undefined {
  const parsed = parseTarget(target);
  if (!parsed) return undefined;
  if (parsed.key != null) throw new Error('Target must be federation[:kind]');
  return { federation: parsed.federation, kind: parsed.kind };
}

function parseKindTarget(target: string | undefined) {
  const parsed = parseTarget(target);
  if (!parsed?.kind) throw new Error('Target must be federation:kind[:key]');
  return {
    federation: parsed.federation,
    kind: parsed.kind,
    key: parsed.key,
  };
}

function parseExactTarget(target: string) {
  const parsed = parseKindTarget(target);
  return { federation: parsed.federation, kind: parsed.kind, key: parsed.key ?? '' };
}

function parseExplainTarget(target: string) {
  if (/^\d+$/.test(target)) {
    return { id: target, federation: null, kind: null, key: null };
  }

  const { federation, kind, key } = parseExactTarget(target);
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
            formatException(e),
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
            formatException(e),
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
  options: { verbose?: boolean },
) {
  if (federation || kind) {
    if (!federation || !kind) throw new Error('Usage: backtest <federation:kind>');
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

  let total = 0;
  let jsonLoaders = 0;
  let skippedTextLoaders = 0;
  let warnings = 0;
  const frontierKinds = new Set<string>();

  for (const row of await getBacktestFrontierKinds.run(undefined, pool)) {
    const { federation, kind } = row;
    frontierKinds.add(loaderKey(federation, kind));

    const loader = loaderFor(federation, kind);
    if (!loader) {
      warnings++;
      console.warn(
        `Warning: no loader for ${federation}:${kind} (${row.total} frontiers)`,
      );
      continue;
    }
    if (loader.mode !== 'json') {
      skippedTextLoaders++;
      console.log(`Skipping text loader ${federation}:${kind} (${row.total} frontiers)`);
      continue;
    }

    const count = await backtestJsonResponses(federation, kind, loader, {
      verbose: options.verbose,
    });
    jsonLoaders++;
    total += count;
    console.log(`Validated ${count} responses of type ${federation}:${kind}`);
  }

  for (const { federation, kind, loader } of loaderEntries()) {
    if (frontierKinds.has(loaderKey(federation, kind))) continue;

    warnings++;
    console.warn(
      `Warning: loader ${federation}:${kind} (${loader.mode}) has no frontiers`,
    );
  }

  console.log(
    `Validated ${total} responses across ${jsonLoaders} JSON loaders` +
      `; skipped ${skippedTextLoaders} text loaders; warnings ${warnings}`,
  );
}

function loaderKey(federation: string, kind: string) {
  return `${federation}:${kind}`;
}

function loaderEntries() {
  return Object.entries(LOADERS).flatMap(([federation, kinds]) =>
    Object.entries(kinds)
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([kind, loader]) => ({ federation, kind, loader })),
  );
}

type StatusOptions = {
  json?: boolean;
  frontiers?: boolean;
};

function numericCell(value: number | null | undefined) {
  return value && value > 0 ? value : '-';
}

function fetchErrorCell(row: Pick<CrawlerStatusRow, 'fetch_error' | 'fetch_transient'>) {
  const errors = row.fetch_error;
  const transient = row.fetch_transient;
  if (errors > 0 && transient > 0) return `${errors} (+${transient} transient)`;
  if (errors > 0) return errors;
  if (transient > 0) return `+${transient} transient`;
  return '-';
}

function frontierFooter(rows: CrawlerStatusRow[]) {
  const totals = rows.reduce(
    (sum, row) => ({
      kinds: sum.kinds + 1,
      total: sum.total + row.total,
      done: sum.done + row.done,
      due: sum.due + row.fetch_due,
      fetch_error: sum.fetch_error + row.fetch_error,
      fetch_transient: sum.fetch_transient + row.fetch_transient,
      process_ready: sum.process_ready + row.process_ready,
      process_error: sum.process_error + row.process_error,
      missing_loaders: sum.missing_loaders + (row.has_loader ? 0 : 1),
    }),
    {
      kinds: 0,
      total: 0,
      done: 0,
      due: 0,
      fetch_error: 0,
      fetch_transient: 0,
      process_ready: 0,
      process_error: 0,
      missing_loaders: 0,
    },
  );

  return {
    kind: `${totals.kinds} kinds`,
    loader: totals.missing_loaders > 0 ? `${totals.missing_loaders} missing` : 'ok',
    total: totals.total,
    done: totals.done,
    due: numericCell(totals.due),
    'fetch errors': fetchErrorCell(totals),
    'process ready': numericCell(totals.process_ready),
    'process errors': numericCell(totals.process_error),
    latest: '',
    keys: '',
  };
}

function frontierTableRows(rows: CrawlerStatusRow[]) {
  return rows.map((row) => ({
    kind: row.target,
    loader: row.has_loader ? 'yes' : 'no',
    total: row.total,
    done: row.done,
    due: numericCell(row.fetch_due),
    'fetch errors': fetchErrorCell(row),
    'process ready': numericCell(row.process_ready),
    'process errors': numericCell(row.process_error),
    latest: formatDate(row.latest),
    keys: row.keys,
  }));
}

function printStatusFrontiers(
  rows: CrawlerStatusRow[],
  expanded = false,
) {
  printSection('Frontiers');

  const stable = rows
    .filter((row) => row.is_stable)
    .sort((a, b) => a.target.localeCompare(b.target));
  const active = rows.filter((row) => !row.is_stable);
  const visibleRows = expanded ? rows : active;

  printTable([...frontierTableRows(visibleRows), frontierFooter(rows)]);
  if (expanded) {
    if (rows.length > visibleRows.length) {
      console.log(`Showing ${visibleRows.length} of ${rows.length} frontier kinds.`);
    }
    return;
  }

  if (active.length > visibleRows.length) {
    console.log(
      `Showing ${visibleRows.length} of ${active.length} active frontier kinds.`,
    );
  }
  if (stable.length > 0) {
    console.log(
      `done: ${stable.map((row) => `${row.target} (${row.total})`).join(', ')}`,
    );
  }
}

async function showFailures(
  target: ScopeTarget | undefined,
  options: {
    json?: boolean;
    limit?: string;
    full?: boolean;
    group?: boolean;
    excludeCode?: string | string[];
  },
) {
  const excludeHttpStatuses = parseHttpStatuses(options.excludeCode);
  const limit = parseLimit(options.limit, options.group ? 50 : 20);
  const { federation, kind } = target ?? {};

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
    {
      federation,
      kind,
      key: null,
      limit,
      excludeHttpStatuses,
    },
    pool,
  );
  const displayRows = rows.map((row) => {
    const details = detailLines(
      formattedDetail('fetch', row.response_error, options.full),
      formattedDetail('process', row.process_error, options.full),
    );

    if (details.length === 0) {
      details.push({
        label: 'error',
        value:
          row.error_text ||
          (row.process_status === 'error'
            ? 'Processing failed without a stored error'
            : '-'),
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
      id: row.id,
      kind: formatFrontierTarget(row.federation, row.kind, row.key),
      http: row.http_status ?? '-',
      errors: row.error_count,
      next: formatDate(row.next_fetch_at),
      url: short(row.url, 80) || '-',
    })),
    (index) => displayRows[index].details,
  );
}

type JobOptions = {
  json?: boolean;
  limit?: string;
  failed?: boolean;
  active?: boolean;
  full?: boolean;
};

function jobStateFromOptions(options: JobOptions): 'failed' | 'active' | null {
  const selected = [options.failed, options.active].filter(Boolean).length;
  if (selected > 1) throw new Error('Use only one of --failed or --active');
  if (options.failed) return 'failed';
  if (options.active) return 'active';
  return null;
}

async function showJobs(targetText: string | undefined, options: JobOptions) {
  const target = parseScopeTarget(targetText);
  const limit = parseLimit(options.limit, 20);
  const state = jobStateFromOptions(options);
  const rows = await getCrawlerFrontierJobs.run(
    { federation: target?.federation, kind: target?.kind, state, limit },
    pool,
  );

  if (options.json) {
    console.log(JSON.stringify(rows, null, 2));
    return;
  }

  printSection(`Frontier jobs (${state ?? 'all'})`);
  const short = (value: unknown, max: number) =>
    options.full ? String(value ?? '') : truncate(value, max);
  const displayRows = rows.map((row) => {
    const details = detailLines(
      formattedDetail('job', row.job_error, options.full),
      formattedDetail('fetch', row.response_error, options.full),
      formattedDetail('process', row.process_error, options.full),
      formattedDetail('content', row.response_content, options.full),
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

function printStatusProblems(rows: StatusProblem[]) {
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

function printStatusBacklog(rows: CrawlerStatusRow[]) {
  printSection('Backlog');
  printTable(
    rows.map((row) => ({
      kind: formatFrontierTarget(row.federation, row.kind),
      host: row.host ?? '-',
      due: row.fetch_due,
      unscheduled: row.unscheduled_fetch,
      queued: row.queued_fetch,
      locked: row.locked_fetch,
      process: row.process_ready,
      oldest: formatDate(row.oldest_due_at),
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

function statusCommands(
  filter: ScopeTarget,
  failures: IGetFrontierFailureGroupsResult[],
  problems: StatusProblem[],
) {
  const scope =
    filter.federation && filter.kind
      ? `${filter.federation}:${filter.kind}`
      : (filter.federation ?? '');
  const commands = [`pnpm crawler failures ${scope}`];

  if (
    problems.some(
      (problem) => problem.code === 'worker_failure' && problem.task === 'frontier_fetch',
    )
  ) {
    commands.push(`pnpm crawler jobs --failed ${scope}`);
  }

  const sampleProblem = problems.find((problem) => problem.sample_id);
  if (sampleProblem?.sample_id) {
    commands.push(`pnpm crawler explain ${sampleProblem.sample_id}`);
  }

  const failureSample = failures.find((failure) => failure.sample_id);
  if (failureSample?.sample_id && failureSample.sample_id !== sampleProblem?.sample_id) {
    commands.push(`pnpm crawler explain ${failureSample.sample_id}`);
  }

  const failure = failures.find((row) => row.federation && row.kind);
  if (failure) {
    const target = `${failure.federation}:${failure.kind}`;
    commands.push(`pnpm crawler refetch ${target}`);
  }

  const refetchProblem = problems.find(
    (problem) =>
      problem.kind &&
      problem.host &&
      problem.sample_key != null &&
      problem.code !== 'unknown_loader',
  );
  if (refetchProblem?.kind && refetchProblem.sample_key != null) {
    const target = refetchProblem.sample_key
      ? `${refetchProblem.kind}:${refetchProblem.sample_key}`
      : refetchProblem.kind;
    commands.push(`pnpm crawler refetch ${target}`);
  }

  return [...new Set(commands)];
}

async function showStatus(target: ScopeTarget | undefined, options: StatusOptions) {
  const allowRefetch = process.env.CRAWLER_DISABLE_REFETCH !== 'true';
  const generatedAt = new Date();
  const { federation, kind } = target ?? {};

  const [frontierRows, failureGroups, workerJobRows, scheduleRows] = await Promise.all([
    getCrawlerFrontierStatus.run({ federation, kind, allowRefetch }, pool),
    getFrontierFailureGroups.run(
      { federation, kind, excludeHttpStatuses: null },
      pool,
    ),
    getCrawlerWorkerJobStatus.run(undefined, pool),
    getCrawlerScheduleStatus.run(undefined, pool),
  ]);

  const snapshot = buildStatusSnapshot({
    generatedAt,
    filter: { federation, kind },
    frontierRows,
    failureGroups,
    workerJobs: workerJobRows,
    scheduleRows,
  });

  const visibleFrontiers = options.frontiers
    ? snapshot.frontiers
    : snapshot.frontiers.filter((row) => !row.is_stable);
  const visibleBacklog =
    [...snapshot.frontiers]
      .filter((row) => row.fetch_due > 0 || row.process_ready > 0)
      .sort(sortBacklogRows);
  const suggestedCommands = statusCommands(
    snapshot.filter,
    snapshot.failures,
    snapshot.problems,
  );

  if (options.json) {
    const output = options.frontiers
      ? {
          generated_at: snapshot.generated_at,
          filter: snapshot.filter,
          frontiers: visibleFrontiers,
        }
      : {
          ...snapshot,
          problems: snapshot.problems,
          frontiers: snapshot.failures,
          backlog: visibleBacklog,
          failures: snapshot.failures,
          schedule: snapshot.schedule,
          suggested_commands: suggestedCommands,
        };
    console.log(JSON.stringify(output, null, 2));
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

  if (options.frontiers) {
    printStatusFrontiers(snapshot.frontiers, true);
    return;
  }

  printStatusProblems(snapshot.problems);
  printStatusFrontiers(snapshot.frontiers);
  printStatusBacklog(visibleBacklog);
  printSection('Failure groups');
  printTable(snapshot.failures.map(failureGroupRow));
  printSection('Schedule');
  printTable(snapshot.schedule.map(scheduleRow));
  printCommands(suggestedCommands);
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

async function explainFrontier(target: string, options: OutputOptions) {
  const parsed = parseExplainTarget(target);
  const [row] = await getFrontierDetail.run(parsed, pool);
  if (!row) throw new Error(`Frontier ${target} not found`);

  const loader = loaderFor(row.federation, row.kind);
  const expectedUrl = loader?.buildRequest(row.key).url.toString();
  const result = {
    ...row,
    has_loader: Boolean(loader),
    expected_url: expectedUrl,
    state: explainState(row),
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
    },
  ]);

  printTable([
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
  ]);

  if (result.response_error) console.log(`response error: ${result.response_error}`);
  if (result.last_process_error)
    console.log(`process error: ${result.last_process_error}`);
  if (result.job_last_error) console.log(`job error: ${result.job_last_error}`);
  if (result.expected_url) console.log(`request: ${result.expected_url}`);
}

async function showLatestResponse(target: string) {
  const parsed = parseExplainTarget(target);
  const [row] = await getLatestFrontierResponse.run(parsed, pool);
  if (!row) throw new Error(`Response for ${target} not found`);

  console.log(
    row.json_content == null
      ? (row.text_content ?? '')
      : JSON.stringify(row.json_content, null, 2),
  );
}

type RefetchOptions = {
  json?: boolean;
  all?: boolean;
  limit?: string;
};

async function queueRefetch(target: string, options: RefetchOptions) {
  const parsed = parseTarget(target);
  if (!parsed) throw new Error('Target must be federation[:kind[:key]]');
  const isExact = parsed.key != null;
  if (isExact && (!parsed.kind || !loaderFor(parsed.federation, parsed.kind))) {
    throw new Error(`Unknown loader ${parsed.federation}:${parsed.kind}`);
  }

  const limit = isExact ? 1 : parseOptionalLimit(options.limit);
  const rows = await queueFrontierRefetch.run(
    {
      federation: parsed.federation,
      kind: parsed.kind ?? null,
      key: parsed.key ?? null,
      includeOk: options.all ?? false,
      limit,
    },
    pool,
  );
  const scheduled = rows.length > 0;
  if (scheduled) await queueCrawlerSchedule.run(undefined, pool);
  if (isExact && rows.length === 0) throw new Error(`Frontier ${target} not found`);

  if (options.json) {
    console.log(JSON.stringify({ scheduled, rows }, null, 2));
    return;
  }

  const label = isExact ? 'frontier row' : options.all ? 'frontier rows' : 'failed frontier rows';
  console.log(`Queued ${rows.length} ${label}` + (limit == null ? '' : ` (limit ${limit})`));
  if (scheduled) console.log(`Queued frontier_schedule job`);
  printTable(
    rows.slice(0, 50).map((row) => ({
      action: 'refetch queued',
      id: row.id,
      target: formatFrontierTarget(row.federation, row.kind, row.key),
    })),
  );
  if (rows.length > 50) console.log(`Showing 50 of ${rows.length}; use --json for all rows.`);
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
          const message = formatException(e);
          console.error(`ERR ${ms}ms ${text} ${message}`);
          throw e;
        }
      };
    },
  });
}

async function loadCached(federation: string, kind: string, key: string, shouldCommit: boolean) {
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
  .command('', 'Show crawler health status')
  .option('--json', 'Print JSON')
  .action((options: StatusOptions) => {
    return showStatus(undefined, options);
  });

cli
  .command('status [target]', 'Show crawler health status')
  .option('--frontiers', 'Show the expanded frontier kind table')
  .option('-n, --limit <limit>', 'Maximum rows to show per list section')
  .option('--json', 'Print JSON')
  .action((target: string | undefined, options: StatusOptions) => {
    return showStatus(parseScopeTarget(target), options);
  });

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
      options: {
        json?: boolean;
        limit?: string;
        full?: boolean;
        group?: boolean;
        excludeCode?: string | string[];
      },
    ) => {
      return showFailures(parseScopeTarget(target), options);
    },
  );

cli
  .command('jobs [target]', 'Show crawler worker job health')
  .option('--failed', 'Show exhausted frontier_fetch jobs')
  .option('--active', 'Show ready, delayed, or locked frontier_fetch jobs')
  .option('-n, --limit <limit>', 'Maximum frontier jobs to show', { default: '20' })
  .option('--full', 'Do not truncate long targets or errors')
  .option('--json', 'Print JSON')
  .action((target: string | undefined, options: JobOptions) => showJobs(target, options));

cli
  .command('explain <target>', 'Explain one frontier row by id or federation:kind[:key]')
  .option('--json', 'Print JSON')
  .action((target: string, options: OutputOptions) => explainFrontier(target, options));

cli
  .command('response <target>', 'Print the latest stored response for a frontier')
  .action((target: string) => showLatestResponse(target));

cli.command('fetch <target>', 'Fetch and cache a response').action((target: string) => {
  const { federation, kind, key } = parseExactTarget(target);
  return ensureCached(federation, kind, key, true);
});

cli
  .command('refetch <target>', 'Queue federation:kind[:key] for immediate re-fetch')
  .option('--all', 'Include OK rows when refetching a scoped federation[:kind] target')
  .option('-n, --limit <limit>', 'Maximum scoped rows to include')
  .option('--json', 'Print JSON for scoped-row selections')
  .action((target: string, options: RefetchOptions) => queueRefetch(target, options));

cli
  .command('backtest [target]', 'Strict-validate cached JSON responses for a loader')
  .option('-v, --verbose', 'Include full input in schema failure logs')
  .action((target: string | undefined, options: { verbose?: boolean }) => {
    const parsed = parseScopeTarget(target);
    return backtestSchemas(parsed?.federation, parsed?.kind, options);
  });

cli
  .command('load <target>', 'Load a cached response')
  .option('--commit', 'Commit instead of rolling back')
  .action((target: string, options: { commit?: boolean }) => {
    const { federation, kind, key } = parseExactTarget(target);
    return loadCached(federation, kind, key, Boolean(options.commit));
  });

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
