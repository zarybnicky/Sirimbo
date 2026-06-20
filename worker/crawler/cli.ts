import process from 'node:process';
import { deepStrictEqual } from 'node:assert/strict';
import { cac } from 'cac';
import { Pool, type PoolClient } from 'pg';
import Cursor from 'pg-cursor';
import { zx } from '@traversable/zod';
import type {
  IGetCrawlerStatusResult,
  IGetFrontierDetailResult,
  IGetFrontierResponsesResult,
} from './crawler.queries.ts';
import {
  getCrawlerJobs,
  getCrawlerStatus,
  getFrontierDetail,
  getFrontierFailureGroups,
  getFrontierResponses,
  getLatestFrontierFailures,
  getLatestFrontierResponse,
  queueCrawlerSchedule,
  queueRefetch,
} from './crawler.queries.ts';
import { ALL_LOADERS, loaderFor, LOADERS, loadersFor } from './handlers.ts';
import type { JsonLoader } from './types.ts';
import { formatException } from './error.ts';

const pool = new Pool();

type CrawlerStatusRow = IGetCrawlerStatusResult & {
  target: string;
  isStable: boolean;
};

function frontierPriority(row: CrawlerStatusRow) {
  return (
    row.process_error * 100_000 +
    row.fetch_error * 10_000 +
    row.fetch_transient * 1_000 +
    row.process_ready * 100 +
    row.fetch_due * 10 +
    row.fetch_pending
  );
}

function formatDate(date: Date | null | undefined) {
  return date ? date.toISOString().replace(/\.\d{3}Z$/, 'Z') : '-';
}

function printTable(
  rows: Array<Record<string, unknown> & { details?: Record<string, string | null> }>,
) {
  if (rows.length === 0) {
    console.log('(none)');
    return;
  }

  const columns = Object.keys(rows[0]).filter((x) => x !== 'details');
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

  rows.forEach(({ details, ...row }) => {
    console.log(rowText(row));
    if (!details) return;
    for (const [label, value] of Object.entries(details)) {
      if (!value) continue;
      const realLabel = label ? `${label}: ` : '  ';
      const lines = value.split('\n');
      console.log(`${realLabel}${lines[0] ?? ''}`);
      const indent = ' '.repeat(realLabel.length);
      for (const line of lines.slice(1)) console.log(`  ${indent}${line}`);
    }
  });
}

function truncate(text: string | null, max: number) {
  if (!text) return null;
  text = text.replace(/\s+/g, ' ');
  return text.length <= max ? text : `${text.slice(0, Math.max(0, max - 3))}...`;
}

function parseLimit(value: unknown) {
  if (!value) return null;
  const limit = Number(value);
  if (!Number.isInteger(limit) || limit < 1) {
    throw new Error('Limit must be a positive integer');
  }
  return limit;
}

function parseHttpStatuses(value: unknown) {
  const codes = (Array.isArray(value) ? value : [value])
    .filter(Boolean)
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

type Target =
  | { scope: 'frontier'; id: null; federation: string; kind: string; key: string }
  | { scope: 'frontier'; id: string; federation: null; kind: null; key: null }
  | { scope: 'kind'; federation: string; kind: string; key: null }
  | { scope: 'federation'; federation: string; kind: null; key: null }
  | { scope: 'none'; federation: null; kind: null; key: null };

function parseTarget(target: string | undefined): Target {
  if (!target) return { scope: 'none', federation: null, kind: null, key: null };
  if (/^\d+$/.test(target)) {
    return { scope: 'frontier', id: target, federation: null, kind: null, key: null };
  }

  const [federation, kind, ...keyParts] = target.split(':');
  const key = keyParts.length > 0 ? keyParts.join(':') : undefined;
  if (key) return { scope: 'frontier', id: null, federation, kind, key };
  if (kind) return { scope: 'kind', federation, kind, key: null };
  return { scope: 'federation', federation, kind: null, key: null };
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
    const cursor = getFrontierResponses.stream(
      { federation, kind, allowErrors: false },
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
  target: string | undefined,
  options: { verbose?: boolean },
) {
  const { scope, federation, kind } = parseTarget(target);
  if (scope === 'frontier') throw new Error('Target must be federation[:kind]');
  const loaders = loadersFor(federation, kind);

  const frontierKinds = new Set<string>();

  for (const { federation, kind, loader } of loaders) {
    const target = [federation, kind].join(':');
    frontierKinds.add(target);

    if (loader.mode !== 'json') {
      console.log(`Skipping text loader ${target}`);
      continue;
    }

    const count = await backtestJsonResponses(federation, kind, loader, options);
    console.log(`Validated ${count} responses of type ${target}`);
  }
}

type StatusOptions = {
  json?: boolean;
  all?: boolean;
};

function fetchErrorCell(row: Pick<CrawlerStatusRow, 'fetch_error' | 'fetch_transient'>) {
  const errors = row.fetch_error;
  const transient = row.fetch_transient;
  if (errors > 0 && transient > 0) return `${errors} (+${transient} temp)`;
  if (errors > 0) return errors;
  if (transient > 0) return `${transient} temp`;
  return '';
}

async function showFailures(
  target: string | undefined,
  options: {
    json?: boolean;
    limit?: string;
    full?: boolean;
    excludeCode?: string | string[];
  },
) {
  const excludeHttpStatuses = parseHttpStatuses(options.excludeCode);
  const limit = parseLimit(options.limit) ?? 20;
  const parsed = parseTarget(target);

  const rows = await getLatestFrontierFailures.run(
    { ...parsed, limit, excludeHttpStatuses },
    pool,
  );
  if (options.json) {
    console.log(JSON.stringify(rows, null, 2));
    return;
  }
  printTable(
    rows.map((row) => ({
      failed: formatDate(row.failed_at),
      failure: row.failure,
      id: row.id,
      kind: [row.federation, row.kind, row.key].join(':'),
      http: row.http_status ?? '-',
      errors: row.error_count,
      next: formatDate(row.next_fetch_at),
      url: row.url || '-',
      details: { '': row.error_text },
    })),
  );
}

type JobOptions = {
  json?: boolean;
  limit?: string;
  failed?: boolean;
  active?: boolean;
  full?: boolean;
};

async function showJobs(targetText: string | undefined, options: JobOptions) {
  const target = targetText ? parseTarget(targetText) : null;
  const limit = parseLimit(options.limit) ?? 20;

  if (options.failed && options.active)
    throw new Error('Use only one of --failed or --active');
  const state = options.failed ? 'failed' : options.active ? 'active' : null;
  const rows = await getCrawlerJobs.run(
    { federation: target?.federation, kind: target?.kind, state, limit },
    pool,
  );

  if (options.json) {
    console.log(JSON.stringify(rows, null, 2));
    return;
  }

  printTable(
    rows.map((row) => ({
      state: row.state,
      job: row.job_id,
      target: [row.federation, row.kind, row.frontier_key].join(':'),
      frontier: row.frontier_id,
      attempts: `${row.attempts}/${row.max_attempts}`,
      run: formatDate(row.run_at),
      locked: formatDate(row.locked_at),
      fetch: row.fetch_status,
      process: row.process_status,
      http: row.response_http_status ?? '-',
      updated: formatDate(row.job_updated_at),
      details: {
        job: options.full ? row.job_error : truncate(row.job_error, 500),
        fetch: options.full ? row.response_error : truncate(row.response_error, 500),
        process: options.full ? row.process_error : truncate(row.process_error, 500),
        content: options.full ? JSON.stringify(row.response_content) : null,
      },
    })),
  );
}

async function showStatus(target: string | undefined, options: StatusOptions) {
  const allowRefetch = process.env.CRAWLER_DISABLE_REFETCH !== 'true';
  const parsed = parseTarget(target);
  if (parsed.scope !== 'kind' && parsed.scope !== 'federation' && parsed.scope !== 'none')
    throw new Error('Must target more than a single frontier');
  const { federation, kind } = parsed;

  const [frontierRows, failures] = await Promise.all([
    getCrawlerStatus.run({ federation, kind, allowRefetch }, pool),
    getFrontierFailureGroups.run({ federation, kind, excludeHttpStatuses: null }, pool),
  ]);

  const frontiers = frontierRows
    .map((row) => ({
      ...row,
      target: [row.federation, row.kind].join(':'),
      isStable:
        row.total === row.done &&
        row.fetch_due === 0 &&
        row.fetch_error === 0 &&
        row.fetch_transient === 0 &&
        row.process_error === 0,
    }))
    .toSorted(
      (a, b) =>
        frontierPriority(b) - frontierPriority(a) || a.target.localeCompare(b.target),
    );

  if (options.json) {
    console.log(JSON.stringify({ frontiers, failures }, null, 2));
    return;
  }

  const totals = frontiers.reduce(
    (sum, row) => ({
      targets: sum.targets + 1,
      done: sum.done + row.done,
      due: sum.due + row.fetch_due,
    }),
    { targets: 0, done: 0, due: 0 },
  );

  printTable([
    ...frontiers
      .filter((row) => options.all || !row.isStable)
      .map((row) => ({
        target: row.target,
        done: row.done > 0 ? row.done : '',
        due: row.fetch_due > 0 ? row.fetch_due : '',
        'fetch errors': fetchErrorCell(row),
        'process errors': row.process_error > 0 ? row.process_error : '',
        process: row.process_ready > 0 ? row.process_ready : '',
        queued: row.queued_fetch > 0 ? row.queued_fetch : '',
        locked: row.locked_fetch > 0 ? row.locked_fetch : '',
        keys: row.keys,
      })),
    {
      target: `${totals.targets} total`,
      done: totals.done > 0 ? totals.done : '',
      due: totals.due > 0 ? totals.due : '',
    },
  ]);

  const knownLoaders = new Set(ALL_LOADERS.map((x) => x.federation + ':' + x.kind));
  const knownTargets = new Set(frontiers.map((x) => x.target));
  const problems = [
    ...knownTargets
      .difference(knownLoaders)
      .entries()
      .map(([row]) => `${row} has no loader`),
    ...knownLoaders
      .difference(knownTargets)
      .entries()
      .map(([row]) => `${row} has no frontiers`),
  ];
  if (problems.length > 0) {
    console.log();
    for (const problem of problems) console.warn(`Warning: ${problem}`);
  }

  if (!options.all) {
    console.log();
    console.log('Done:');
    const byFederation = new Map<string, { kind: string; total: number }[]>();
    for (const row of frontiers.filter((row) => row.isStable)) {
      const { federation, kind, total } = row;
      const slot =
        byFederation.get(federation) ?? byFederation.set(federation, []).get(federation)!;
      slot.push({ kind, total });
    }
    for (const [fed, kinds] of [...byFederation.entries()].toSorted((a, b) =>
      a[0].localeCompare(b[0]),
    )) {
      const kindsReport = kinds
        .toSorted((a, b) => b.total - a.total || a.kind.localeCompare(b.kind))
        .map((x) => (x.total === 1 ? x.kind : `${x.kind} (${x.total})`))
        .join(', ');
      console.log(`${fed}: ${kindsReport}`);
    }
  }
  console.log();
  console.log('Failures');
  printTable(
    failures.map((row) => ({
      count: row.count,
      kind: [row.federation, row.kind].join(':'),
      failure: row.failure,
      http: row.http_status ?? '',
      samples: truncate(row.samples, 80),
      details: { '': row.error_fingerprint },
    })),
  );
}

function explainState(row: IGetFrontierDetailResult) {
  if (!loaderFor(row.federation, row.kind)) return 'unknown loader';
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

async function explainFrontier(target: string, options: { json?: boolean }) {
  const parsed = parseTarget(target);
  if (parsed.scope !== 'frontier')
    throw new Error('Must target a single frontier by ID or path');
  const [row] = await getFrontierDetail.run(parsed, pool);
  if (!row) throw new Error(`Frontier ${target} not found`);

  const loader = loaderFor(row.federation, row.kind);
  const expectedUrl = loader?.buildRequest(row.key).url.toString();
  const result = {
    ...row,
    has_loader: !!loader,
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
  const parsed = parseTarget(target);
  if (parsed.scope !== 'frontier')
    throw new Error('Must target a single frontier by ID or path');
  const [row] = await getLatestFrontierResponse.run(parsed, pool);
  if (!row) throw new Error(`Response for ${target} not found`);

  if (typeof row.content === 'string') console.log(row.content);
  else console.log(JSON.stringify(row.content, null, 2));
}

type RefetchOptions = {
  json?: boolean;
  all?: boolean;
  limit?: string;
};

async function refetch(target: string, options: RefetchOptions) {
  const parsed = parseTarget(target);
  const rows = await queueRefetch.run(
    { ...parsed, includeOk: options.all ?? false },
    pool,
  );
  if (rows.length > 0) await queueCrawlerSchedule.run(undefined, pool);
  if (parsed.key && rows.length === 0) throw new Error(`Frontier ${target} not found`);

  if (options.json) {
    console.log(JSON.stringify(rows, null, 2));
  } else {
    console.log(`Queued ${rows.length} rows`);
  }
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
        const query = args[0];
        const queryText =
          typeof query === 'string'
            ? query
            : query && typeof query === 'object' && 'text' in query
              ? query.text
              : '<unknown query>';
        const text = String(queryText).replace(/\s+/g, ' ').trim();
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

async function processFrontier(
  client: PoolClient,
  row: IGetFrontierResponsesResult,
  options: { keepGoing?: boolean },
) {
  await client.query('SAVEPOINT bulk_process');

  try {
    const loader = loaderFor(row.federation, row.kind);
    if (!loader) throw new Error(`Unknown loader ${row.federation}:${row.kind}`);
    const content =
      loader.mode === 'json'
        ? zx.deepStrict(loader.schema).parse(row.content, { reportInput: true })
        : row.content;
    await loader.load(client, content);
    await client.query('RELEASE SAVEPOINT bulk_process');
    return true;
  } catch (e) {
    await client.query('ROLLBACK TO SAVEPOINT bulk_process');
    await client.query('RELEASE SAVEPOINT bulk_process');

    console.error(
      `Processing failed for frontier ${row.id} (${[row.federation, row.kind, row.key].join(':')})`,
    );
    console.error(`response: ${row.url}`);
    console.error(formatException(e));
    if (!options.keepGoing) {
      console.error('Stopping after first failure; use --keep-going to collect more.');
    }
    return false;
  }
}

async function processLatest(
  target: string,
  options: {
    keepGoing?: boolean;
    limit?: string;
    verbose?: boolean;
    commit?: boolean;
  },
) {
  const parsed = parseTarget(target);
  const limit = parseLimit(options.limit);
  const selectClient = await pool.connect();
  const processClient = options.verbose
    ? logClientQueries(await pool.connect())
    : await pool.connect();
  const stats = new Map<string, { count: number; errors: number; ms: number }>();
  const counts = { processed: 0, failures: 0 };
  const started = performance.now();
  let inTransaction = false;

  try {
    const cursor = getFrontierResponses.stream(
      { ...parsed, limit, allowErrors: true },
      {
        query: selectClient.query,
        stream: (query, bindings) => selectClient.query(new Cursor(query, bindings)),
      },
    );

    await processClient.query('BEGIN');
    inTransaction = true;

    let shouldStop = false;
    while (!shouldStop) {
      const rows = await cursor.read(25);
      if (rows.length === 0) break;

      for (const row of rows) {
        const target = [row.federation, row.kind].join(':');
        const rowStarted = performance.now();
        const ok = await processFrontier(processClient, row, options);
        const kindStats = stats.get(target) ?? {
          count: 0,
          errors: 0,
          ms: 0,
        };
        kindStats.count += 1;
        kindStats.ms += performance.now() - rowStarted;
        if (!ok) kindStats.errors += 1;
        stats.set(target, kindStats);

        if (ok) {
          counts.processed += 1;
        } else {
          counts.failures += 1;
          shouldStop = !options.keepGoing;
          if (shouldStop) break;
        }

        const total = counts.processed + counts.failures;
        if (total % 100 === 0) {
          process.stdout.write(
            `\rValidated ${counts.processed}/${total} responses so far; ` +
              `${counts.failures} failures; ` +
              `${Math.round(performance.now() - started)}ms\x1b[K`,
          );
        }
      }
    }

    await processClient.query(options.commit ? 'COMMIT' : 'ROLLBACK');
    inTransaction = false;
  } catch (e) {
    if (inTransaction) await processClient.query('ROLLBACK');
    throw e;
  } finally {
    processClient.release();
    selectClient.release();
  }

  process.stdout.write('\n');
  if (stats.size > 0) {
    printTable(
      [...stats.entries()]
        .sort((a, b) => b[1].ms - a[1].ms)
        .map(([kind, row]) => ({
          kind,
          ok: row.count - row.errors,
          errors: row.errors,
          total: row.count,
          ms: Math.round(row.ms),
        })),
    );
  }

  const total = counts.processed + counts.failures;
  console.log(
    `Validated ${counts.processed}/${total} responses in ` +
      `${Math.round(performance.now() - started)}ms; ` +
      `${counts.failures} failures; rolled back`,
  );
  if (counts.failures > 0) process.exitCode = 1;
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
  .option('--all', 'Do not abbreviate done frontiers')
  .action((options: StatusOptions) => showStatus(undefined, options));

cli
  .command('status [target]', 'Show crawler health status')
  .option('--json', 'Print JSON')
  .option('--all', 'Do not abbreviate done frontiers')
  .action((target: string | undefined, options: StatusOptions) =>
    showStatus(target, options),
  );

cli
  .command('failures [target]', 'Show latest failed frontier rows')
  .option('-n, --limit <limit>', 'Maximum rows to show', { default: '20' })
  .option('--exclude-code <codes>', 'Hide HTTP status codes, comma-separated')
  .option('--full', 'Do not truncate long keys or URLs')
  .option('--json', 'Print JSON')
  .action(
    (
      target: string | undefined,
      options: {
        json?: boolean;
        limit?: string;
        full?: boolean;
        excludeCode?: string | string[];
      },
    ) => {
      return showFailures(target, options);
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
  .action((target: string, options: { json?: boolean }) =>
    explainFrontier(target, options),
  );

cli
  .command('response <target>', 'Print the latest stored response for a frontier')
  .action((target: string) => showLatestResponse(target));

cli
  .command('refetch <target>', 'Queue federation[:kind[:key]] for immediate re-fetch')
  .option('--all', 'Include OK rows when refetching a scoped federation[:kind] target')
  .option('--json', 'Print JSON for scoped-row selections')
  .action((target: string, options: RefetchOptions) => refetch(target, options));

cli
  .command('backtest [target]', 'Validate latest JSON responses for a loader')
  .option('-v, --verbose', 'Include full input in schema failure logs')
  .action((target: string | undefined, options: { verbose?: boolean }) => {
    return backtestSchemas(target, options);
  });
cli
  .command('process <target>', 'Re-run loaders for stored OK responses')
  .option('--keep-going', 'Continue after failures')
  .option('--commit', 'Don\'t rollback everything in the end')
  .option('-v, --verbose', 'Log loader queries')
  .option('-n, --limit <limit>', 'Maximum responses to validate')
  .action(
    (
      target: string,
      options: {
        keepGoing?: boolean;
        limit?: string;
        verbose?: boolean;
        commit?: boolean;
      },
    ) => processLatest(target, options),
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
