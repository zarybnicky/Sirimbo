import process from 'node:process';
import { deepStrictEqual } from 'node:assert/strict';
import { cac } from 'cac';
import type { PoolClient } from 'pg';
import Cursor from 'pg-cursor';
import { zx } from '@traversable/zod';
import type { IGetFrontierResponsesResult } from './crawler.queries.ts';
import {
  getFrontierResponses,
  markFrontiersProcessSuccess,
} from './crawler.queries.ts';
import { LOADERS, loadersFor } from './handlers.ts';
import type { JsonLoader } from './types.ts';
import { formatException } from './error.ts';
import {
  flushLoaderEffects,
  type LoaderEffect,
  type LoaderResult,
} from './effects.ts';
import { pool } from '../pool.ts';
import { loadFrontier } from './process.ts';

function printTable(rows: Record<string, unknown>[]) {
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

  rows.forEach((row) => console.log(rowText(row)));
}

function parseLimit(value: unknown) {
  if (!value) return null;
  const limit = Number(value);
  if (!Number.isInteger(limit) || limit < 1) {
    throw new Error('Limit must be a positive integer');
  }
  return limit;
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
          throw new Error(message, { cause: e });
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
          throw new Error(message, { cause: e });
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
): Promise<{ ok: true; effects: LoaderResult } | { ok: false }> {
  await client.query('SAVEPOINT bulk_process');

  try {
    const effects = await loadFrontier(client, row, 'strict');
    await client.query('RELEASE SAVEPOINT bulk_process');
    return { ok: true, effects };
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
    return { ok: false };
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
    const effects: LoaderEffect[] = [];
    const successfulIds: string[] = [];

    let shouldStop = false;
    while (!shouldStop) {
      const rows = await cursor.read(25);
      if (rows.length === 0) break;

      for (const row of rows) {
        const target = [row.federation, row.kind].join(':');
        const rowStarted = performance.now();
        const result = await processFrontier(processClient, row, options);
        const kindStats = stats.get(target) ?? {
          count: 0,
          errors: 0,
          ms: 0,
        };
        kindStats.count += 1;
        kindStats.ms += performance.now() - rowStarted;
        if (!result.ok) kindStats.errors += 1;
        stats.set(target, kindStats);

        if (result.ok) {
          counts.processed += 1;
          successfulIds.push(row.id);
          if (result.effects) effects.push(result.effects);
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

    await flushLoaderEffects(processClient, effects);
    if (successfulIds.length) {
      await markFrontiersProcessSuccess.run({ ids: successfulIds }, processClient);
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
      `${counts.failures} failures; ${options.commit ? 'committed' : 'rolled back'}`,
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

cli.command('', 'Show loader development commands').action(() => cli.outputHelp());

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
