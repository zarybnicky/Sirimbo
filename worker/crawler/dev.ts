import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import process from 'node:process';
import { deepStrictEqual } from 'node:assert/strict';
import { cac } from 'cac';
import { Pool, type PoolClient } from 'pg';
import Cursor from 'pg-cursor';
import { zx } from '@traversable/zod';
import { getFrontierKindStatus, getLatestFrontierResponses } from './crawler.queries.ts';
import { fetchResponse } from './fetch.ts';
import { loaderFor, LOADERS } from './handlers.ts';
import type { JsonLoader } from './types.ts';

const pool = new Pool();
const REQUEST_CACHE_DIR = resolve(import.meta.dirname, '..', '.requests');

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
    const cursor = getLatestFrontierResponses.stream(
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
      throw new Error('Usage: backtest <federation> <kind> or backtest --all');
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
    throw new Error('Use either `backtest --all` or `backtest <federation> <kind>`');
  }

  let total = 0;
  let jsonLoaders = 0;
  for (const row of await getFrontierKindStatus.run({}, pool)) {
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

async function showStatus(
  federation: string | undefined,
  kind: string | undefined,
  options: { unknown?: boolean },
) {
  if (kind && !federation) {
    throw new Error('Usage: status [federation] [kind]');
  }

  const rows = (await getFrontierKindStatus.run({ federation, kind }, pool)).filter(
    (row) => !options.unknown || loaderFor(row.federation, row.kind),
  );

  printTable(
    rows.map((row) => ({
      kind: `${row.federation}:${row.kind}`,
      total: row.total ?? 0,
      responses: row.responses ?? 0,
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
    })),
  );
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
  .command('status [federation] [kind]', 'Show crawler frontier status')
  .option('--unknown', 'Only show frontier kinds without a loader')
  .action(
    (
      federation: string | undefined,
      kind: string | undefined,
      options: { unknown?: boolean },
    ) => showStatus(federation, kind, options),
  );

cli
  .command('request <federation> <kind> [key]', 'Print the loader request')
  .action((federation: string, kind: string, key = '') => {
    const loader = loaderFor(federation, kind);
    if (!loader) throw new Error(`Unknown loader ${federation}:${kind}`);
    const { url, init = {} } = loader.buildRequest(key);
    console.log(JSON.stringify({ url: url.toString(), init }, null, 2));
  });

cli
  .command('fetch <federation> <kind> [key]', 'Fetch and cache a response')
  .action((federation: string, kind: string, key = '') =>
    ensureCached(federation, kind, key, true),
  );

cli
  .command(
    'backtest [federation] [kind]',
    'Strict-validate cached JSON responses for a loader',
  )
  .option('--all', 'Strict-validate cached JSON responses for all JSON loaders')
  .option('-v, --verbose', 'Include full input in schema failure logs')
  .action(
    (
      federation: string | undefined,
      kind: string | undefined,
      options: { all?: boolean; verbose?: boolean },
    ) => backtestSchemas(federation, kind, options),
  );

cli
  .command('load <federation> <kind> [key]', 'Load a cached response')
  .option('--commit', 'Commit instead of rolling back')
  .action(
    (
      federation: string,
      kind: string,
      key: string | undefined,
      options: { commit?: boolean },
    ) => loadCached(federation, kind, key ?? '', Boolean(options.commit)),
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
