import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import process from 'node:process';
import { cac } from 'cac';
import { Pool, type PoolClient } from 'pg';
import Cursor from 'pg-cursor';
import { zx } from '@traversable/zod';
import { getLatestFrontierJsonResponses } from './crawler.queries.ts';
import { fetchJsonResponse, fetchTextResponse } from './fetch.ts';
import { LOADER_MAP, LOADERS } from './handlers.ts';
import type { FrontierRow, HtmlLoader, JsonLoader } from './types.ts';

type Loader = JsonLoader | HtmlLoader;

function loaderFor(federation: string, kind: string): Loader {
  const loader = LOADER_MAP[federation]?.[kind];
  if (!loader) throw new Error(`Unknown crawler loader ${federation}:${kind}`);
  return loader;
}

async function ensureCached(
  federation: string,
  kind: string,
  key: string,
  overwrite?: boolean,
) {
  const loader = loaderFor(federation, kind);
  const path = resolve('.requests', `${federation}:${kind}:${key}`);
  if (existsSync(path) && !overwrite) {
    console.log(`Using ${path}`);
    return { loader, path };
  }

  const { url, init = {} } = loader.buildRequest(key);
  const result =
    loader.mode === 'json'
      ? await fetchJsonResponse(loader, url, init, { mode: 'strict' })
      : await fetchTextResponse(loader, url, init);

  const body =
    loader.mode === 'json'
      ? `${JSON.stringify(result.content, null, 2)}\n`
      : String(result.content ?? '');
  await mkdir(dirname(path), { recursive: true });
  await writeFile(path, body, 'utf8');
  console.log(`Fetched ${url} (${result.httpStatus}, ${result.fetchStatus})`);

  if (result.error) throw new Error(result.error);
  return { loader, path };
}

async function backtestSchemas(federation: string, kind: string) {
  const loader = loaderFor(federation, kind);
  if (loader.mode !== 'json') {
    throw new Error(`Backtest only supports JSON loaders (${federation}:${kind})`);
  }

  const strictSchema = zx.deepStrict(loader.schema);
  const pool = new Pool();
  const client = await pool.connect();

  try {
    const cursor = getLatestFrontierJsonResponses.stream(
      { federation, kind },
      {
        query: client.query,
        stream: (query, bindings) => client.query(new Cursor(query, bindings)),
      },
    );

    let count = 0;
    try {
      while (true) {
        const rows = await cursor.read(10);
        if (rows.length === 0) break;

        for (const row of rows) {
          try {
            strictSchema.parse(row.content, { reportInput: true });
            count++;
          } catch (e) {
            const details = e instanceof Error ? e.stack || e.message : String(e);
            throw new Error(
              `Strict schema failed for ${federation}:${kind} frontier ${row.id} (${row.url})\n${details}\n${JSON.stringify(row, null, 2)}`,
            );
          }
        }
      }
    } finally {
      await cursor.close();
    }

    console.log(`Validated ${count} responses of type ${federation}:${kind}`);
  } finally {
    client.release();
    await pool.end();
  }
}

function frontier(federation: string, kind: string, key: string): FrontierRow {
  return {
    id: '0',
    federation,
    kind,
    key,
    fetch_status: 'ok',
    error_count: 0,
    meta: {},
  };
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
  const cached = loader.mode === 'json' ? JSON.parse(body) : body;

  const pool = new Pool();
  const client = await pool.connect();
  const loggedClient = logClientQueries(client);
  try {
    await loggedClient.query('BEGIN');
    if (loader.mode === 'json') {
      const content = loader.schema.parse(cached, { reportInput: true });
      await loader.load(loggedClient, frontier(federation, kind, key), content);
    } else if (typeof cached === 'string') {
      await loader.load(loggedClient, frontier(federation, kind, key), cached);
    } else {
      throw new Error(`Expected text response content, got ${typeof cached}`);
    }
    await loggedClient.query(shouldCommit ? 'COMMIT' : 'ROLLBACK');
    console.log(`${shouldCommit ? 'Committed' : 'Rolled back'} ${path}`);
  } catch (e) {
    await loggedClient.query('ROLLBACK');
    throw e;
  } finally {
    client.release();
    await pool.end();
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
  .command('request <federation> <kind> [key]', 'Print the loader request')
  .action((federation: string, kind: string, key = '') => {
    const { url, init = {} } = loaderFor(federation, kind).buildRequest(key);
    console.log(JSON.stringify({ url: url.toString(), init }, null, 2));
  });

cli
  .command('fetch <federation> <kind> [key]', 'Fetch and cache a response')
  .action((federation: string, kind: string, key = '') =>
    ensureCached(federation, kind, key, true),
  );

cli
  .command(
    'backtest <federation> <kind>',
    'Strict-validate cached JSON responses for a loader',
  )
  .action((federation: string, kind: string) => backtestSchemas(federation, kind));

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
}
