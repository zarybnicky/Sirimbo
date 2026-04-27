import type { JobHelpers, Task } from 'graphile-worker';
import { upsertFrontierKeys } from '../crawler/crawler.queries.ts';
import { cstsEventIndex } from '../crawler/cstsEventIndex.ts';
import { cstsResultIndex } from '../crawler/cstsResultIndex.ts';

async function checkCstsEvents(month: Date): Promise<string | null> {
  const key = month.toISOString().slice(0, 7);
  const { url, init } = cstsEventIndex.buildRequest(key);
  try {
    const response = await fetch(url, init);
    if (!response.ok) return null;

    const raw = await response.json();
    const parsed = cstsEventIndex.schema.safeParse(raw, { reportInput: true });
    if (!parsed.success || parsed.data.collection.length === 0) return null;
  } catch (e) {
    console.error(e);
  }

  return key;
}

async function checkCstsResults(month: Date): Promise<string | null> {
  const key = month.toISOString().slice(0, 7);
  const { url, init } = cstsResultIndex.buildRequest(key);
  const response = await fetch(url, init);
  if (!response.ok) return null;

  const raw = await response.json();
  const parsed = cstsResultIndex.schema.safeParse(raw, { reportInput: true });
  if (!parsed.success || parsed.data.collection.length === 0) return null;

  return key;
}

export const discover_events: Task<'discover_events'> = async (
  _payload,
  helpers,
) => {
  await discoverMonths('csts', 'eventIndex', checkCstsEvents, helpers);
  await discoverMonths('csts', 'resultIndex', checkCstsResults, helpers);
};

async function discoverMonths(
  federation: string,
  kind: string,
  probeMonth: (month: Date) => Promise<string | null>,
  helpers: JobHelpers,
  opts = {
    maxBackfillMonths: 36,
    maxLookaheadMonths: 12,
    emptyStopPast: 4,
    emptyStopFuture: 3,
  },
): Promise<number> {
  const { rows } = await helpers.query<{ key: string }>(
    `select key from crawler.frontier where federation = $1 and kind = $2 order by key`,
    [federation, kind],
  );

  const keysToAdd: string[] = [];
  const knownMonths = rows.map((row) => new Date(`${row.key}-01T00:00:00Z`));
  if (knownMonths.length === 0) {
    const now = startOfMonth(new Date());
    knownMonths.push(now);
    keysToAdd.push(now.toISOString().slice(0, 7));
  }

  const knownList = knownMonths.map(toMonthId);
  const known = new Set<number>(knownList);

  // 1) Probe all internal gaps
  for (let i = 0; i < knownList.length - 1; i++) {
    const left = knownList[i];
    const right = knownList[i + 1];

    for (let id = left + 1; id < right; id++) {
      if (known.has(id)) continue;

      const key = await probeMonth(fromMonthId(id));
      if (key) {
        known.add(id);
        keysToAdd.push(key);
      }
    }
  }

  // 2) Scan outward to the left
  {
    let empties = 0;
    let depth = 1;
    let id = knownList[0] - 1;

    while (depth <= opts.maxBackfillMonths && empties < opts.emptyStopPast) {
      const key = await probeMonth(fromMonthId(id));

      if (key) {
        empties = 0;
        if (!known.has(id)) {
          known.add(id);
          keysToAdd.push(key);
        }
      } else {
        empties += 1;
      }

      id -= 1;
      depth += 1;
    }
  }

  // 3) Scan outward to the right
  {
    let empties = 0;
    let depth = 1;
    let id = knownList[knownList.length - 1] + 1;

    while (depth <= opts.maxLookaheadMonths && empties < opts.emptyStopFuture) {
      const key = await probeMonth(fromMonthId(id));

      if (key) {
        empties = 0;
        if (!known.has(id)) {
          known.add(id);
          keysToAdd.push(key);
        }
      } else {
        empties += 1;
      }

      id += 1;
      depth += 1;
    }
  }

  await helpers.withPgClient(async (client) => {
    await upsertFrontierKeys.run({ federation, kind, keys: keysToAdd }, client);
  });
  helpers.logger.info(
    `[${federation}:${kind}] inserted ${keysToAdd.length} new window(s)`,
  );
  return keysToAdd.length;
}

const startOfMonth = (date: Date): Date =>
  new Date(Date.UTC(date.getUTCFullYear(), date.getUTCMonth(), 1));

const toMonthId = (date: Date): number => {
  const d = startOfMonth(date);
  return d.getUTCFullYear() * 12 + d.getUTCMonth();
};

const fromMonthId = (id: number): Date => {
  return new Date(Date.UTC(Math.floor(id / 12), id % 12, 1));
};

export default discover_events;
