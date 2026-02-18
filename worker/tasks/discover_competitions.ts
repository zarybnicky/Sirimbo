import type { JobHelpers, Task } from 'graphile-worker';
import {
  getExistingFrontierKeys,
  getFrontierKeyBounds,
  reserveRequest,
  upsertFrontier,
} from '../crawler/crawler.queries.ts';
import { LOADER_MAP, type LoaderIds } from '../crawler/handlers.ts';
import { fetchFrontier } from './frontier_fetch.ts';

const discover_competitions: Task<'discover_competitions'> = async (
  _payload,
  helpers,
) => {
  const targets: Array<LoaderIds & Windows> = [
    { federation: 'wdsf', kind: 'competitionIndex', hot: 3, back: 12, forward: 12 },
    { federation: 'csts', kind: 'competitionIndex', hot: 3, back: 12, forward: 12 },
    { federation: 'csts', kind: 'resultsIndex', hot: 3, back: 12, forward: 0 },
  ];
  await Promise.allSettled(targets.map((x) => discover(helpers, x)));
};

async function discover(
  helpers: JobHelpers,
  { federation, kind, ...windows }: LoaderIds & Windows,
) {
  const { logger, withPgClient } = helpers;
  const loader = LOADER_MAP[federation]?.[kind];

  const probe = async (key: string) => {
    const { url, init = {} } = loader.buildRequest(key);
    await acquireSlot(helpers, url.host);

    const { fetchStatus, error } = await fetchFrontier(loader, url, init);
    logger.info(
      `${federation}:${kind} ${key}: ${fetchStatus}` + (error ? ` ${error}` : ``),
    );

    if (fetchStatus === 'ok') {
      await withPgClient((client) =>
        upsertFrontier.run({ federation, kind, key }, client),
      );
      return true;
    }
    return false;
  };

  const [{ min_key, max_key }] = await withPgClient((client) =>
    getFrontierKeyBounds.run({ federation, kind }, client),
  );
  const candidates = buildCandidates(windows, min_key, max_key);
  if (candidates.length === 0) return;

  const existingRows = await withPgClient((client) =>
    getExistingFrontierKeys.run({ federation, kind, keys: candidates }, client),
  );
  const existing = new Set(existingRows.map((r) => r.key as string));
  for (const key of candidates) {
    if (existing.has(key)) continue;
    await probe(key);
  }
}

const acquireSlot = async (helpers: JobHelpers, host: string) => {
  while (true) {
    const [{ granted, allowed_at }] = await helpers.withPgClient((client) =>
      reserveRequest.run({ host }, client),
    );
    if (granted) return;
    const waitMs = allowed_at!.getTime() - Date.now();
    await new Promise((r) => setTimeout(r, waitMs));
  }
};

const monthKey = (d: Date) => d.toISOString().slice(0, 7);
const addMonths = (key: string, delta: number): string => {
  const d = new Date(`${key}-01T00:00:00Z`);
  d.setUTCMonth(d.getUTCMonth() + delta);
  return monthKey(d);
};

type Windows = {
  hot: number;
  back: number;
  forward: number;
};

const buildCandidates = (w: Windows, minKey: string | null, maxKey: string | null) => {
  const s = new Set<string>();
  const nowKey = monthKey(new Date());

  for (let i = 0; i < w.hot; i += 1) {
    s.add(addMonths(nowKey, i));
    s.add(addMonths(nowKey, -i));
  }
  for (let i = 0; i < w.back; i += 1) s.add(addMonths(minKey ?? nowKey, -i - 1));
  for (let i = 0; i < w.forward; i += 1) s.add(addMonths(maxKey ?? nowKey, i + 1));

  return [...s].sort(); // YYYY-MM lex order == chronological
};

export default discover_competitions;
