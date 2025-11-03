import stringify from 'json-stringify-deterministic';
import { createHash } from 'node:crypto';
import type { PoolClient } from 'pg';
import {
  loadIngestRecord,
  selectAthletesToRefresh,
  touchIngestRecord,
  updateAthleteLastChecked,
  updateLatestIngestError,
  upsertAthlete,
  upsertAthleteRanking,
  upsertCompetitorRanking,
  upsertCouple,
  upsertIngestRecord,
  type ILoadIngestRecordResult,
} from './athlete.queries.ts';
import {
  fetchAthletesByIdt,
  parseAthletesResponse,
  type Athlete,
} from './athletes.ts';

export const INGEST_TYPE = 'athlete';
const REQUEST_DELAY_MS = 250;
const DEFAULT_CACHE_MAX_AGE_MS = 1000 * 60 * 60 * 24;
export const ATHLETE_API_BASE_URL = 'https://www.csts.cz/api/1/athletes/';

const stringifyOptions: Parameters<typeof stringify>[1] = {
  replacer(key, value) {
    return key === 'validFor' ? undefined : value;
  },
};

export interface BaseSyncOptions {
  signal?: AbortSignal;
  onFetchError?: (idt: number, error: unknown) => void | Promise<void>;
  maxRequests?: number;
}

export interface RefreshAthletesOptions extends BaseSyncOptions {
  cacheMaxAgeMs?: number;
}

export interface RefreshAthletesResult {
  refreshed: number;
}

interface SyncContext {
  client: PoolClient;
  idt: number;
  signal?: AbortSignal;
}

interface SyncOutcome {
  status: 'updated' | 'unchanged' | 'not_found';
  athlete: Athlete | null;
}

export function formatError(error: unknown): string {
  if (error instanceof Error) {
    return `${error.name}: ${error.message}`;
  }
  return String(error);
}

async function persistAthleteData(client: PoolClient, athlete: Athlete): Promise<void> {
  await client.query('begin');
  try {
    await upsertAthlete.run(athlete, client);

    for (const ranking of athlete.rankingPoints) {
      await upsertAthleteRanking.run({ ...ranking, athleteId: athlete.idt }, client);

      if (!ranking.id || !ranking.competitorId) continue;

      if (ranking.competitors === 'Couple') {
        const coupleId = await upsertCouple
          .run({ ...ranking, athleteIdt: athlete.idt }, client)
          .then((x) => x[0]?.id ?? null);
        if (coupleId !== null) {
          await upsertCompetitorRanking.run({ ...ranking, coupleId }, client);
        }
      } else {
        await upsertCompetitorRanking.run(
          { ...ranking, athleteIdt: athlete.idt },
          client,
        );
      }
    }

    await client.query('commit');
  } catch (error) {
    await client.query('rollback');
    throw error;
  }
}

export async function syncAthlete(
  context: SyncContext,
  cachedRecord: ILoadIngestRecordResult | null,
): Promise<SyncOutcome> {
  const { client, idt, signal } = context;
  const url = `${ATHLETE_API_BASE_URL}${idt}`;

  signal?.throwIfAborted?.();

  await new Promise((resolve) => setTimeout(resolve, REQUEST_DELAY_MS));

  signal?.throwIfAborted?.();

  const rawResponse = await fetchAthletesByIdt(idt);

  if (rawResponse === null) {
    return { status: 'not_found', athlete: null };
  }

  const parsedResponse = parseAthletesResponse(rawResponse);
  const athlete = parsedResponse.collection[0] ?? null;

  if (!athlete) {
    return { status: 'not_found', athlete: null };
  }

  const payloadHash = createHash('sha256')
    .update(stringify(rawResponse, stringifyOptions))
    .digest('hex');

  if (cachedRecord?.hash === payloadHash) {
    await touchIngestRecord.run({ type: INGEST_TYPE, url, hash: payloadHash }, client);
    await updateAthleteLastChecked.run({ idt }, client);
    return { status: 'unchanged', athlete };
  }

  await upsertIngestRecord.run(
    {
      type: INGEST_TYPE,
      url,
      hash: payloadHash,
      payload: rawResponse as any,
    },
    client,
  );
  await persistAthleteData(client, athlete);
  await updateAthleteLastChecked.run({ idt }, client);

  return { status: 'updated', athlete };
}

export async function refreshAthletes(
  client: PoolClient,
  options: RefreshAthletesOptions = {},
): Promise<RefreshAthletesResult> {
  const { cacheMaxAgeMs = DEFAULT_CACHE_MAX_AGE_MS, signal, onFetchError, maxRequests } = options;
  const threshold = new Date(Date.now() - cacheMaxAgeMs);
  const limit = maxRequests ?? 100;

  const rows = await selectAthletesToRefresh.run(
    { threshold, limit },
    client,
  );

  let refreshed = 0;

  for (const row of rows) {
    const idt = row.idt;
    const url = `${ATHLETE_API_BASE_URL}${idt}`;
    const cachedRows = await loadIngestRecord.run({ type: INGEST_TYPE, url }, client);
    const cachedRecord = cachedRows[0] ?? null;

    try {
      const result = await syncAthlete({ client, idt, signal }, cachedRecord);

      if (result.status === 'not_found') {
        await updateAthleteLastChecked.run({ idt }, client);
        continue;
      }

      refreshed += 1;
    } catch (error) {
      if (cachedRecord) {
        await updateLatestIngestError.run(
          {
            type: INGEST_TYPE,
            url,
            hash: cachedRecord.hash,
            lastError: formatError(error),
          },
          client,
        );
      }
      if (onFetchError) {
        await onFetchError(idt, error);
      } else {
        throw error;
      }
    }
  }

  return { refreshed };
}
