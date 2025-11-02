import type { PoolClient } from 'pg';
import {
  deleteIngestByUrls,
  loadIngestRecord,
  selectMaxAthleteIdt,
  updateLatestIngestError,
} from './athlete.queries.ts';
import { iterateAthleteIdts, MIN_IDT } from './idts.ts';
import type { BaseSyncOptions } from './sync.ts';
import {
  ATHLETE_API_BASE_URL,
  formatError,
  INGEST_TYPE,
  syncAthlete,
} from './sync.ts';

export interface DiscoverAthletesOptions extends BaseSyncOptions {
  lastFoundIdt?: number;
  lastCheckedIdt?: number;
}

export interface DiscoverAthletesResult {
  lastFoundIdt?: number;
  lastCheckedIdt?: number;
}

function* iterateAthleteIdtsAfter(startExclusive?: number): Generator<number> {
  let started = startExclusive === undefined;
  for (const idt of iterateAthleteIdts()) {
    if (!started) {
      if (idt <= (startExclusive as number)) {
        continue;
      }
      started = true;
    }
    yield idt;
  }
}

export async function discoverAthletes(
  client: PoolClient,
  options: DiscoverAthletesOptions = {},
): Promise<DiscoverAthletesResult> {
  const { signal, onFetchError, maxRequests } = options;

  let lastFoundIdt = options.lastFoundIdt;
  let lastCheckedIdt = options.lastCheckedIdt;

  if (lastCheckedIdt === undefined) {
    const [row] = await selectMaxAthleteIdt.run(undefined, client);
    lastCheckedIdt = row?.max ?? MIN_IDT - 1;
  }

  let requestsSent = 0;
  const notFoundContiguous = new Set<string>();

  for (const idt of iterateAthleteIdtsAfter(lastCheckedIdt)) {
    const url = `${ATHLETE_API_BASE_URL}${idt}`;
    const cachedRows = await loadIngestRecord.run({ type: INGEST_TYPE, url }, client);
    const cachedRecord = cachedRows[0] ?? null;

    try {
      const result = await syncAthlete({ client, idt, signal }, cachedRecord);
      requestsSent += 1;

      if (result.status === 'not_found') {
        lastCheckedIdt = idt;
        notFoundContiguous.add(url);
        if (notFoundContiguous.size > 100 && idt > 10_600_000) {
          await deleteIngestByUrls.run({ type: INGEST_TYPE, urls: [...notFoundContiguous] }, client);
          break;
        }
        if (maxRequests && requestsSent >= maxRequests) {
          break;
        }
        continue;
      }

      notFoundContiguous.clear();

      if (result.athlete) {
        lastFoundIdt = idt;
      }
      lastCheckedIdt = idt;
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
      notFoundContiguous.clear();
      if (onFetchError) {
        await onFetchError(idt, error);
      } else {
        throw error;
      }
      requestsSent += 1;
      // Skip advancing the checkpoint so discovery will retry this IDT soon.
      if (maxRequests && requestsSent >= maxRequests) {
        break;
      }
      continue;
    }

    if (maxRequests && requestsSent >= maxRequests) {
      break;
    }
  }

  return {
    lastFoundIdt,
    lastCheckedIdt,
  };
}
