import stringify from 'json-stringify-deterministic';
import { createHash } from 'node:crypto';
import type { PoolClient } from 'pg';
import {
  deleteIngestByUrls,
  loadIngestRecord,
  touchIngestRecord,
  upsertAthlete,
  upsertAthleteRanking,
  upsertCompetitorRanking,
  upsertCouple,
  upsertIngestRecord,
} from './athlete.queries.ts';
import {
  fetchAthletesByIdt,
  parseAthletesResponse,
  type AthletesResponse,
} from './athletes.ts';
import { iterateAthleteIdts } from './idts.ts';

export interface SynchronizeAthletesOptions {
  signal?: AbortSignal;
  onFetchError?: (idt: number, error: unknown) => void | Promise<void>;
  cacheMaxAgeMs?: number;
  maxRequests?: number;
  lastFoundIdt?: number;
  lastCheckedIdt?: number;
}

const stringifyOptions: Parameters<typeof stringify>[1] = {
  replacer(key, value) {
    return key === 'validFor' ? undefined : value;
  },
};

export async function synchronizeAthletes(
  client: PoolClient,
  options: SynchronizeAthletesOptions = {},
): Promise<{
  lastFoundIdt?: number;
  lastCheckedIdt?: number;
}> {
  let { lastFoundIdt, lastCheckedIdt } = options;
  const { signal, onFetchError, maxRequests, cacheMaxAgeMs = 0 } = options;

  let requestsSent = 0;
  let started = !lastCheckedIdt;
  const notFoundContiguous = new Set<string>();

  for (const idt of iterateAthleteIdts()) {
    signal?.throwIfAborted?.();

    const url = `https://www.csts.cz/api/1/athletes/${idt}`;

    if (!started) {
      if (idt !== lastCheckedIdt) {
        continue;
      } else {
        started = true;
      }
    }

    const cachedRecord = await loadIngestRecord
      .run({ url }, client)
      .then((x) => x[0] ?? null);

    const checkedAt = cachedRecord?.checked_at?.getTime();
    if (
      cacheMaxAgeMs > 0 &&
      Number.isFinite(checkedAt) &&
      Date.now() - checkedAt <= cacheMaxAgeMs
    ) {
      notFoundContiguous.clear();
      continue;
    }

    let rawResponse: unknown;
    let parsedResponse: AthletesResponse | undefined;
    try {
      await new Promise((resolve) => setTimeout(resolve, 250));
      requestsSent += 1;
      lastCheckedIdt = idt;
      rawResponse = await fetchAthletesByIdt(idt);
      parsedResponse = parseAthletesResponse(rawResponse);
    } catch (error) {
      if (onFetchError) {
        await onFetchError(idt, error);
        continue;
      }
      throw error;
    }
    const payloadHash = createHash('sha256')
      .update(stringify(rawResponse, stringifyOptions))
      .digest('hex');

    let shouldSkipProcessing = false;
    if (cachedRecord?.hash === payloadHash) {
      shouldSkipProcessing = true;
      await touchIngestRecord.run({ url, hash: payloadHash }, client);
    } else {
      await upsertIngestRecord.run(
        { url, hash: payloadHash, payload: rawResponse as any },
        client,
      );
    }

    const athlete = parsedResponse.collection[0];
    if (!athlete) {
      console.log(`[csts] ${idt} not found`);
      notFoundContiguous.add(url);
      if (notFoundContiguous.size > 100 && idt > 10600000) {
        console.log(
          "[csts] Didn't find 100 in a row, aborting - we might be at the end of assigned IDTs",
        );
        console.log(`[csts] Last found IDT: ${lastFoundIdt}`);
        await deleteIngestByUrls.run({ urls: [...notFoundContiguous] }, client);
        return {};
      }
      continue;
    }

    if (shouldSkipProcessing) continue;

    lastFoundIdt = idt;
    notFoundContiguous.clear();

    await client.query('begin');
    try {
      console.log(`[csts] ${idt}`);
      await upsertAthlete.run(athlete, client);

      for (const ranking of athlete.rankingPoints) {
        await upsertAthleteRanking.run({ ...ranking, athleteId: athlete.idt }, client);

        if (!ranking.id || !ranking.competitorId) continue;

        if (ranking.competitors === 'Couple') {
          const coupleId = await upsertCouple
            .run({ ...ranking, athleteIdt: athlete.idt }, client)
            .then((x) => x[0]?.id);
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

    if (maxRequests && requestsSent >= maxRequests) {
      return {
        lastCheckedIdt,
        lastFoundIdt,
      };
    }
  }

  // Shouldn't ever happen
  return {};
}
