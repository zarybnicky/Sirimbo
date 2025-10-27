import stringify from 'json-stringify-deterministic';
import { createHash } from 'node:crypto';
import type {
  Athlete,
  AthletesResponse,
  RankingPoints,
} from './athletes.ts';
import { fetchAthletesByIdt, parseAthletesResponse } from './athletes.ts';
import { iterateAthleteIdts } from './idts.ts';
import type { PoolClient } from 'pg';

export interface SynchronizeAthletesOptions {
  signal?: AbortSignal;
  onFetchError?: (idt: number, error: unknown) => void | Promise<void>;
  cacheMaxAgeMs?: number;
  startFromIdt?: number;
  maxRequests?: number;
}

const INGEST_TYPE_ATHLETE = 'athlete';

interface IngestRecordRow {
  hash: string;
  payload: unknown;
  created_at: string;
  checked_at: string;
}

const stringifyOptions: Parameters<typeof stringify>[1] = {
  replacer(key, value) {
    if (key === 'validFor') {
      return undefined;
    }
    return value;
  },
};

async function loadCachedRecord(
  client: PoolClient,
  type: string,
  url: string,
): Promise<IngestRecordRow | null> {
  const { rows } = await client.query<IngestRecordRow>(
    `
      select hash, payload, created_at, checked_at
      from csts.ingest
      where type = $1 and url = $2
      order by checked_at desc, created_at desc
      limit 1
    `,
    [type, url],
  );

  return rows[0] ?? null;
}

async function touchIngestRecord(client: PoolClient, type: string, url: string, hash: string) {
  await client.query(
    `
      update csts.ingest
      set checked_at = now()
      where type = $1 and url = $2 and hash = $3
    `,
    [type, url, hash],
  );
}

async function upsertIngestRecord(client: PoolClient, type: string, url: string, hash: string, payload: unknown,) {
  await client.query(
    `
      insert into csts.ingest (type, url, hash, payload)
      values ($1, $2, $3, $4::jsonb)
      on conflict (type, url, hash) do update set
        payload = excluded.payload,
        checked_at = now()
    `,
    [type, url, hash, JSON.stringify(payload)],
  );
}

function isRecordFresh(record: IngestRecordRow, maxAgeMs: number): boolean {
  if (maxAgeMs <= 0) {
    return false;
  }

  const checkedAt = new Date(record.checked_at).getTime();
  if (!Number.isFinite(checkedAt)) {
    return false;
  }

  return Date.now() - checkedAt <= maxAgeMs;
}

export async function synchronizeAthletes(
  client: PoolClient,
  options: SynchronizeAthletesOptions = {},
): Promise<number> {
  const { signal, onFetchError, startFromIdt, maxRequests, cacheMaxAgeMs = 0 } = options;

  let requestsSent = 0;
  let started = !startFromIdt;
  const notFoundContiguous = new Set<string>();
  let lastFoundUrl: string = '';

  for (const idt of iterateAthleteIdts()) {
    signal?.throwIfAborted?.();

    if (!started && idt !== startFromIdt) continue;

    const url = `https://www.csts.cz/api/1/athletes/${idt}`;
    const cachedRecord = await loadCachedRecord(client, INGEST_TYPE_ATHLETE, url);

    if (cachedRecord && isRecordFresh(cachedRecord, cacheMaxAgeMs)) {
      notFoundContiguous.clear();
      continue;
    }

    let rawResponse: unknown
    let parsedResponse: AthletesResponse | undefined;
    try {
      await new Promise((resolve) => setTimeout(resolve, 250));
      requestsSent += 1;
      rawResponse = await fetchAthletesByIdt(idt);
      parsedResponse = parseAthletesResponse(rawResponse);
    } catch (error) {
      if (onFetchError) {
        await onFetchError(idt, error);
        continue;
      }

      throw error;
    }
    const payloadHash = createHash('sha256').update(stringify(rawResponse, stringifyOptions)).digest('hex');

    let shouldSkipProcessing = false;
    if (cachedRecord?.hash === payloadHash) {
      shouldSkipProcessing = true;
      await touchIngestRecord(client, INGEST_TYPE_ATHLETE, url, payloadHash);
    } else {
      await upsertIngestRecord(client, INGEST_TYPE_ATHLETE, url, payloadHash, rawResponse);
    }

    const athlete = parsedResponse.collection[0];
    if (!athlete) {
      console.log(`[csts] ${idt} not found`);
      notFoundContiguous.add(url);
      if (notFoundContiguous.size > 100 && idt > 10600000) {
        console.log("[csts] Didn't find 100 in a row, aborting - we might be at the end of assigned IDTs");
        console.log(`[csts] Last found URL: ${lastFoundUrl}`)
        await deleteByUrls(client, INGEST_TYPE_ATHLETE, notFoundContiguous);
        return 0;
      }
      continue;
    }

    if (shouldSkipProcessing)
      continue;

    lastFoundUrl = url;
    notFoundContiguous.clear();

    await client.query('begin');
    try {
      console.log(`[csts] ${idt}`);
      await upsertAthlete(client, athlete);

      for (const ranking of athlete.rankingPoints) {
        await upsertAthleteRanking(client, athlete, ranking);

        if (!ranking.id || !ranking.competitorId)
          continue;

        if (ranking.competitors === 'Couple') {
          const coupleId = await upsertCouple(client, athlete, ranking);
          if (coupleId !== null) {
            await upsertCompetitorRanking(client, ranking, {
              athleteIdt: null,
              coupleId,
            });
          }
        } else {
          await upsertCompetitorRanking(client, ranking, {
            athleteIdt: athlete.idt,
            coupleId: null,
          });
        }
      }

      await client.query('commit');
    } catch (error) {
      await client.query('rollback');
      throw error;
    }

    if (maxRequests && requestsSent >= maxRequests)
      return idt;
  }

  // Shouldn't ever happen
  return 0;
}

async function deleteByUrls(client: PoolClient, type: string, urls: Set<string>) {
  await client.query('delete from csts.ingest where type = $1 and url = any($2::text[])', [
    type,
    [...urls],
  ]);
}

async function upsertAthlete(client: PoolClient, athlete: Athlete) {
  await client.query(
    `
      insert into csts.athlete (
        idt,
        name,
        age_category,
        sex,
        medical_checkup_expiration
      )
      values ($1, $2, $3::text, $4, $5::date)
      on conflict (idt) do update set
        name = excluded.name,
        age_category = excluded.age_category,
        sex = excluded.sex,
        medical_checkup_expiration = excluded.medical_checkup_expiration,
        fetched_at = now()
    `,
    [
      athlete.idt,
      athlete.name,
      athlete.age,
      athlete.sex,
      athlete.medicalCheckupExpiration,
    ],
  );
}

async function upsertAthleteRanking(
  client: PoolClient,
  athlete: Athlete,
  ranking: RankingPoints,
): Promise<void> {
  await client.query(
    `
      insert into csts.athlete_ranking (
        athlete_id,
        discipline,
        series,
        personal_class,
        personal_points,
        personal_domestic_finale_count,
        personal_foreign_finale_count
      )
      values ($1, $2, $3, $4, $5, $6, $7)
      on conflict (athlete_id, discipline, series) do update set
        personal_class = excluded.personal_class,
        personal_points = excluded.personal_points,
        personal_domestic_finale_count = excluded.personal_domestic_finale_count,
        personal_foreign_finale_count = excluded.personal_foreign_finale_count
    `,
    [
      athlete.idt,
      ranking.discipline,
      ranking.series,
      ranking.personalClass,
      ranking.personalPoints,
      ranking.personalDomesticFinaleCount,
      ranking.personalForeignFinaleCount,
    ],
  );
}

async function upsertCompetitorRanking(
  client: PoolClient,
  ranking: RankingPoints,
  identifiers: { athleteIdt: number | null; coupleId: number | null },
): Promise<void> {
  await client.query(
    `
      insert into csts.competitor_ranking (
        competitor_id,
        discipline,
        ranking_points_age,
        ranking_age,
        competitor_age,
        series,
        competitors,
        class,
        points,
        domestic_finale_count,
        foreign_finale_count,
        ranklist_ranking,
        ranklist_points,
        athlete_idt,
        couple_id
      )
      values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15)
      on conflict (competitor_id, discipline) do update set
        ranking_points_age = excluded.ranking_points_age,
        ranking_age = excluded.ranking_age,
        competitor_age = excluded.competitor_age,
        series = excluded.series,
        competitors = excluded.competitors,
        class = excluded.class,
        points = excluded.points,
        domestic_finale_count = excluded.domestic_finale_count,
        foreign_finale_count = excluded.foreign_finale_count,
        ranklist_ranking = excluded.ranklist_ranking,
        ranklist_points = excluded.ranklist_points,
        athlete_idt = excluded.athlete_idt,
        couple_id = excluded.couple_id
    `,
    [
      ranking.competitorId,
      ranking.discipline,
      ranking.rankingPointsAge,
      ranking.rankingAge,
      ranking.age,
      ranking.series,
      ranking.competitors,
      ranking.class,
      ranking.points,
      ranking.domesticFinaleCount,
      ranking.foreignFinaleCount,
      ranking.ranklistRanking,
      ranking.ranklistPoints,
      identifiers.athleteIdt,
      identifiers.coupleId,
    ],
  );
}

async function upsertCouple(
  client: PoolClient,
  athlete: Athlete,
  ranking: RankingPoints,
): Promise<number | null> {
  const participants = determineCoupleParticipants(athlete, ranking);
  if (!participants) {
    return null;
  }

  const uniqueParticipants = Array.from(
    new Set([participants.manIdt, participants.womanIdt]),
  );
  const { rowCount } = await client.query<{ idt: number }>(
    `select idt from csts.athlete where idt = any($1::int[])`,
    [uniqueParticipants],
  );

  if (rowCount !== uniqueParticipants.length) {
    return null;
  }

  await client.query(
    `
      insert into csts.couple (
        id,
        couple_idt,
        man_idt,
        woman_idt,
        formed_at
      )
      values ($1, $2, $3, $4, $5::timestamptz)
      on conflict (id) do update set
        couple_idt = excluded.couple_idt,
        man_idt = excluded.man_idt,
        woman_idt = excluded.woman_idt,
        formed_at = excluded.formed_at
    `,
    [
      ranking.competitorId,
      ranking.idt,
      participants.manIdt,
      participants.womanIdt,
      ranking.time,
    ],
  );

  return ranking.competitorId;
}

function determineCoupleParticipants(
  athlete: Athlete,
  ranking: RankingPoints,
): { manIdt: number; womanIdt: number } | null {
  if (!ranking.partnerIdt)
    return null;

  if (athlete.sex === 'M') {
    return { manIdt: athlete.idt, womanIdt: ranking.partnerIdt };
  }

  if (athlete.sex === 'F') {
    return { manIdt: ranking.partnerIdt, womanIdt: athlete.idt };
  }

  return null;
}
