import stringify from 'fast-json-stable-stringify';
import { createHash } from 'node:crypto';
import type {
  Athlete,
  AthletesResponse,
  FetchAthletesOptions,
  RankingPoints,
} from './athletes.ts';
import { fetchAthletesByIdt, parseAthletesResponse } from './athletes.ts';
import { iterateAthleteIdts } from './idts.ts';
import type { Pool } from 'pg';

export interface SynchronizeAthletesOptions extends FetchAthletesOptions {
  signal?: AbortSignal;
  onFetchError?: (idt: number, error: unknown) => void | Promise<void>;
  cacheMaxAgeMs?: number;
}

const INGEST_TYPE_ATHLETE = 'athlete';

interface IngestRecordRow {
  hash: string;
  payload: unknown;
  created_at: string;
  checked_at: string;
}

function delay(ms: number): Promise<void> {
  return new Promise((resolve) => {
    setTimeout(resolve, ms);
  });
}

function computePayloadHash(payload: unknown): string {
  const serialized = stringify(payload, {
    replacer(key, value) {
      if (key === 'validFor') {
        return undefined;
      }
      return value;
    },
  });
  if (typeof serialized !== 'string') {
    throw new TypeError('Unable to serialize payload for hashing');
  }
  return createHash('sha256').update(serialized).digest('hex');
}

async function loadIngestRecord(
  client: Pool,
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

async function touchIngestRecord(
  client: Pool,
  type: string,
  url: string,
  hash: string,
): Promise<void> {
  await client.query(
    `
      update csts.ingest
      set checked_at = now()
      where type = $1 and url = $2 and hash = $3
    `,
    [type, url, hash],
  );
}

async function upsertIngestRecord(
  client: Pool,
  type: string,
  url: string,
  hash: string,
  payload: unknown,
): Promise<void> {
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
  client: Pool,
  options: SynchronizeAthletesOptions = {},
): Promise<void> {
  const { signal, onFetchError, init, cacheMaxAgeMs = 0 } = options;

  let notFoundContiguous = 0;

  for (const idt of iterateAthleteIdts()) {
    signal?.throwIfAborted?.();

    const url = `https://www.csts.cz/api/1/athletes/${idt}`;
    const ingestRecord = await loadIngestRecord(client, INGEST_TYPE_ATHLETE, url);

    let response: AthletesResponse | undefined;
    let shouldUpdateTables = true;

    if (ingestRecord && isRecordFresh(ingestRecord, cacheMaxAgeMs)) {
      try {
        response = parseAthletesResponse(ingestRecord.payload);
        shouldUpdateTables = false;
        await touchIngestRecord(
          client,
          INGEST_TYPE_ATHLETE,
          url,
          ingestRecord.hash,
        );
      } catch {
        response = undefined;
        shouldUpdateTables = true;
      }
    }

    if (!response) {
      await delay(250);
      signal?.throwIfAborted?.();

      try {
        response = await fetchAthletesByIdt(idt, { init });
      } catch (error) {
        if (onFetchError) {
          await onFetchError(idt, error);
          continue;
        }

        throw error;
      }

      const payloadHash = computePayloadHash(response);

      if (!ingestRecord) {
        await upsertIngestRecord(
          client,
          INGEST_TYPE_ATHLETE,
          url,
          payloadHash,
          response,
        );
      } else if (ingestRecord.hash === payloadHash) {
        shouldUpdateTables = false;
        await touchIngestRecord(
          client,
          INGEST_TYPE_ATHLETE,
          url,
          ingestRecord.hash,
        );
      } else {
        await upsertIngestRecord(
          client,
          INGEST_TYPE_ATHLETE,
          url,
          payloadHash,
          response,
        );
      }
    }

    if (!response) {
      continue;
    }

    if (response.collection.length === 0) {
      console.log(`${idt} not found`);
      notFoundContiguous++;
      if (notFoundContiguous > 100) {
        console.log("Didn't find 100 in a row, we might be at the end");
        break;
      }
      continue;
    }
    notFoundContiguous = 0;
    console.log(idt);

    if (!shouldUpdateTables) {
      continue;
    }

    for (const athlete of response.collection) {
      signal?.throwIfAborted?.();

      await client.query('begin');
      try {
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
    }
  }
}

async function upsertAthlete(client: Pool, athlete: Athlete): Promise<void> {
  await client.query(
    `
      insert into csts.athlete (
        idt,
        name,
        valid_for,
        age_category,
        sex,
        medical_checkup_expiration,
        barcode_url
      )
      values ($1, $2, $3::date, $4, $5, $6::date, $7)
      on conflict (idt) do update set
        name = excluded.name,
        valid_for = excluded.valid_for,
        age_category = excluded.age_category,
        sex = excluded.sex,
        medical_checkup_expiration = excluded.medical_checkup_expiration,
        barcode_url = excluded.barcode_url,
        fetched_at = now()
    `,
    [
      athlete.idt,
      athlete.name,
      athlete.validFor,
      athlete.age,
      athlete.sex,
      athlete.medicalCheckupExpiration,
      athlete.barcode,
    ],
  );
}

async function upsertAthleteRanking(
  client: Pool,
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
        personal_foreign_finale_count,
        personal_approved
      )
      values ($1, $2, $3, $4, $5, $6, $7, $8)
      on conflict (athlete_id, discipline, series) do update set
        personal_class = excluded.personal_class,
        personal_points = excluded.personal_points,
        personal_domestic_finale_count = excluded.personal_domestic_finale_count,
        personal_foreign_finale_count = excluded.personal_foreign_finale_count,
        personal_approved = excluded.personal_approved
    `,
    [
      athlete.idt,
      ranking.discipline,
      ranking.series,
      ranking.personalClass,
      ranking.personalPoints,
      ranking.personalDomesticFinaleCount,
      ranking.personalForeignFinaleCount,
      ranking.personalApproved,
    ],
  );
}

async function upsertCompetitorRanking(
  client: Pool,
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
  client: Pool,
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
        medical_checkup_expiration,
        formed_at
      )
      values ($1, $2, $3, $4, $5::date, $6::timestamptz)
      on conflict (id) do update set
        couple_idt = excluded.couple_idt,
        man_idt = excluded.man_idt,
        woman_idt = excluded.woman_idt,
        medical_checkup_expiration = excluded.medical_checkup_expiration,
        formed_at = excluded.formed_at
    `,
    [
      ranking.competitorId,
      ranking.idt,
      participants.manIdt,
      participants.womanIdt,
      ranking.medicalCheckupExpiration,
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
