import { z } from 'zod';
import { defaultMapResponseToStatus, type JsonLoader } from './types.ts';
import {
  type gender,
  upsertCategory,
  upsertFederationAthlete,
  upsertCompetitor,
  upsertCompetitorProgress,
} from './federated.queries.ts';
import type { PoolClient } from 'pg';
import { mapCompetitorType } from './cstsEnums.ts';

const rankingPointsSchema = z.object({
  id: z.number(),
  idt: z.number().optional(),
  age: z.string().optional(),
  class: z.string().optional(),
  points: z.number().optional(),
  partner: z.string().optional(),
  partnerIdt: z.number().optional(),
  series: z.string(),
  discipline: z.string(),
  competitors: z.enum(['Couple', 'SoloDancer', 'SoloM', 'SoloF', 'Duo', 'DuoF']),
  competitorId: z.number(),
  rankingAge: z.string(),
  rankingPointsAge: z.string(),
  time: z.iso.datetime({ offset: true }).optional(),

  personalApproved: z.boolean().optional(),
  personalClass: z.string().optional(),
  personalPoints: z.number().optional(),
  personalDomesticFinaleCount: z.number().optional(),
  personalForeignFinaleCount: z.number().optional(),
  medicalCheckupExpiration: z.iso.date().optional(),

  domesticFinaleCount: z.number().optional(),
  foreignFinaleCount: z.number().optional(),

  ranklistPoints: z.number().optional(),
  ranklistRanking: z.number().optional(),
});

const athleteSchema = z.object({
  idt: z.number(),
  name: z.string(),
  age: z.string(),
  sex: z.enum(['M', 'F', 'male', 'female']).transform(
    (x): gender =>
      (({
        M: 'male',
        F: 'female',
        male: 'male',
        female: 'female',
      })[x] ?? 'unknown') as gender,
  ),
  medicalCheckupExpiration: z.iso.date().optional().nullable(),
  rankingPoints: z.array(rankingPointsSchema).optional().default([]),
  lat: rankingPointsSchema.optional(),
  stt: rankingPointsSchema.optional(),

  validFor: z.iso.date().optional(),
  barcode: z.string().optional(),
  avatar: z.string().optional(),
});

const athletesResponseSchema = z.object({
  collection: z.array(athleteSchema),
});

type Response = z.infer<typeof athletesResponseSchema>;
type Athlete = z.infer<typeof athleteSchema>;
type RankingPoints = z.infer<typeof rankingPointsSchema>;

export const cstsAthlete: JsonLoader<Response> = {
  mode: 'json',
  schema: athletesResponseSchema,
  revalidatePeriod: '3 day',
  buildRequest: (key) => ({
    url: new URL(`https://www.csts.cz/api/1/athletes/${key}`),
    init: {
      referrer: 'https://www.csts.cz/dancesport/kalendar_akci',
    },
  }),
  mapResponseToStatus(args) {
    if (!args.parsed?.collection.length) return 'gone';
    return defaultMapResponseToStatus(args);
  },
  cleanResponse(url, parsed) {
    for (const row of parsed.collection) {
      if ('validFor' in row) delete row['validFor'];
    }
    return parsed;
  },
  async load(client, frontier, parsed) {
    for (const athlete of parsed.collection) {
      await loadCstsAthlete(client, athlete);
    }
  },
};

async function loadCstsAthlete(client: PoolClient, data: Athlete) {
  const [{ athlete_id: mainAthleteId }] = await upsertFederationAthlete.run(
    {
      federation: 'csts',
      externalId: String(data.idt),
      canonicalName: data.name,
      gender: data.sex,
    },
    client,
  );
  if (mainAthleteId === null) return;

  await client.query(
    `
        UPDATE federated.federation_athlete fa
        SET age_group = $2, medical_checkup_expiration = $3
        WHERE federation = 'csts' AND external_id = $1
      `,
    [String(data.idt), data.age, data.medicalCheckupExpiration],
  );

  for (const rp of data.rankingPoints) {
    if (!rp.competitorId) continue;

    const competitorType = mapCompetitorType(rp.competitors);

    const [{ id: categoryId }] = await upsertCategory.run(
      {
        class: rp.class ?? '',
        ageGroup: rp.rankingAge,
        genderGroup: 'mixed', // ČSTS distinguishes this only in competitions
        discipline: rp.discipline,
        series: rp.series,
        competitorType,
      },
      client,
    );

    const competitorId =
      competitorType === 'couple'
        ? await loadCstsCouple(data, rp, client, mainAthleteId)
        : competitorType === 'duo'
          ? await loadCstsDuo(data, rp, client, mainAthleteId)
          : competitorType === 'solo'
            ? await loadCstsSolo(data, rp, client, mainAthleteId)
        : (() => {throw new Error(`Don't know how to process ${competitorType}`) })();

    await upsertCompetitorProgress.run(
      {
        federation: 'csts',
        competitorId,
        categoryId,
        points: rp.points ?? 0,
        domesticFinale: rp.domesticFinaleCount ?? 0,
        foreignFinale: rp.foreignFinaleCount ?? 0,
      },
      client,
    );
  }
}

async function loadCstsSolo(
  data: Athlete,
  rp: RankingPoints,
  client: PoolClient,
  mainAthleteId: string,
) {
  const [{ competitor_id: competitorId }] = await upsertCompetitor.run(
    {
      federation: 'csts',
      federationCompetitorId: rp.competitorId.toString(),
      label: data.name,
      type: 'solo',
      component_athlete_ids: [mainAthleteId],
      component_roles: ['member'],
    },
    client,
  );
  return competitorId;
}

async function loadCstsDuo(
  data: Athlete,
  rp: RankingPoints,
  client: PoolClient,
  mainAthleteId: string,
) {
  const [{ athlete_id: partnerAthleteId }] = await upsertFederationAthlete.run(
    {
      federation: 'csts',
      externalId: String(rp.partnerIdt),
      canonicalName: rp.partner,
      gender: data.sex === 'male' ? 'male' : 'female',
    },
    client,
  );
  if (!partnerAthleteId) return;

  const [{ competitor_id: competitorId }] = await upsertCompetitor.run(
    {
      federation: 'csts',
      federationCompetitorId: rp.competitorId.toString(),
      label:
        data.idt < (rp.partnerIdt ?? 0)
          ? `${data.name} - ${rp.partner}`
          : `${rp.partner} - ${data.name}`,
      type: 'duo',
      component_athlete_ids: [mainAthleteId, partnerAthleteId],
      component_roles: ['member', 'member'],
    },
    client,
  );

  return competitorId;
}

async function loadCstsCouple(
  data: Athlete,
  rp: RankingPoints,
  client: PoolClient,
  mainAthleteId: string,
) {
  const [{ athlete_id: partnerAthleteId }] = await upsertFederationAthlete.run(
    {
      federation: 'csts',
      externalId: String(rp.partnerIdt),
      canonicalName: rp.partner,
      gender: data.sex === 'male' ? 'female' : 'male',
    },
    client,
  );
  if (!partnerAthleteId) return;

  const [{ competitor_id: competitorId }] = await upsertCompetitor.run(
    {
      federation: 'csts',
      federationCompetitorId: rp.competitorId.toString(),
      label:
        data.sex === 'male'
          ? `${data.name} - ${rp.partner}`
          : `${rp.partner} - ${data.name}`,
      type: 'couple',
      component_athlete_ids: [
        data.sex === 'male' ? mainAthleteId : partnerAthleteId,
        data.sex === 'male' ? partnerAthleteId : mainAthleteId,
      ],
      component_roles: ['lead', 'follow'],
    },
    client,
  );
  return competitorId;
}
