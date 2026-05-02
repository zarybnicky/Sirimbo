import { z } from 'zod';
import { type JsonLoader } from './types.ts';
import {
  type gender,
  replaceCompetitorProgress,
  updatePerson,
  upsertCompetitor,
  upsertPerson,
} from './federated.queries.ts';
import type { PoolClient } from 'pg';
import { mapCompetitorType } from './cstsEnums.ts';
import { getFederatedCategoryId } from './federatedCategory.ts';

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
type ProgressEntry = {
  categoryId: string;
  rank: number;
  domesticFinale: number;
  foreignFinale: number;
  points: number;
};

type ProgressClass = {
  bucket: string;
  className: string;
  rank: number;
};

const MAIN_CLASS_ORDER = ['ENTRY', 'E', 'D', 'C', 'B', 'A', 'S', 'M'];

const MEDAL_CLASS_ORDER = ['NOVICE', 'BRONZE', 'SILVER', 'GOLD'];

export const cstsMember: JsonLoader<Response> = {
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
    if (args.parsed && !args.parsed?.collection.length) return 'gone';
    return undefined;
  },
  cleanResponse(_url, parsed) {
    for (const row of parsed.collection) {
      if ('validFor' in row) delete row['validFor'];
    }
    return parsed;
  },
  async load(client, _frontier, parsed) {
    for (const athlete of parsed.collection) {
      await loadCstsAthlete(client, athlete);
    }
  },
};

async function loadCstsAthlete(client: PoolClient, data: Athlete) {
  await upsertPerson.run(
    {
      federation: 'csts',
      externalId: String(data.idt),
      canonicalName: data.name,
      gender: data.sex,
    },
    client,
  );
  const mainPersonId = `csts:${data.idt}`;
  await updatePerson.run(
    {
      federation: 'csts',
      externalId: String(data.idt),
      ageGroup: data.age,
      medicalCheckupExpiration: data.medicalCheckupExpiration,
    },
    client,
  );

  const byCompetitor = new Map<string, Map<string, ProgressEntry>>();

  for (const rankingPoint of data.rankingPoints) {
    if (!rankingPoint.competitorId) continue;

    const competitorType = mapCompetitorType(rankingPoint.competitors);
    const progressClass = classifyProgressClass(rankingPoint.class);
    const categoryId = await getFederatedCategoryId(client, {
      class: progressClass.className,
      ageGroup: rankingPoint.rankingAge,
      genderGroup: 'mixed', // ČSTS distinguishes this only in competitions
      discipline: rankingPoint.discipline,
      series: rankingPoint.series,
      competitorType,
    });
    if (!categoryId) continue;

    switch (competitorType) {
      case 'couple':
        await loadCstsCouple(data, rankingPoint, client, mainPersonId);
        break;
      case 'duo':
        await loadCstsDuo(data, rankingPoint, client, mainPersonId);
        break;
      case 'solo':
        await loadCstsSolo(data, rankingPoint, client, mainPersonId);
        break;
      default:
        throw new Error(`Don't know how to process ${competitorType}`);
    }
    const competitorId = `csts:${rankingPoint.competitorId}`;

    const bucket = [
      rankingPoint.series,
      rankingPoint.discipline,
      rankingPoint.rankingAge,
      competitorType,
      progressClass.bucket,
    ].join('\u0000');

    const nextProgress = {
      categoryId,
      rank: progressClass.rank,
      domesticFinale: rankingPoint.domesticFinaleCount ?? 0,
      foreignFinale: rankingPoint.foreignFinaleCount ?? 0,
      points: rankingPoint.points ?? 0,
    };

    const byBucket = byCompetitor.get(competitorId) ?? new Map<string, ProgressEntry>();
    const currentProgress = byBucket.get(bucket);
    if (!currentProgress || isBetterProgress(nextProgress, currentProgress)) {
      byBucket.set(bucket, nextProgress);
    }
    byCompetitor.set(competitorId, byBucket);
  }

  for (const [competitorId, progressByBucket] of byCompetitor) {
    const progress = [...progressByBucket.values()];

    await replaceCompetitorProgress.run(
      {
        category_ids: progress.map((entry) => entry.categoryId),
        competitorId,
        domestic_finales: progress.map((entry) => entry.domesticFinale),
        foreign_finales: progress.map((entry) => entry.foreignFinale),
        points: progress.map((entry) => entry.points),
      },
      client,
    );
  }
}

async function loadCstsSolo(
  data: Athlete,
  rp: RankingPoints,
  client: PoolClient,
  mainPersonId: string,
) {
  await upsertCompetitor.run(
    {
      federation: 'csts',
      externalId: rp.competitorId.toString(),
      label: data.name,
      type: 'solo',
      component_person_ids: [mainPersonId],
      component_roles: ['member'],
    },
    client,
  );
}

async function loadCstsDuo(
  data: Athlete,
  rp: RankingPoints,
  client: PoolClient,
  mainPersonId: string,
) {
  await upsertPerson.run(
    {
      federation: 'csts',
      externalId: String(rp.partnerIdt),
      canonicalName: rp.partner,
      gender: data.sex === 'male' ? 'male' : 'female',
    },
    client,
  );
  const partnerPersonId = `csts:${rp.partnerIdt}`;

  await upsertCompetitor.run(
    {
      federation: 'csts',
      externalId: rp.competitorId.toString(),
      label:
        data.idt < (rp.partnerIdt ?? 0)
          ? `${data.name} - ${rp.partner}`
          : `${rp.partner} - ${data.name}`,
      type: 'duo',
      component_person_ids: [mainPersonId, partnerPersonId],
      component_roles: ['member', 'member'],
    },
    client,
  );
}

async function loadCstsCouple(
  data: Athlete,
  rp: RankingPoints,
  client: PoolClient,
  mainPersonId: string,
) {
  await upsertPerson.run(
    {
      federation: 'csts',
      externalId: String(rp.partnerIdt),
      canonicalName: rp.partner,
      gender: data.sex === 'male' ? 'female' : 'male',
    },
    client,
  );
  const partnerPersonId = `csts:${rp.partnerIdt}`;

  await upsertCompetitor.run(
    {
      federation: 'csts',
      externalId: rp.competitorId.toString(),
      label:
        data.sex === 'male'
          ? `${data.name} - ${rp.partner}`
          : `${rp.partner} - ${data.name}`,
      type: 'couple',
      component_person_ids: [
        data.sex === 'male' ? mainPersonId : partnerPersonId,
        data.sex === 'male' ? partnerPersonId : mainPersonId,
      ],
      component_roles: ['lead', 'follow'],
    },
    client,
  );
}

function classifyProgressClass(value: string | null | undefined): ProgressClass {
  const className = value?.trim() ?? '';
  const mainRank = MAIN_CLASS_ORDER.indexOf(className.toUpperCase());
  if (mainRank !== -1) {
    return {
      bucket: 'ladder:main',
      className,
      rank: mainRank,
    };
  }

  const medalRank = MEDAL_CLASS_ORDER.indexOf(className.toUpperCase());
  if (medalRank !== -1) {
    return {
      bucket: 'ladder:medal',
      className,
      rank: medalRank,
    };
  }

  return {
    bucket: `class:${className.toUpperCase()}`,
    className,
    rank: 0,
  };
}

function isBetterProgress(candidate: ProgressEntry, current: ProgressEntry) {
  const classRankDiff = candidate.rank - current.rank;
  if (classRankDiff !== 0) return classRankDiff > 0;

  const pointDiff = candidate.points - current.points;
  if (pointDiff !== 0) return pointDiff > 0;

  return (
    candidate.domesticFinale + candidate.foreignFinale >
    current.domesticFinale + current.foreignFinale
  );
}
