import { z } from 'zod';
import { type JsonLoader } from './types.ts';
import {
  type competitor_role,
  type competitor_type,
  type gender,
  mergeCompetitorComponents,
  mergeCompetitorProgress,
  updatePerson,
  upsertCompetitors,
  upsertPeople,
} from './federated.queries.ts';
import type { PoolClient } from 'pg';
import { mapCompetitorType } from './cstsEnums.ts';
import { getFederatedCategoryId } from './federatedCategory.ts';
import { makePgtypedCollection } from './pgtypedCollection.ts';

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
  collection: z.array(athleteSchema).nonempty(),
});

type Response = z.infer<typeof athletesResponseSchema>;
type Athlete = z.infer<typeof athleteSchema>;
type RankingPoints = z.infer<typeof rankingPointsSchema>;
type ProgressEntry = {
  competitorId: string;
  categoryId: string;
  rank: number;
  domesticFinals: number;
  foreignFinals: number;
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
  mapResponseToStatus({ parsed }) {
    if (!parsed?.collection.length) return 'gone';
    return undefined;
  },
  cleanResponse(_url, parsed) {
    for (const row of parsed.collection) {
      if ('validFor' in row) delete row['validFor'];
    }
    return parsed;
  },
  async load(client, parsed) {
    for (const athlete of parsed.collection) {
      await loadCstsAthlete(client, athlete);
    }
  },
};

async function loadCstsAthlete(client: PoolClient, data: Athlete) {
  const people = makePgtypedCollection<{
    federation: string;
    externalId: string;
    canonicalName: string;
    gender: gender;
  }>(
    ['federation', 'externalId', 'canonicalName', 'gender'],
    ['federation', 'externalId'],
  );

  const competitors = makePgtypedCollection<{
    federation: string;
    externalId: string;
    label: string;
    type: competitor_type;
  }>(['federation', 'externalId', 'label', 'type'], ['federation', 'externalId']);

  const components = makePgtypedCollection<{
    competitorId: string;
    personId: string;
    role: competitor_role;
  }>(['competitorId', 'personId', 'role'], ['competitorId', 'personId']);

  people.add({
    federation: 'csts',
    externalId: data.idt.toString(),
    canonicalName: data.name,
    gender: data.sex,
  });

  const progressByCompetitor = new Map<string, Map<string, ProgressEntry>>();
  for (const rp of data.rankingPoints) {
    if (!rp.competitorId) continue;

    const competitorType = mapCompetitorType(rp.competitors);
    const progressClass = classifyProgressClass(rp.class);
    const categoryId = await getFederatedCategoryId(client, {
      class: progressClass.className,
      ageGroup: rp.rankingAge,
      genderGroup: 'mixed', // ČSTS distinguishes this only in competitions
      discipline: rp.discipline,
      series: rp.series,
      competitorType,
    });
    if (!categoryId) continue;

    const competitorId = `csts:${rp.competitorId}`;
    const mainPersonId = `csts:${data.idt}`;

    if (rp.partnerIdt && (competitorType === 'couple' || competitorType === 'duo')) {
      people.add({
        federation: 'csts',
        externalId: String(rp.partnerIdt),
        canonicalName: rp.partner || '',
        gender:
          competitorType !== 'couple'
            ? data.sex
            : data.sex === 'male'
              ? 'female'
              : 'male',
      });
    }

    competitors.add({
      federation: 'csts',
      externalId: rp.competitorId.toString(),
      label: buildLabel(data, rp, competitorType),
      type: competitorType,
    });
    components.add(...buildComponents(data, rp, competitorType, mainPersonId));

    const bucket = [
      rp.series,
      rp.discipline,
      rp.rankingAge,
      competitorType,
      progressClass.bucket,
    ].join('\u0000');

    const nextProgress = {
      competitorId,
      categoryId,
      rank: progressClass.rank,
      domesticFinals: rp.domesticFinaleCount ?? 0,
      foreignFinals: rp.foreignFinaleCount ?? 0,
      points: rp.points ?? 0,
    };
    const byBucket =
      progressByCompetitor.get(competitorId) ?? new Map<string, ProgressEntry>();
    const currentProgress = byBucket.get(bucket);
    if (!currentProgress || isBetterProgress(nextProgress, currentProgress)) {
      byBucket.set(bucket, nextProgress);
    }
    progressByCompetitor.set(competitorId, byBucket);
  }

  if (people.length) {
    await upsertPeople.run(people.params, client);
    await updatePerson.run(
      {
        federation: 'csts',
        externalId: String(data.idt),
        ageGroup: data.age,
        medicalCheckupExpiration: data.medicalCheckupExpiration,
      },
      client,
    );
  }

  if (competitors.length) {
    await upsertCompetitors.run(competitors.params, client);
  }
  if (components.length) {
    await mergeCompetitorComponents.run(components.params, client);
  }

  const progress = makePgtypedCollection<{
    competitorId: string;
    categoryId: string;
    points: number;
    domesticFinals: number;
    foreignFinals: number;
  }>(['competitorId', 'categoryId', 'points', 'domesticFinals', 'foreignFinals']);
  progress.add(...progressByCompetitor.values().flatMap((x) => x.values()));
  if (progress.length) {
    await mergeCompetitorProgress.run(progress.params, client);
  }
}

function buildLabel(data: Athlete, rp: RankingPoints, type: string): string {
  if (type === 'solo') return data.name;
  if (type === 'couple') {
    return data.sex === 'male'
      ? `${data.name} - ${rp.partner}`
      : `${rp.partner} - ${data.name}`;
  }
  // duo
  return data.idt < (rp.partnerIdt ?? 0)
    ? `${data.name} - ${rp.partner}`
    : `${rp.partner} - ${data.name}`;
}

function buildComponents(
  data: Athlete,
  rp: RankingPoints,
  type: string,
  mainPersonId: string,
): Array<{ competitorId: string; personId: string; role: competitor_role }> {
  const competitorId = `csts:${rp.competitorId}`;
  if (type === 'solo') {
    return [{ competitorId, personId: mainPersonId, role: 'member' }];
  }
  if (type === 'duo') {
    return [
      { competitorId, personId: mainPersonId, role: 'member' },
      { competitorId, personId: `csts:${rp.partnerIdt}`, role: 'member' },
    ];
  }
  if (type === 'couple') {
    const leadId = data.sex === 'male' ? mainPersonId : `csts:${rp.partnerIdt}`;
    const followId = data.sex === 'male' ? `csts:${rp.partnerIdt}` : mainPersonId;
    return [
      { competitorId, personId: leadId, role: 'lead' },
      { competitorId, personId: followId, role: 'follow' },
    ];
  }
  throw new Error('Unsupported competitor type');
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
    candidate.domesticFinals + candidate.foreignFinals >
    current.domesticFinals + current.foreignFinals
  );
}
