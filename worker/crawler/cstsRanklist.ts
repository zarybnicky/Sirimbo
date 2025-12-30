import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import {
  upsertCategory,
  upsertManyCompetitors,
  upsertRanklistSnapshot,
} from './federated.queries.ts';
import type { PoolClient } from 'pg';
import {
  ageGroup,
  competitorType,
  disciplineType,
  mapCompetitorType,
  numberAsEnum,
  ranklistCompetitorPointsType,
  ranklistType,
  seriesType,
} from './cstsEnums.ts';

const ranklistEntrySchema = z.object({
  competitorId: z.number(),
  competitorName: z.string(),
  ranking: z.number(),
  rankingTo: z.number(),
  pointsChampionship: z.number(),
  pointsLeague: z.number(),
  pointsWdsf: z.number(),
  points: z.number(),
  higherAge: z.boolean(),
  events: z.array(
    z.object({
      date: z.string(),
      type: numberAsEnum(ranklistCompetitorPointsType),
      name: z.string(),
      points: z.number(),
      used: z.boolean(),
    }),
  ),
});

const ranklistSnapshotSchema = z.object({
  id: z.number(),
  state: z.number(),
  competitorType: numberAsEnum(competitorType),
  series: numberAsEnum(seriesType),
  date: z.string(),
  type: numberAsEnum(ranklistType),
  age: numberAsEnum(ageGroup),
  discipline: numberAsEnum(disciplineType),
  coefChampionship: z.number(),
  coefLeague: z.number(),
  coefWdsf: z.number(),
  competitors: z.array(ranklistEntrySchema),
});

const responseSchema = z.object({
  entity: ranklistSnapshotSchema,
});

type Response = z.infer<typeof responseSchema>;
type Ranklist = z.infer<typeof ranklistSnapshotSchema>;

export const cstsRanklist: JsonLoader<Response> = {
  mode: 'json',
  revalidatePeriod: '5 day',
  buildRequest: (key) => ({
    url: new URL(`https://www.csts.cz/api/1/ranklist/${key}`),
    init: {
      headers: {
        referrer: 'https://www.csts.cz/dancesport/kalendar_akci',
      },
    },
  }),
  schema: responseSchema,
  async load(client, frontier, parsed) {
    await loadCstsRanklist(client, parsed.entity);
  },
};

async function loadCstsRanklist(client: PoolClient, entity: Ranklist) {
  const [{ id: categoryId }] = await upsertCategory.run(
    {
      class: '',
      ageGroup: entity.age,
      genderGroup: 'mixed', // ČSTS distinguishes this only in competitions
      discipline: entity.discipline,
      series: entity.series,
    },
    client,
  );

  const competitors = entity.competitors.map((competitor) => ({
    type: mapCompetitorType(entity.competitorType),
    label: competitor.competitorName,
    federation: 'csts',
    federationCompetitorId: competitor.competitorId.toString(),
  }));
  const competitorIds =
    competitors.length > 0
      ? await upsertManyCompetitors.run({ competitors }, client)
      : [];
  const idMap = new Map(competitorIds.map((x) => [x.federation_id, x.federated_id]));

  const ranklistComponents: {
    competitor_id: string;
    ranking: number;
    ranking_to: number;
    points: number;
  }[] = [];

  for (const competitor of entity.competitors) {
    const federatedId = idMap.get(competitor.competitorId.toString());
    if (federatedId) {
      ranklistComponents.push({
        competitor_id: federatedId,
        ranking: competitor.ranking,
        ranking_to: competitor.rankingTo,
        points:
          competitor.points +
          competitor.pointsWdsf +
          competitor.pointsLeague +
          competitor.pointsWdsf,
      });
    }
  }

  await upsertRanklistSnapshot.run(
    {
      federation: 'csts',
      categoryId,
      ranklistName: [entity.series, entity.discipline, entity.age].join(' '),
      asOfDate: entity.date,
      kind: entity.type,
      entries: JSON.stringify(ranklistComponents),
    },
    client,
  );
}
