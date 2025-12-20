import z from 'zod';
import type { JsonLoader } from './types.ts';
import {
  type competitor_type,
  upsertCategory,
  upsertManyCompetitors,
  upsertRanklistSnapshot,
} from './federated.queries.ts';
import type { PoolClient } from 'pg';

const numberAsEnum = <T extends string>(enumDict: { [key in T]: number }) =>
  z.union([
    z
      .number()
      .refine((x) => Object.values(enumDict).includes(x as any))
      .transform((x) => Object.entries(enumDict).find(([_, n]) => x === n)?.[0]! as T),
    z
      .string()
      .refine((x) => Object.keys(enumDict).includes(x))
      .transform((x) => x as T),
  ]);

type RanklistCompetitorPointsType =
  | 'NationalChampionship'
  | 'LeagueCompetition'
  | 'WDSFCompetition'
  | 'WDSFRanklist'
  | 'G_Cup';

const ranklistCompetitorPointsType: { [key in RanklistCompetitorPointsType]: number } = {
  NationalChampionship: 0,
  LeagueCompetition: 1,
  WDSFCompetition: 2,
  WDSFRanklist: 3,
  G_Cup: 4,
};

type RanklistType = 'Continuous' | 'Nomination' | 'Championship';

const ranklistType: { [key in RanklistType]: number } = {
  Continuous: 0,
  Nomination: 1,
  Championship: 2,
};

type CompetitorType =
  | 'Couple'
  | 'SoloDancer'
  | 'Duo'
  | 'Group'
  | 'ProAm'
  | 'Team'
  | 'Formation'
  | 'SmallTeam'
  | 'BigTeam';

const competitorType: { [key in CompetitorType]: number } = {
  Couple: 1,
  SoloDancer: 2,
  Duo: 3,
  Group: 4,
  ProAm: 5,
  Team: 6,
  Formation: 7,
  SmallTeam: 8,
  BigTeam: 9,
};

const mapCompetitorType = (type: CompetitorType): competitor_type => {
  switch (type) {
    case 'Duo':
      return 'duo';
    case 'SoloDancer':
      return 'solo';
    case 'Formation':
      return 'formation';
    case 'Group':
    case 'SmallTeam':
    case 'Team':
    case 'BigTeam':
      return 'team';
    case 'ProAm':
    case 'Couple':
    default:
      return 'couple';
  }
};

type DisciplineType =
  | 'Unknown'
  | 'Standard'
  | 'Latin'
  | 'Standard_Latin'
  | 'Breaking'
  | 'SingleDance'
  | 'FreeStyle'
  | 'Other'
  | 'ShowdanceStandard'
  | 'ShowdanceLatin'
  | 'TenDance'
  | 'Bachata'
  | 'Merengue'
  | 'Salsa'
  | 'ShowdanceBachata'
  | 'ShowdanceMerengue'
  | 'ShowdanceSalsa'
  | 'Caribbean';

const disciplineType: { [key in DisciplineType]: number } = {
  Unknown: 0,
  Standard: 1,
  Latin: 2,
  Standard_Latin: 3,
  Breaking: 4,
  SingleDance: 5,
  FreeStyle: 6,
  Other: 7,
  ShowdanceStandard: 8,
  ShowdanceLatin: 9,
  TenDance: 10,
  Bachata: 11,
  Merengue: 12,
  Salsa: 13,
  ShowdanceBachata: 14,
  ShowdanceMerengue: 15,
  ShowdanceSalsa: 16,
  Caribbean: 17,
};

type SeriesType = 'Unknown' | 'DanceForAll' | 'Professional' | 'DanceSport' | 'Caribbean';

const seriesType: { [key in SeriesType]: number } = {
  Unknown: 0,
  DanceForAll: 1,
  Professional: 2,
  DanceSport: 3,
  Caribbean: 4,
};

type CompetitionType =
  | 'Unknown'
  | 'Cup'
  | 'Ranking'
  | 'League'
  | 'Championship'
  | 'TopLevel'
  | 'SuperLeague'
  | 'G_Cup';

const competitionType: { [key in CompetitionType]: number } = {
  Unknown: 0,
  Cup: 1,
  Ranking: 2,
  League: 3,
  Championship: 4,
  TopLevel: 5,
  SuperLeague: 6,
  G_Cup: 7,
};

type AgeGroup =
  | 'Unknown'
  | 'Under_8'
  | 'Juvenile_I'
  | 'Juvenile_II'
  | 'Juvenile'
  | 'Junior_I'
  | 'Junior_II'
  | 'Junior'
  | 'Youth'
  | 'Under_21'
  | 'Adult'
  | 'Senior_I'
  | 'Senior_Ia'
  | 'Senior_Ib'
  | 'Senior_II'
  | 'Senior_III'
  | 'Senior_IV'
  | 'Senior_V'
  | 'Senior';

const ageGroup: { [key in AgeGroup]: number } = {
  Unknown: 0,
  Under_8: 1,
  Juvenile_I: 2,
  Juvenile_II: 3,
  Juvenile: 4,
  Junior_I: 5,
  Junior_II: 6,
  Junior: 7,
  Youth: 8,
  Under_21: 9,
  Adult: 10,
  Senior_I: 11,
  Senior_Ia: 17,
  Senior_Ib: 18,
  Senior_II: 12,
  Senior_III: 13,
  Senior_IV: 14,
  Senior_V: 15,
  Senior: 16,
};

type CompetitionClassType =
  | 'Unknown'
  | 'Entry'
  | 'E'
  | 'D'
  | 'C'
  | 'B'
  | 'A'
  | 'M'
  | 'Open'
  | 'Novice'
  | 'Bronze'
  | 'Silver'
  | 'Gold';

const competitionClassType: { [key in CompetitionClassType]: number } = {
  Unknown: 0,
  Entry: 1,
  E: 2,
  D: 3,
  C: 4,
  B: 5,
  A: 6,
  M: 7,
  Open: 8,
  Novice: 9,
  Bronze: 10,
  Silver: 11,
  Gold: 12,
};

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
