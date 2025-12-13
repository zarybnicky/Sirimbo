import z from 'zod';
import type { JsonLoader } from './types.ts';
import { upsertFrontier } from './crawler.queries.ts';
import { upsertCategory } from './federated.queries.ts';

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
      type: z
        .number()
        .refine((x) => Object.values(ranklistCompetitorPointsType).includes(x as any))
        .transform(
          (x) =>
            Object.entries(ranklistCompetitorPointsType).find(
              ([_, n]) => x === n,
            )?.[0]! as RanklistCompetitorPointsType,
        ),
      name: z.string(),
      points: z.number(),
      used: z.boolean(),
    }),
  ),
});

const ranklistSnapshotSchema = z.object({
  id: z.number(),
  state: z.number(),
  competitorType: z.number(),
  series: z.number(),
  date: z.string(),
  type: z
    .number()
    .refine((x) => Object.values(ranklistType).includes(x as any))
    .transform(
      (x) => Object.entries(ranklistType).find(([_, n]) => x === n)?.[0]! as RanklistType,
    ),
  age: z.number(),
  discipline: z.number(),
  coefChampionship: z.number(),
  coefLeague: z.number(),
  coefWdsf: z.number(),
  competitors: z.array(ranklistEntrySchema),
});

const responseSchema = z.object({
  entity: ranklistSnapshotSchema,
});

export const cstsRanklist: JsonLoader<z.output<typeof responseSchema>> = {
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
  async load(client, frontier, { entity }) {
    const [{ id: categoryId }] = await upsertCategory.run(
      {
        class: entity.class ?? '',
        ageGroup: entity.age,
        genderGroup: 'mixed', // ČSTS distinguishes this only in competitions
        discipline: entity.discipline,
        series: entity.series,
      },
      client,
    );

    parsed.entity.competitors.forEach((competitor) => {});
  },
};
