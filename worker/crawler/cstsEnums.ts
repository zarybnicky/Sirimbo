import { z } from 'zod';
import type { competitor_type } from './federated.queries.ts';

export const numberAsEnum = <T extends string>(enumDict: { [key in T]: number }) =>
  z.union([
    z
      .number()
      .refine((x) => Object.values(enumDict).includes(x))
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

export const ranklistCompetitorPointsType: {
  [key in RanklistCompetitorPointsType]: number;
} = {
  NationalChampionship: 0,
  LeagueCompetition: 1,
  WDSFCompetition: 2,
  WDSFRanklist: 3,
  G_Cup: 4,
};

type RanklistType = 'Continuous' | 'Nomination' | 'Championship';

export const ranklistType: { [key in RanklistType]: number } = {
  Continuous: 0,
  Nomination: 1,
  Championship: 2,
};

type CompetitorType =
  | 'Couple'
  | 'SoloDancer'
  | 'DuoF'
  | 'Group'
  | 'ProAm'
  | 'Team'
  | 'Formation'
  | 'SmallTeam'
  | 'BigTeam'
  | 'SoloM'
  | 'SoloF'
  | 'Trio'
  | 'TrioF';

export const competitorType: { [key in CompetitorType]: number } = {
  Couple: 1,
  SoloDancer: 2,
  DuoF: 3,
  Group: 4,
  ProAm: 5,
  Team: 6,
  Formation: 7,
  SmallTeam: 8,
  BigTeam: 9,
  SoloM: 10,
  SoloF: 11,
  Trio: 12,
  TrioF: 13,
};

export const mapCompetitorType = (type: CompetitorType): competitor_type => {
  switch (type) {
    case 'DuoF':
      return 'duo';
    case 'SoloDancer':
    case 'SoloM':
    case 'SoloF':
      return 'solo';
    case 'Trio':
    case 'TrioF':
      return 'trio';
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

export const disciplineType: { [key in DisciplineType]: number } = {
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

export const seriesType: { [key in SeriesType]: number } = {
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

export const competitionType: { [key in CompetitionType]: number } = {
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

export const ageGroup: { [key in AgeGroup]: number } = {
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

export const competitionClassType: { [key in CompetitionClassType]: number } = {
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
