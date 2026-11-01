import type { GraphCacheKeysConfig } from '@/graphql';

type CacheKeyFields = {
  [Type in keyof GraphCacheKeysConfig]?:
    readonly (keyof Parameters<NonNullable<GraphCacheKeysConfig[Type]>>[0])[] | null;
};

export const cacheKeyFields = {
  AddressDomain: null,
  AgeGroup: ['code'],
  CompetitionEntry: ['competitionId', 'competitorId'],
  CompetitionOfficial: ['competitionId', 'personId', 'role'],
  CompetitionResult: ['competitionId', 'competitorId'],
  CompetitionSuggestionsRecord: null,
  CrawlerTargetSummary: null,
  CrawlerTargetFailureGroup: null,
  DatetimeRange: null,
  DatetimeRangeBound: null,
  EventApproxPriceRecord: null,
  EventConflict: null,
  EventFacetsRecord: null,
  EventRegistrationInfo: null,
  EventStat: null,
  Federation: ['code'],
  FederationCoupleAgeRule: ['ageGroupCode'],
  FederationCompetitorFacetsRecord: null,
  FederationEventFacet: null,
  FederationEventYear: null,
  FederationCompetitorComponent: ['competitorId', 'personId'],
  FederationCompetitorCategoryProgress: ['competitorId', 'categoryId'],
  FederationEventOfficial: ['eventId', 'personId', 'role', 'discipline'],
  IntRange: null,
  IntRangeBound: null,
  LocationImage: ['fileId'],
  PersonFederationCandidatesRecord: null,
  PersonFacetsRecord: null,
  PersonLastActivity: null,
  Price: null,
  RanklistEntry: ['snapshotId', 'competitorId'],
  ScoreboardRecord: ['personId'],
  TenantSetting: ['tenantId'],
} as const satisfies CacheKeyFields;

export const cacheKeys = Object.fromEntries(
  Object.entries(cacheKeyFields).map(
    ([type, fields]) =>
      [
        type,
        (data: Record<string, unknown>) => {
          if (!fields) return null;
          const values = fields.map((field) => data[field]);
          if (values.some((value) => value == null)) return null;
          return values.length === 1 && !values[0] ? null : values.join(':');
        },
      ] as const,
  ),
) satisfies GraphCacheKeysConfig;
