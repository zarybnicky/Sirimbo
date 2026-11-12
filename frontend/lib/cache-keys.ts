import type { GraphCacheKeysConfig } from '@/graphql';

type CacheKeyFields = {
  [Type in keyof GraphCacheKeysConfig]?:
    readonly (keyof Parameters<NonNullable<GraphCacheKeysConfig[Type]>>[0])[] | null;
};

export const cacheKeyFields = {
  AddressDomain: null,
  AnnouncementAttachment: ['announcementId', 'fileId'],
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
  EventSearchCondition: null,
  EventStat: null,
  Federation: ['code'],
  FederationCompetitorCategoryProgress: ['competitorId', 'categoryId'],
  FederationCompetitorComponent: ['competitorId', 'personId'],
  FederationCompetitorFacetsRecord: null,
  FederationCoupleAgeRule: ['ageGroupCode'],
  FederationEventFacet: null,
  FederationEventOfficial: ['eventId', 'personId', 'role', 'discipline'],
  FederationEventYear: null,
  FederationPeopleFacetsRecord: null,
  GlobalSearchResult: null,
  IntRange: null,
  IntRangeBound: null,
  LocationImage: ['fileId'],
  PersonFacetsRecord: null,
  PaymentDebtorPriceRecord: null,
  PaymentFacetsRecord: null,
  PersonFederationCandidatesRecord: null,
  PersonLastActivity: null,
  Price: null,
  RanklistEntry: ['snapshotId', 'competitorId'],
  ScoreboardRecord: ['personId'],
  TenantSetting: ['tenantId'],
  TransactionFacetsRecord: null,
} as const satisfies CacheKeyFields;

export const cacheKeys = Object.fromEntries(
  Object.entries(cacheKeyFields).map(
    ([type, fields]) =>
      [
        type,
        (data: Record<string, unknown>) => {
          if (!fields) return null;
          const values = fields.map((field) => data[field]);
          if (values.some((x) => !x)) return null;
          return values.length === 1 && !values[0] ? null : values.join(':');
        },
      ] as const,
  ),
) satisfies GraphCacheKeysConfig;
