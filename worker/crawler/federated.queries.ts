/** Types generated for queries found in "crawler/federated.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type competitor_type = 'couple' | 'duo' | 'formation' | 'solo' | 'team';

export type gender = 'female' | 'male' | 'other' | 'unknown';

export type DateOrString = Date | string;

export type NumberOrString = number | string;

/** 'UpsertFederationAthlete' parameters type */
export interface IUpsertFederationAthleteParams {
  canonicalName?: string | null | void;
  externalId?: string | null | void;
  federation?: string | null | void;
  gender?: gender | null | void;
}

/** 'UpsertFederationAthlete' return type */
export interface IUpsertFederationAthleteResult {
  athlete_id: string | null;
}

/** 'UpsertFederationAthlete' query type */
export interface IUpsertFederationAthleteQuery {
  params: IUpsertFederationAthleteParams;
  result: IUpsertFederationAthleteResult;
}

const upsertFederationAthleteIR: any = {"usedParamSet":{"federation":true,"externalId":true,"canonicalName":true,"gender":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":56,"b":66}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":92,"b":102}]},{"name":"canonicalName","required":false,"transform":{"type":"scalar"},"locs":[{"a":128,"b":141}]},{"name":"gender","required":false,"transform":{"type":"scalar"},"locs":[{"a":167,"b":173}]}],"statement":"SELECT federated.upsert_athlete(\n  in_federation     => :federation,\n  in_external_id    => :externalId,\n  in_canonical_name => :canonicalName,\n  in_gender         => :gender::federated.gender\n) AS athlete_id"};

/**
 * Query generated from SQL:
 * ```
 * SELECT federated.upsert_athlete(
 *   in_federation     => :federation,
 *   in_external_id    => :externalId,
 *   in_canonical_name => :canonicalName,
 *   in_gender         => :gender::federated.gender
 * ) AS athlete_id
 * ```
 */
export const upsertFederationAthlete = new PreparedQuery<IUpsertFederationAthleteParams,IUpsertFederationAthleteResult>(upsertFederationAthleteIR);


/** 'UpsertCategory' parameters type */
export interface IUpsertCategoryParams {
  ageGroup?: string | null | void;
  class?: string | null | void;
  discipline?: string | null | void;
  genderGroup?: string | null | void;
  series?: string | null | void;
}

/** 'UpsertCategory' return type */
export interface IUpsertCategoryResult {
  id: string | null;
}

/** 'UpsertCategory' query type */
export interface IUpsertCategoryQuery {
  params: IUpsertCategoryParams;
  result: IUpsertCategoryResult;
}

const upsertCategoryIR: any = {"usedParamSet":{"series":true,"discipline":true,"ageGroup":true,"genderGroup":true,"class":true},"params":[{"name":"series","required":false,"transform":{"type":"scalar"},"locs":[{"a":55,"b":61}]},{"name":"discipline","required":false,"transform":{"type":"scalar"},"locs":[{"a":85,"b":95}]},{"name":"ageGroup","required":false,"transform":{"type":"scalar"},"locs":[{"a":119,"b":127}]},{"name":"genderGroup","required":false,"transform":{"type":"scalar"},"locs":[{"a":151,"b":162}]},{"name":"class","required":false,"transform":{"type":"scalar"},"locs":[{"a":186,"b":191}]}],"statement":"SELECT federated.upsert_category(\n  in_series       => :series,\n  in_discipline   => :discipline,\n  in_age_group    => :ageGroup,\n  in_gender_group => :genderGroup,\n  in_class        => :class\n) as id"};

/**
 * Query generated from SQL:
 * ```
 * SELECT federated.upsert_category(
 *   in_series       => :series,
 *   in_discipline   => :discipline,
 *   in_age_group    => :ageGroup,
 *   in_gender_group => :genderGroup,
 *   in_class        => :class
 * ) as id
 * ```
 */
export const upsertCategory = new PreparedQuery<IUpsertCategoryParams,IUpsertCategoryResult>(upsertCategoryIR);


/** 'UpsertCompetitor' parameters type */
export interface IUpsertCompetitorParams {
  components?: string | null | void;
  federation?: string | null | void;
  federationCompetitorId?: string | null | void;
  label?: string | null | void;
  type?: competitor_type | null | void;
}

/** 'UpsertCompetitor' return type */
export interface IUpsertCompetitorResult {
  competitor_id: string | null;
}

/** 'UpsertCompetitor' query type */
export interface IUpsertCompetitorQuery {
  params: IUpsertCompetitorParams;
  result: IUpsertCompetitorResult;
}

const upsertCompetitorIR: any = {"usedParamSet":{"federation":true,"federationCompetitorId":true,"type":true,"label":true,"components":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":55,"b":65}]},{"name":"federationCompetitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":88,"b":110}]},{"name":"type","required":false,"transform":{"type":"scalar"},"locs":[{"a":126,"b":130}]},{"name":"label","required":false,"transform":{"type":"scalar"},"locs":[{"a":174,"b":179}]},{"name":"components","required":false,"transform":{"type":"scalar"},"locs":[{"a":293,"b":303}]}],"statement":"SELECT federated.upsert_competitor(\n  in_federation => :federation,\n  in_external_id => :federationCompetitorId,\n  in_type => :type::federated.competitor_type,\n  in_label => :label,\n  in_components => (SELECT array_agg(x::federated.competitor_component_input) FROM jsonb_to_recordset(COALESCE(:components::text::jsonb, '[]'::jsonb)) as x(\n    athlete_id bigint,\n    role federated.competitor_role\n  ))\n) as competitor_id"};

/**
 * Query generated from SQL:
 * ```
 * SELECT federated.upsert_competitor(
 *   in_federation => :federation,
 *   in_external_id => :federationCompetitorId,
 *   in_type => :type::federated.competitor_type,
 *   in_label => :label,
 *   in_components => (SELECT array_agg(x::federated.competitor_component_input) FROM jsonb_to_recordset(COALESCE(:components::text::jsonb, '[]'::jsonb)) as x(
 *     athlete_id bigint,
 *     role federated.competitor_role
 *   ))
 * ) as competitor_id
 * ```
 */
export const upsertCompetitor = new PreparedQuery<IUpsertCompetitorParams,IUpsertCompetitorResult>(upsertCompetitorIR);


/** 'UpsertManyCompetitors' parameters type */
export interface IUpsertManyCompetitorsParams {
  competitors: readonly ({
    federation: string | null | void,
    federationCompetitorId: string | null | void,
    type: string | null | void,
    label: string | null | void
  })[];
}

/** 'UpsertManyCompetitors' return type */
export interface IUpsertManyCompetitorsResult {
  federated_id: string | null;
  federation_id: string | null;
}

/** 'UpsertManyCompetitors' query type */
export interface IUpsertManyCompetitorsQuery {
  params: IUpsertManyCompetitorsParams;
  result: IUpsertManyCompetitorsResult;
}

const upsertManyCompetitorsIR: any = {"usedParamSet":{"competitors":true},"params":[{"name":"competitors","required":false,"transform":{"type":"pick_array_spread","keys":[{"name":"federation","required":false},{"name":"federationCompetitorId","required":false},{"name":"type","required":false},{"name":"label","required":false}]},"locs":[{"a":295,"b":306}]}],"statement":"SELECT\n  federation_id,\n  federated.upsert_competitor(\n    in_federation => federation,\n    in_external_id => federation_id,\n    in_type => type::federated.competitor_type,\n    in_label => label,\n    in_components => '{}'::federated.competitor_component_input[]\n  ) as federated_id\nFROM (VALUES :competitors) as t(federation, federation_id, type, label)"};

/**
 * Query generated from SQL:
 * ```
 * SELECT
 *   federation_id,
 *   federated.upsert_competitor(
 *     in_federation => federation,
 *     in_external_id => federation_id,
 *     in_type => type::federated.competitor_type,
 *     in_label => label,
 *     in_components => '{}'::federated.competitor_component_input[]
 *   ) as federated_id
 * FROM (VALUES :competitors) as t(federation, federation_id, type, label)
 * ```
 */
export const upsertManyCompetitors = new PreparedQuery<IUpsertManyCompetitorsParams,IUpsertManyCompetitorsResult>(upsertManyCompetitorsIR);


/** 'UpsertCompetitorProgress' parameters type */
export interface IUpsertCompetitorProgressParams {
  categoryId?: NumberOrString | null | void;
  competitorId?: NumberOrString | null | void;
  domesticFinale?: number | null | void;
  federation?: string | null | void;
  foreignFinale?: number | null | void;
  points?: NumberOrString | null | void;
}

/** 'UpsertCompetitorProgress' return type */
export interface IUpsertCompetitorProgressResult {
  upsert_competitor_category_progress: undefined | null;
}

/** 'UpsertCompetitorProgress' query type */
export interface IUpsertCompetitorProgressQuery {
  params: IUpsertCompetitorProgressParams;
  result: IUpsertCompetitorProgressResult;
}

const upsertCompetitorProgressIR: any = {"usedParamSet":{"federation":true,"competitorId":true,"categoryId":true,"points":true,"domesticFinale":true,"foreignFinale":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":78,"b":88}]},{"name":"competitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":115,"b":127}]},{"name":"categoryId","required":false,"transform":{"type":"scalar"},"locs":[{"a":154,"b":164}]},{"name":"points","required":false,"transform":{"type":"scalar"},"locs":[{"a":191,"b":197}]},{"name":"domesticFinale","required":false,"transform":{"type":"scalar"},"locs":[{"a":239,"b":253}]},{"name":"foreignFinale","required":false,"transform":{"type":"scalar"},"locs":[{"a":285,"b":298}]}],"statement":"SELECT federated.upsert_competitor_category_progress(\n  in_federation      => :federation,\n  in_competitor_id   => :competitorId,\n  in_category_id     => :categoryId,\n  in_points          => :points::numeric(10,3),\n  in_domestic_finale => :domesticFinale::int,\n  in_foreign_finale  => :foreignFinale::int\n)"};

/**
 * Query generated from SQL:
 * ```
 * SELECT federated.upsert_competitor_category_progress(
 *   in_federation      => :federation,
 *   in_competitor_id   => :competitorId,
 *   in_category_id     => :categoryId,
 *   in_points          => :points::numeric(10,3),
 *   in_domestic_finale => :domesticFinale::int,
 *   in_foreign_finale  => :foreignFinale::int
 * )
 * ```
 */
export const upsertCompetitorProgress = new PreparedQuery<IUpsertCompetitorProgressParams,IUpsertCompetitorProgressResult>(upsertCompetitorProgressIR);


/** 'UpsertRanklistSnapshot' parameters type */
export interface IUpsertRanklistSnapshotParams {
  asOfDate?: DateOrString | null | void;
  categoryId?: NumberOrString | null | void;
  entries?: string | null | void;
  federation?: string | null | void;
  kind?: string | null | void;
  ranklistName?: string | null | void;
}

/** 'UpsertRanklistSnapshot' return type */
export interface IUpsertRanklistSnapshotResult {
  snapshot_id: string | null;
}

/** 'UpsertRanklistSnapshot' query type */
export interface IUpsertRanklistSnapshotQuery {
  params: IUpsertRanklistSnapshotParams;
  result: IUpsertRanklistSnapshotResult;
}

const upsertRanklistSnapshotIR: any = {"usedParamSet":{"federation":true,"categoryId":true,"ranklistName":true,"asOfDate":true,"kind":true,"entries":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":66,"b":76}]},{"name":"categoryId","required":false,"transform":{"type":"scalar"},"locs":[{"a":102,"b":112}]},{"name":"ranklistName","required":false,"transform":{"type":"scalar"},"locs":[{"a":138,"b":150}]},{"name":"asOfDate","required":false,"transform":{"type":"scalar"},"locs":[{"a":176,"b":184}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":225,"b":229}]},{"name":"entries","required":false,"transform":{"type":"scalar"},"locs":[{"a":267,"b":274}]}],"statement":"SELECT federated.upsert_ranklist_snapshot(\n  in_federation     => :federation,\n  in_category_id    => :categoryId,\n  in_ranklist_name  => :ranklistName,\n  in_as_of_date     => :asOfDate::date,\n  in_kind           => COALESCE(:kind, 'default'),\n  in_entries        => :entries::text::jsonb\n) AS snapshot_id"};

/**
 * Query generated from SQL:
 * ```
 * SELECT federated.upsert_ranklist_snapshot(
 *   in_federation     => :federation,
 *   in_category_id    => :categoryId,
 *   in_ranklist_name  => :ranklistName,
 *   in_as_of_date     => :asOfDate::date,
 *   in_kind           => COALESCE(:kind, 'default'),
 *   in_entries        => :entries::text::jsonb
 * ) AS snapshot_id
 * ```
 */
export const upsertRanklistSnapshot = new PreparedQuery<IUpsertRanklistSnapshotParams,IUpsertRanklistSnapshotResult>(upsertRanklistSnapshotIR);


