/** Types generated for queries found in "crawler/federated.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type competitor_role = 'follow' | 'lead' | 'member' | 'substitute';

export type competitor_type = 'couple' | 'duo' | 'formation' | 'solo' | 'team' | 'trio';

export type gender = 'female' | 'male' | 'other' | 'unknown';

export type DateOrString = Date | string;

export type NumberOrString = number | string;

export type NumberOrStringArray = (NumberOrString)[];

export type competitor_roleArray = (competitor_role)[];

export type competitor_typeArray = (competitor_type)[];

export type stringArray = (string)[];

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
  competitorType?: competitor_type | null | void;
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

const upsertCategoryIR: any = {"usedParamSet":{"series":true,"discipline":true,"ageGroup":true,"genderGroup":true,"class":true,"competitorType":true},"params":[{"name":"series","required":false,"transform":{"type":"scalar"},"locs":[{"a":55,"b":61}]},{"name":"discipline","required":false,"transform":{"type":"scalar"},"locs":[{"a":85,"b":95}]},{"name":"ageGroup","required":false,"transform":{"type":"scalar"},"locs":[{"a":119,"b":127}]},{"name":"genderGroup","required":false,"transform":{"type":"scalar"},"locs":[{"a":151,"b":162}]},{"name":"class","required":false,"transform":{"type":"scalar"},"locs":[{"a":186,"b":191}]},{"name":"competitorType","required":false,"transform":{"type":"scalar"},"locs":[{"a":218,"b":232}]}],"statement":"SELECT federated.upsert_category(\n  in_series       => :series,\n  in_discipline   => :discipline,\n  in_age_group    => :ageGroup,\n  in_gender_group => :genderGroup,\n  in_class        => :class,\n  in_competitor_type => :competitorType::federated.competitor_type\n) as id"};

/**
 * Query generated from SQL:
 * ```
 * SELECT federated.upsert_category(
 *   in_series       => :series,
 *   in_discipline   => :discipline,
 *   in_age_group    => :ageGroup,
 *   in_gender_group => :genderGroup,
 *   in_class        => :class,
 *   in_competitor_type => :competitorType::federated.competitor_type
 * ) as id
 * ```
 */
export const upsertCategory = new PreparedQuery<IUpsertCategoryParams,IUpsertCategoryResult>(upsertCategoryIR);


/** 'UpsertCompetitor' parameters type */
export interface IUpsertCompetitorParams {
  component_athlete_ids?: NumberOrStringArray | null | void;
  component_roles?: competitor_roleArray | null | void;
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

const upsertCompetitorIR: any = {"usedParamSet":{"federation":true,"federationCompetitorId":true,"type":true,"label":true,"component_athlete_ids":true,"component_roles":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":55,"b":65}]},{"name":"federationCompetitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":88,"b":110}]},{"name":"type","required":false,"transform":{"type":"scalar"},"locs":[{"a":126,"b":130}]},{"name":"label","required":false,"transform":{"type":"scalar"},"locs":[{"a":174,"b":179}]},{"name":"component_athlete_ids","required":false,"transform":{"type":"scalar"},"locs":[{"a":303,"b":324}]},{"name":"component_roles","required":false,"transform":{"type":"scalar"},"locs":[{"a":343,"b":358}]}],"statement":"SELECT federated.upsert_competitor(\n  in_federation => :federation,\n  in_external_id => :federationCompetitorId,\n  in_type => :type::federated.competitor_type,\n  in_label => :label,\n  in_components => ARRAY(\n    SELECT (u.athlete_id, u.role)::federated.competitor_component_input\n    FROM unnest(\n      :component_athlete_ids::bigint[],\n      :component_roles::federated.competitor_role[]\n    ) AS u(athlete_id, role)\n  )\n) as competitor_id"};

/**
 * Query generated from SQL:
 * ```
 * SELECT federated.upsert_competitor(
 *   in_federation => :federation,
 *   in_external_id => :federationCompetitorId,
 *   in_type => :type::federated.competitor_type,
 *   in_label => :label,
 *   in_components => ARRAY(
 *     SELECT (u.athlete_id, u.role)::federated.competitor_component_input
 *     FROM unnest(
 *       :component_athlete_ids::bigint[],
 *       :component_roles::federated.competitor_role[]
 *     ) AS u(athlete_id, role)
 *   )
 * ) as competitor_id
 * ```
 */
export const upsertCompetitor = new PreparedQuery<IUpsertCompetitorParams,IUpsertCompetitorResult>(upsertCompetitorIR);


/** 'UpsertManyCompetitors' parameters type */
export interface IUpsertManyCompetitorsParams {
  external_ids?: stringArray | null | void;
  federations?: stringArray | null | void;
  labels?: stringArray | null | void;
  types?: competitor_typeArray | null | void;
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

const upsertManyCompetitorsIR: any = {"usedParamSet":{"federations":true,"external_ids":true,"types":true,"labels":true},"params":[{"name":"federations","required":false,"transform":{"type":"scalar"},"locs":[{"a":339,"b":350}]},{"name":"external_ids","required":false,"transform":{"type":"scalar"},"locs":[{"a":363,"b":375}]},{"name":"types","required":false,"transform":{"type":"scalar"},"locs":[{"a":388,"b":393}]},{"name":"labels","required":false,"transform":{"type":"scalar"},"locs":[{"a":427,"b":433}]}],"statement":"SELECT\n  input.external_id AS federation_id,\n  federated.upsert_competitor(\n    in_federation  => input.federation,\n    in_external_id => input.external_id,\n    in_type        => input.competitor_type,\n    in_label       => input.label,\n    in_components  => '{}'::federated.competitor_component_input[]\n  ) AS federated_id\nFROM unnest(\n  :federations::text[],\n  :external_ids::text[],\n  :types::federated.competitor_type[],\n  :labels::text[]\n) AS input(federation, external_id, competitor_type, label)"};

/**
 * Query generated from SQL:
 * ```
 * SELECT
 *   input.external_id AS federation_id,
 *   federated.upsert_competitor(
 *     in_federation  => input.federation,
 *     in_external_id => input.external_id,
 *     in_type        => input.competitor_type,
 *     in_label       => input.label,
 *     in_components  => '{}'::federated.competitor_component_input[]
 *   ) AS federated_id
 * FROM unnest(
 *   :federations::text[],
 *   :external_ids::text[],
 *   :types::federated.competitor_type[],
 *   :labels::text[]
 * ) AS input(federation, external_id, competitor_type, label)
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


