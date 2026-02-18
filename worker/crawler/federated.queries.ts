/** Types generated for queries found in "crawler/federated.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type competitor_role = 'follow' | 'lead' | 'member' | 'substitute';

export type competitor_type = 'couple' | 'duo' | 'formation' | 'solo' | 'team' | 'trio';

export type gender = 'female' | 'male' | 'other' | 'unknown';

export type score_component = 'ajs_cp' | 'ajs_mm' | 'ajs_ps' | 'ajs_reduction' | 'ajs_tq' | 'mark' | 'places';

export type scoring_method = 'ajs-3.0' | 'skating_marks' | 'skating_places';

export type DateOrString = Date | string;

export type NumberOrString = number | string;

export type NumberOrStringArray = (NumberOrString)[];

export type booleanArray = (boolean)[];

export type competitor_roleArray = (competitor_role)[];

export type competitor_typeArray = (competitor_type)[];

export type numberArray = (number)[];

export type score_componentArray = (score_component)[];

export type scoring_methodArray = (scoring_method)[];

export type stringArray = (string)[];

/** 'GetCompetitionWithCategory' parameters type */
export interface IGetCompetitionWithCategoryParams {
  competitionExternalId?: string | null | void;
  federation?: string | null | void;
}

/** 'GetCompetitionWithCategory' return type */
export interface IGetCompetitionWithCategoryResult {
  age_group: string;
  category_id: string;
  class: string;
  competition_id: string;
  competitor_type: string | null;
  discipline: string;
  event_id: string;
  gender_group: string;
  series: string;
  start_date: string | null;
}

/** 'GetCompetitionWithCategory' query type */
export interface IGetCompetitionWithCategoryQuery {
  params: IGetCompetitionWithCategoryParams;
  result: IGetCompetitionWithCategoryResult;
}

const getCompetitionWithCategoryIR: any = {"usedParamSet":{"federation":true,"competitionExternalId":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":333,"b":343}]},{"name":"competitionExternalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":365,"b":386}]}],"statement":"SELECT\n  c.id AS competition_id,\n  c.event_id,\n  c.category_id,\n  c.start_date::text AS start_date,\n  cat.series,\n  cat.discipline,\n  cat.age_group,\n  cat.gender_group,\n  cat.class,\n  cat.competitor_type::text AS competitor_type\nFROM federated.competition c\nJOIN federated.category cat ON cat.id = c.category_id\nWHERE c.federation = :federation AND c.external_id = :competitionExternalId"};

/**
 * Query generated from SQL:
 * ```
 * SELECT
 *   c.id AS competition_id,
 *   c.event_id,
 *   c.category_id,
 *   c.start_date::text AS start_date,
 *   cat.series,
 *   cat.discipline,
 *   cat.age_group,
 *   cat.gender_group,
 *   cat.class,
 *   cat.competitor_type::text AS competitor_type
 * FROM federated.competition c
 * JOIN federated.category cat ON cat.id = c.category_id
 * WHERE c.federation = :federation AND c.external_id = :competitionExternalId
 * ```
 */
export const getCompetitionWithCategory = new PreparedQuery<IGetCompetitionWithCategoryParams,IGetCompetitionWithCategoryResult>(getCompetitionWithCategoryIR);


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


/** 'UpsertCompetitionEntries' parameters type */
export interface IUpsertCompetitionEntriesParams {
  cancelled?: booleanArray | null | void;
  competitionId?: NumberOrString | null | void;
  competitorId?: NumberOrStringArray | null | void;
}

/** 'UpsertCompetitionEntries' return type */
export interface IUpsertCompetitionEntriesResult {
  count: number | null;
}

/** 'UpsertCompetitionEntries' query type */
export interface IUpsertCompetitionEntriesQuery {
  params: IUpsertCompetitionEntriesParams;
  result: IUpsertCompetitionEntriesResult;
}

const upsertCompetitionEntriesIR: any = {"usedParamSet":{"competitionId":true,"competitorId":true,"cancelled":true},"params":[{"name":"competitionId","required":false,"transform":{"type":"scalar"},"locs":[{"a":112,"b":125}]},{"name":"competitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":188,"b":200}]},{"name":"cancelled","required":false,"transform":{"type":"scalar"},"locs":[{"a":217,"b":226}]}],"statement":"WITH ins AS (\n  INSERT INTO federated.competition_entry (competition_id, competitor_id, cancelled)\n  SELECT\n    :competitionId::bigint,\n    competitor_id,\n    cancelled\n  FROM unnest(\n    :competitorId::bigint[],\n    :cancelled::boolean[]\n  ) AS t(competitor_id, cancelled)\n  ON CONFLICT (competition_id, competitor_id)\n    DO UPDATE SET cancelled = EXCLUDED.cancelled\n  RETURNING 1\n)\nSELECT count(*)::int FROM ins"};

/**
 * Query generated from SQL:
 * ```
 * WITH ins AS (
 *   INSERT INTO federated.competition_entry (competition_id, competitor_id, cancelled)
 *   SELECT
 *     :competitionId::bigint,
 *     competitor_id,
 *     cancelled
 *   FROM unnest(
 *     :competitorId::bigint[],
 *     :cancelled::boolean[]
 *   ) AS t(competitor_id, cancelled)
 *   ON CONFLICT (competition_id, competitor_id)
 *     DO UPDATE SET cancelled = EXCLUDED.cancelled
 *   RETURNING 1
 * )
 * SELECT count(*)::int FROM ins
 * ```
 */
export const upsertCompetitionEntries = new PreparedQuery<IUpsertCompetitionEntriesParams,IUpsertCompetitionEntriesResult>(upsertCompetitionEntriesIR);


/** Query 'UpsertCompetitonResults' is invalid, so its result is assigned type 'never'.
 *  */
export type IUpsertCompetitonResultsResult = never;

/** Query 'UpsertCompetitonResults' is invalid, so its parameters are assigned type 'never'.
 *  */
export type IUpsertCompetitonResultsParams = never;

const upsertCompetitonResultsIR: any = {"usedParamSet":{"competition_id":true,"competitor_id":true,"start_number":true,"text":true,"ranking":true,"ranking_to":true},"params":[{"name":"competition_id","required":false,"transform":{"type":"scalar"},"locs":[{"a":137,"b":151}]},{"name":"competitor_id","required":false,"transform":{"type":"scalar"},"locs":[{"a":258,"b":271}]},{"name":"start_number","required":false,"transform":{"type":"scalar"},"locs":[{"a":288,"b":300}]},{"name":"text","required":false,"transform":{"type":"scalar"},"locs":[{"a":301,"b":305}]},{"name":"ranking","required":false,"transform":{"type":"scalar"},"locs":[{"a":314,"b":321}]},{"name":"ranking_to","required":false,"transform":{"type":"scalar"},"locs":[{"a":339,"b":349}]}],"statement":"WITH ins AS (\n  INSERT INTO federated.competition_result (competition_id, competitor_id, start_number, ranking, ranking_to)\n  SELECT\n    :competition_id::bigint,\n    competitor_id,\n    nullif(start_number, ''),\n    ranking,\n    ranking_to\n  FROM unnest(\n    :competitor_id::bigint[],\n    :start_number:text[],\n    :ranking::integer[],\n    :ranking_to::integer[]\n  ) AS t(competitor_id, start_number, ranking, ranking_to)\n  ON CONFLICT (competition_id, competitor_id)\n    DO UPDATE SET\n      start_number = EXCLUDED.start_number,\n      ranking      = EXCLUDED.ranking,\n      ranking_to   = EXCLUDED.ranking_to\n  RETURNING 1\n)\nSELECT count(*)::int FROM ins"};

/**
 * Query generated from SQL:
 * ```
 * WITH ins AS (
 *   INSERT INTO federated.competition_result (competition_id, competitor_id, start_number, ranking, ranking_to)
 *   SELECT
 *     :competition_id::bigint,
 *     competitor_id,
 *     nullif(start_number, ''),
 *     ranking,
 *     ranking_to
 *   FROM unnest(
 *     :competitor_id::bigint[],
 *     :start_number:text[],
 *     :ranking::integer[],
 *     :ranking_to::integer[]
 *   ) AS t(competitor_id, start_number, ranking, ranking_to)
 *   ON CONFLICT (competition_id, competitor_id)
 *     DO UPDATE SET
 *       start_number = EXCLUDED.start_number,
 *       ranking      = EXCLUDED.ranking,
 *       ranking_to   = EXCLUDED.ranking_to
 *   RETURNING 1
 * )
 * SELECT count(*)::int FROM ins
 * ```
 */
export const upsertCompetitonResults = new PreparedQuery<IUpsertCompetitonResultsParams,IUpsertCompetitonResultsResult>(upsertCompetitonResultsIR);


/** 'UpsertCompetitionRoundResults' parameters type */
export interface IUpsertCompetitionRoundResultsParams {
  competitorId?: NumberOrString | null | void;
  overallRanking?: numberArray | null | void;
  overallRankingTo?: numberArray | null | void;
  overallScores?: NumberOrStringArray | null | void;
  qualifiedNext?: booleanArray | null | void;
  roundIds?: NumberOrStringArray | null | void;
}

/** 'UpsertCompetitionRoundResults' return type */
export type IUpsertCompetitionRoundResultsResult = void;

/** 'UpsertCompetitionRoundResults' query type */
export interface IUpsertCompetitionRoundResultsQuery {
  params: IUpsertCompetitionRoundResultsParams;
  result: IUpsertCompetitionRoundResultsResult;
}

const upsertCompetitionRoundResultsIR: any = {"usedParamSet":{"competitorId":true,"roundIds":true,"overallRanking":true,"overallRankingTo":true,"qualifiedNext":true,"overallScores":true},"params":[{"name":"competitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":166,"b":178}]},{"name":"roundIds","required":false,"transform":{"type":"scalar"},"locs":[{"a":287,"b":295}]},{"name":"overallRanking","required":false,"transform":{"type":"scalar"},"locs":[{"a":310,"b":324}]},{"name":"overallRankingTo","required":false,"transform":{"type":"scalar"},"locs":[{"a":340,"b":356}]},{"name":"qualifiedNext","required":false,"transform":{"type":"scalar"},"locs":[{"a":372,"b":385}]},{"name":"overallScores","required":false,"transform":{"type":"scalar"},"locs":[{"a":401,"b":414}]}],"statement":"INSERT INTO federated.competition_round_result\n  (round_id, competitor_id, overall_ranking, overall_ranking_to, qualified_next, overall_score)\nSELECT\n  s.round_id,\n  :competitorId::bigint,\n  s.overall_ranking,\n  s.overall_ranking_to,\n  s.qualified_next,\n  s.overall_score\nFROM unnest(\n  :roundIds::bigint[],\n  :overallRanking::integer[],\n  :overallRankingTo::integer[],\n  :qualifiedNext::boolean[],\n  :overallScores::numeric[]\n) AS s(round_id, overall_ranking, overall_ranking_to, qualified_next, overall_score)\nON CONFLICT (round_id, competitor_id)\n  DO UPDATE SET\n    overall_ranking     = EXCLUDED.overall_ranking,\n    overall_ranking_to  = EXCLUDED.overall_ranking_to,\n    qualified_next      = EXCLUDED.qualified_next,\n    overall_score       = EXCLUDED.overall_score"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO federated.competition_round_result
 *   (round_id, competitor_id, overall_ranking, overall_ranking_to, qualified_next, overall_score)
 * SELECT
 *   s.round_id,
 *   :competitorId::bigint,
 *   s.overall_ranking,
 *   s.overall_ranking_to,
 *   s.qualified_next,
 *   s.overall_score
 * FROM unnest(
 *   :roundIds::bigint[],
 *   :overallRanking::integer[],
 *   :overallRankingTo::integer[],
 *   :qualifiedNext::boolean[],
 *   :overallScores::numeric[]
 * ) AS s(round_id, overall_ranking, overall_ranking_to, qualified_next, overall_score)
 * ON CONFLICT (round_id, competitor_id)
 *   DO UPDATE SET
 *     overall_ranking     = EXCLUDED.overall_ranking,
 *     overall_ranking_to  = EXCLUDED.overall_ranking_to,
 *     qualified_next      = EXCLUDED.qualified_next,
 *     overall_score       = EXCLUDED.overall_score
 * ```
 */
export const upsertCompetitionRoundResults = new PreparedQuery<IUpsertCompetitionRoundResultsParams,IUpsertCompetitionRoundResultsResult>(upsertCompetitionRoundResultsIR);


/** 'UpsertJudgeScores' parameters type */
export interface IUpsertJudgeScoresParams {
  categoryId?: NumberOrString | null | void;
  competitionId?: NumberOrString | null | void;
  competitorId?: NumberOrString | null | void;
  component?: score_componentArray | null | void;
  danceCode?: stringArray | null | void;
  eventDate?: DateOrString | null | void;
  eventId?: NumberOrString | null | void;
  federation?: string | null | void;
  judgeId?: NumberOrStringArray | null | void;
  rawScore?: stringArray | null | void;
  roundId?: NumberOrStringArray | null | void;
  score?: NumberOrStringArray | null | void;
}

/** 'UpsertJudgeScores' return type */
export interface IUpsertJudgeScoresResult {
  count: number | null;
}

/** 'UpsertJudgeScores' query type */
export interface IUpsertJudgeScoresQuery {
  params: IUpsertJudgeScoresParams;
  result: IUpsertJudgeScoresResult;
}

const upsertJudgeScoresIR: any = {"usedParamSet":{"federation":true,"eventDate":true,"eventId":true,"competitionId":true,"categoryId":true,"competitorId":true,"roundId":true,"danceCode":true,"judgeId":true,"component":true,"score":true,"rawScore":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":207,"b":217}]},{"name":"eventDate","required":false,"transform":{"type":"scalar"},"locs":[{"a":224,"b":233}]},{"name":"eventId","required":false,"transform":{"type":"scalar"},"locs":[{"a":240,"b":247}]},{"name":"competitionId","required":false,"transform":{"type":"scalar"},"locs":[{"a":254,"b":267}]},{"name":"categoryId","required":false,"transform":{"type":"scalar"},"locs":[{"a":274,"b":284}]},{"name":"competitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":335,"b":347}]},{"name":"roundId","required":false,"transform":{"type":"scalar"},"locs":[{"a":421,"b":428}]},{"name":"danceCode","required":false,"transform":{"type":"scalar"},"locs":[{"a":445,"b":454}]},{"name":"judgeId","required":false,"transform":{"type":"scalar"},"locs":[{"a":469,"b":476}]},{"name":"component","required":false,"transform":{"type":"scalar"},"locs":[{"a":493,"b":502}]},{"name":"score","required":false,"transform":{"type":"scalar"},"locs":[{"a":538,"b":543}]},{"name":"rawScore","required":false,"transform":{"type":"scalar"},"locs":[{"a":561,"b":569}]}],"statement":"WITH ins AS (\n  INSERT INTO federated.judge_score\n    (federation, event_date, event_id, competition_id, category_id, round_id, dance_code, judge_id, competitor_id, component, score, raw_score)\n  SELECT\n    :federation,\n    :eventDate,\n    :eventId,\n    :competitionId,\n    :categoryId,\n    round_id,\n    dance_code,\n    judge_id,\n    :competitorId,\n    component,\n    score,\n    nullif(raw_score, '')\n  FROM unnest(\n    :roundId::bigint[],\n    :danceCode::text[],\n    :judgeId::bigint[],\n    :component::federated.score_component[],\n    :score::numeric[],\n    :rawScore::text[]\n  ) AS t(round_id, dance_code, judge_id, component, score, raw_score)\n  ON CONFLICT (round_id, dance_code, judge_id, competitor_id, component)\n    DO UPDATE SET\n      score     = EXCLUDED.score,\n      raw_score = EXCLUDED.raw_score\n  RETURNING 1\n)\nSELECT count(*)::int FROM ins"};

/**
 * Query generated from SQL:
 * ```
 * WITH ins AS (
 *   INSERT INTO federated.judge_score
 *     (federation, event_date, event_id, competition_id, category_id, round_id, dance_code, judge_id, competitor_id, component, score, raw_score)
 *   SELECT
 *     :federation,
 *     :eventDate,
 *     :eventId,
 *     :competitionId,
 *     :categoryId,
 *     round_id,
 *     dance_code,
 *     judge_id,
 *     :competitorId,
 *     component,
 *     score,
 *     nullif(raw_score, '')
 *   FROM unnest(
 *     :roundId::bigint[],
 *     :danceCode::text[],
 *     :judgeId::bigint[],
 *     :component::federated.score_component[],
 *     :score::numeric[],
 *     :rawScore::text[]
 *   ) AS t(round_id, dance_code, judge_id, component, score, raw_score)
 *   ON CONFLICT (round_id, dance_code, judge_id, competitor_id, component)
 *     DO UPDATE SET
 *       score     = EXCLUDED.score,
 *       raw_score = EXCLUDED.raw_score
 *   RETURNING 1
 * )
 * SELECT count(*)::int FROM ins
 * ```
 */
export const upsertJudgeScores = new PreparedQuery<IUpsertJudgeScoresParams,IUpsertJudgeScoresResult>(upsertJudgeScoresIR);


/** Query 'UpsertAgeGroupRules' is invalid, so its result is assigned type 'never'.
 *  */
export type IUpsertAgeGroupRulesResult = never;

/** Query 'UpsertAgeGroupRules' is invalid, so its parameters are assigned type 'never'.
 *  */
export type IUpsertAgeGroupRulesParams = never;

const upsertAgeGroupRulesIR: any = {"usedParamSet":{"federation":true,"validFrom":true,"validTo":true,"name":true,"fromAge":true,"toAge":true,"minBirthdate":true,"maxBirthdate":true,"allowedToDanceIn":true,"division":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":200,"b":210}]},{"name":"validFrom","required":false,"transform":{"type":"scalar"},"locs":[{"a":245,"b":254}]},{"name":"validTo","required":false,"transform":{"type":"scalar"},"locs":[{"a":279,"b":286}]},{"name":"name","required":false,"transform":{"type":"scalar"},"locs":[{"a":425,"b":429}]},{"name":"fromAge","required":false,"transform":{"type":"scalar"},"locs":[{"a":442,"b":449}]},{"name":"toAge","required":false,"transform":{"type":"scalar"},"locs":[{"a":461,"b":466}]},{"name":"minBirthdate","required":false,"transform":{"type":"scalar"},"locs":[{"a":478,"b":490}]},{"name":"maxBirthdate","required":false,"transform":{"type":"scalar"},"locs":[{"a":503,"b":515}]},{"name":"allowedToDanceIn","required":false,"transform":{"type":"scalar"},"locs":[{"a":528,"b":544}]},{"name":"division","required":false,"transform":{"type":"scalar"},"locs":[{"a":559,"b":567}]}],"statement":"INSERT INTO federated.federation_age_group_rule (\n  federation,\n  name,\n  valid_from,\n  valid_to,\n  from_age,\n  to_age,\n  min_birthdate,\n  max_birthdate,\n  allowed_to_dance_in,\n  divisions\n)\nSELECT\n  :federation::text AS federation,\n  t.name,\n  :validFrom::date AS valid_from,\n  :validTo::date AS valid_to,\n  t.from_age,\n  t.to_age,\n  t.min_birthdate,\n  t.max_birthdate,\n  t.allowed_to_dance_in,\n  t.divisions\nFROM unnest(\n  :name::text[],\n  :fromAge::int[],\n  :toAge::int[],\n  :minBirthdate::date[],\n  :maxBirthdate::date[],\n  :allowedToDanceIn::text[][],\n  :division::text[][]\n) AS t(name, from_age, to_age, min_birthdate, max_birthdate, allowed_to_dance_in, divisions)\nON CONFLICT (federation, name, valid_from)\n  DO UPDATE SET\n    valid_to            = EXCLUDED.valid_to,\n    from_age            = EXCLUDED.from_age,\n    to_age              = EXCLUDED.to_age,\n    min_birthdate       = EXCLUDED.min_birthdate,\n    max_birthdate       = EXCLUDED.max_birthdate,\n    allowed_to_dance_in = EXCLUDED.allowed_to_dance_in,\n    divisions           = EXCLUDED.divisions"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO federated.federation_age_group_rule (
 *   federation,
 *   name,
 *   valid_from,
 *   valid_to,
 *   from_age,
 *   to_age,
 *   min_birthdate,
 *   max_birthdate,
 *   allowed_to_dance_in,
 *   divisions
 * )
 * SELECT
 *   :federation::text AS federation,
 *   t.name,
 *   :validFrom::date AS valid_from,
 *   :validTo::date AS valid_to,
 *   t.from_age,
 *   t.to_age,
 *   t.min_birthdate,
 *   t.max_birthdate,
 *   t.allowed_to_dance_in,
 *   t.divisions
 * FROM unnest(
 *   :name::text[],
 *   :fromAge::int[],
 *   :toAge::int[],
 *   :minBirthdate::date[],
 *   :maxBirthdate::date[],
 *   :allowedToDanceIn::text[][],
 *   :division::text[][]
 * ) AS t(name, from_age, to_age, min_birthdate, max_birthdate, allowed_to_dance_in, divisions)
 * ON CONFLICT (federation, name, valid_from)
 *   DO UPDATE SET
 *     valid_to            = EXCLUDED.valid_to,
 *     from_age            = EXCLUDED.from_age,
 *     to_age              = EXCLUDED.to_age,
 *     min_birthdate       = EXCLUDED.min_birthdate,
 *     max_birthdate       = EXCLUDED.max_birthdate,
 *     allowed_to_dance_in = EXCLUDED.allowed_to_dance_in,
 *     divisions           = EXCLUDED.divisions
 * ```
 */
export const upsertAgeGroupRules = new PreparedQuery<IUpsertAgeGroupRulesParams,IUpsertAgeGroupRulesResult>(upsertAgeGroupRulesIR);


/** 'UpsertRoundsAndRoundDances' parameters type */
export interface IUpsertRoundsAndRoundDancesParams {
  competitionId?: NumberOrString | null | void;
  danceCodes?: stringArray | null | void;
  danceProgramIds?: NumberOrStringArray | null | void;
  roundIndexes?: numberArray | null | void;
  roundKeys?: stringArray | null | void;
  roundLabels?: stringArray | null | void;
  scoringMethods?: scoring_methodArray | null | void;
}

/** 'UpsertRoundsAndRoundDances' return type */
export interface IUpsertRoundsAndRoundDancesResult {
  id: string;
  round_key: string;
}

/** 'UpsertRoundsAndRoundDances' query type */
export interface IUpsertRoundsAndRoundDancesQuery {
  params: IUpsertRoundsAndRoundDancesParams;
  result: IUpsertRoundsAndRoundDancesResult;
}

const upsertRoundsAndRoundDancesIR: any = {"usedParamSet":{"competitionId":true,"roundKeys":true,"roundLabels":true,"roundIndexes":true,"danceProgramIds":true,"scoringMethods":true,"danceCodes":true},"params":[{"name":"competitionId","required":false,"transform":{"type":"scalar"},"locs":[{"a":170,"b":183}]},{"name":"roundKeys","required":false,"transform":{"type":"scalar"},"locs":[{"a":315,"b":324},{"a":959,"b":968}]},{"name":"roundLabels","required":false,"transform":{"type":"scalar"},"locs":[{"a":339,"b":350}]},{"name":"roundIndexes","required":false,"transform":{"type":"scalar"},"locs":[{"a":365,"b":377}]},{"name":"danceProgramIds","required":false,"transform":{"type":"scalar"},"locs":[{"a":391,"b":406}]},{"name":"scoringMethods","required":false,"transform":{"type":"scalar"},"locs":[{"a":423,"b":437}]},{"name":"danceCodes","required":false,"transform":{"type":"scalar"},"locs":[{"a":983,"b":993}]}],"statement":"WITH upserted AS (\n  INSERT INTO federated.competition_round (\n    competition_id, round_key, round_label, round_index, dance_program_id, scoring_method\n  )\n  SELECT\n    :competitionId::bigint,\n    round_key,\n    nullif(round_label, ''),\n    round_index,\n    dance_program_id,\n    scoring_method\n  FROM unnest(\n    :roundKeys::text[],\n    :roundLabels::text[],\n    :roundIndexes::int[],\n    :danceProgramIds::bigint[],\n    :scoringMethods::federated.scoring_method[]\n  ) AS rk(round_key, round_label, round_index, dance_program_id, scoring_method)\n  ON CONFLICT (competition_id, round_key)\n    DO UPDATE SET\n      round_label      = EXCLUDED.round_label,\n      round_index      = EXCLUDED.round_index,\n      dance_program_id = EXCLUDED.dance_program_id,\n      scoring_method   = EXCLUDED.scoring_method\n  RETURNING id, round_key\n),\nins_round_dance AS (\n  INSERT INTO federated.round_dance (round_id, dance_code)\n  SELECT u.id, p.dance_code\n  FROM unnest(\n    :roundKeys::text[],\n    :danceCodes::text[]\n  ) AS p(round_key, dance_code)\n  JOIN upserted u USING (round_key)\n  ON CONFLICT (round_id, dance_code) DO NOTHING\n  RETURNING 1\n)\nSELECT id, round_key FROM upserted"};

/**
 * Query generated from SQL:
 * ```
 * WITH upserted AS (
 *   INSERT INTO federated.competition_round (
 *     competition_id, round_key, round_label, round_index, dance_program_id, scoring_method
 *   )
 *   SELECT
 *     :competitionId::bigint,
 *     round_key,
 *     nullif(round_label, ''),
 *     round_index,
 *     dance_program_id,
 *     scoring_method
 *   FROM unnest(
 *     :roundKeys::text[],
 *     :roundLabels::text[],
 *     :roundIndexes::int[],
 *     :danceProgramIds::bigint[],
 *     :scoringMethods::federated.scoring_method[]
 *   ) AS rk(round_key, round_label, round_index, dance_program_id, scoring_method)
 *   ON CONFLICT (competition_id, round_key)
 *     DO UPDATE SET
 *       round_label      = EXCLUDED.round_label,
 *       round_index      = EXCLUDED.round_index,
 *       dance_program_id = EXCLUDED.dance_program_id,
 *       scoring_method   = EXCLUDED.scoring_method
 *   RETURNING id, round_key
 * ),
 * ins_round_dance AS (
 *   INSERT INTO federated.round_dance (round_id, dance_code)
 *   SELECT u.id, p.dance_code
 *   FROM unnest(
 *     :roundKeys::text[],
 *     :danceCodes::text[]
 *   ) AS p(round_key, dance_code)
 *   JOIN upserted u USING (round_key)
 *   ON CONFLICT (round_id, dance_code) DO NOTHING
 *   RETURNING 1
 * )
 * SELECT id, round_key FROM upserted
 * ```
 */
export const upsertRoundsAndRoundDances = new PreparedQuery<IUpsertRoundsAndRoundDancesParams,IUpsertRoundsAndRoundDancesResult>(upsertRoundsAndRoundDancesIR);


