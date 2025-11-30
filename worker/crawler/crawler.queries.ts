/** Types generated for queries found in "crawler/crawler.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type fetch_status = 'error' | 'gone' | 'ok' | 'pending';

export type gender = 'female' | 'male' | 'other' | 'unknown';

export type process_status = 'error' | 'ok' | 'pending';

export type DateOrString = Date | string;

export type Json = null | boolean | number | string | Json[] | { [key: string]: Json };

export type NumberOrString = number | string;

export type stringArray = (string)[];

/** 'GetFrontierForUpdate' parameters type */
export interface IGetFrontierForUpdateParams {
  id?: NumberOrString | null | void;
}

/** 'GetFrontierForUpdate' return type */
export interface IGetFrontierForUpdateResult {
  error_count: number;
  federation: string;
  fetch_status: fetch_status;
  id: string;
  key: string;
  kind: string;
  meta: Json;
}

/** 'GetFrontierForUpdate' query type */
export interface IGetFrontierForUpdateQuery {
  params: IGetFrontierForUpdateParams;
  result: IGetFrontierForUpdateResult;
}

const getFrontierForUpdateIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":99,"b":101}]}],"statement":"SELECT id, federation, kind, key, fetch_status, error_count, meta\nFROM crawler.frontier\nWHERE id = :id::bigint FOR UPDATE"};

/**
 * Query generated from SQL:
 * ```
 * SELECT id, federation, kind, key, fetch_status, error_count, meta
 * FROM crawler.frontier
 * WHERE id = :id::bigint FOR UPDATE
 * ```
 */
export const getFrontierForUpdate = new PreparedQuery<IGetFrontierForUpdateParams,IGetFrontierForUpdateResult>(getFrontierForUpdateIR);


/** 'ReserveRequest' parameters type */
export interface IReserveRequestParams {
  host?: string | null | void;
  prefixes?: stringArray | null | void;
}

/** 'ReserveRequest' return type */
export interface IReserveRequestResult {
  allowed_at: Date | null;
  granted: boolean | null;
}

/** 'ReserveRequest' query type */
export interface IReserveRequestQuery {
  params: IReserveRequestParams;
  result: IReserveRequestResult;
}

const reserveRequestIR: any = {"usedParamSet":{"host":true,"prefixes":true},"params":[{"name":"host","required":false,"transform":{"type":"scalar"},"locs":[{"a":56,"b":60}]},{"name":"prefixes","required":false,"transform":{"type":"scalar"},"locs":[{"a":69,"b":77}]}],"statement":"SELECT granted, allowed_at\nFROM crawler.reserve_request(:host::text, :prefixes::text[])"};

/**
 * Query generated from SQL:
 * ```
 * SELECT granted, allowed_at
 * FROM crawler.reserve_request(:host::text, :prefixes::text[])
 * ```
 */
export const reserveRequest = new PreparedQuery<IReserveRequestParams,IReserveRequestResult>(reserveRequestIR);


/** 'MarkFrontierFetchError' parameters type */
export interface IMarkFrontierFetchErrorParams {
  id?: NumberOrString | null | void;
}

/** 'MarkFrontierFetchError' return type */
export type IMarkFrontierFetchErrorResult = void;

/** 'MarkFrontierFetchError' query type */
export interface IMarkFrontierFetchErrorQuery {
  params: IMarkFrontierFetchErrorParams;
  result: IMarkFrontierFetchErrorResult;
}

const markFrontierFetchErrorIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":239,"b":241}]}],"statement":"UPDATE crawler.frontier\nSET last_fetched_at = now(),\n    fetch_status    = 'error',\n    error_count     = error_count + 1,\n    next_fetch_at   = now() + least(interval '5 minutes', (2 ^ (error_count + 1)) * interval '5 second')\nWHERE id = :id::bigint"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier
 * SET last_fetched_at = now(),
 *     fetch_status    = 'error',
 *     error_count     = error_count + 1,
 *     next_fetch_at   = now() + least(interval '5 minutes', (2 ^ (error_count + 1)) * interval '5 second')
 * WHERE id = :id::bigint
 * ```
 */
export const markFrontierFetchError = new PreparedQuery<IMarkFrontierFetchErrorParams,IMarkFrontierFetchErrorResult>(markFrontierFetchErrorIR);


/** 'MarkFrontierFetchSuccess' parameters type */
export interface IMarkFrontierFetchSuccessParams {
  fetchStatus?: fetch_status | null | void;
  id?: NumberOrString | null | void;
  processStatus?: process_status | null | void;
  revalidatePeriod?: DateOrString | null | void;
}

/** 'MarkFrontierFetchSuccess' return type */
export type IMarkFrontierFetchSuccessResult = void;

/** 'MarkFrontierFetchSuccess' query type */
export interface IMarkFrontierFetchSuccessQuery {
  params: IMarkFrontierFetchSuccessParams;
  result: IMarkFrontierFetchSuccessResult;
}

const markFrontierFetchSuccessIR: any = {"usedParamSet":{"fetchStatus":true,"processStatus":true,"revalidatePeriod":true,"id":true},"params":[{"name":"fetchStatus","required":false,"transform":{"type":"scalar"},"locs":[{"a":72,"b":83}]},{"name":"processStatus","required":false,"transform":{"type":"scalar"},"locs":[{"a":107,"b":120}]},{"name":"revalidatePeriod","required":false,"transform":{"type":"scalar"},"locs":[{"a":172,"b":188}]},{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":211,"b":213}]}],"statement":"UPDATE crawler.frontier\nSET last_fetched_at = now(),\n    fetch_status = :fetchStatus,\n    process_status = :processStatus,\n    error_count = 0,\n    next_fetch_at = now() + :revalidatePeriod::interval\nWHERE id = :id::bigint"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier
 * SET last_fetched_at = now(),
 *     fetch_status = :fetchStatus,
 *     process_status = :processStatus,
 *     error_count = 0,
 *     next_fetch_at = now() + :revalidatePeriod::interval
 * WHERE id = :id::bigint
 * ```
 */
export const markFrontierFetchSuccess = new PreparedQuery<IMarkFrontierFetchSuccessParams,IMarkFrontierFetchSuccessResult>(markFrontierFetchSuccessIR);


/** 'RescheduleFrontier' parameters type */
export interface IRescheduleFrontierParams {
  id?: NumberOrString | null | void;
  nextRetryAt?: DateOrString | null | void;
}

/** 'RescheduleFrontier' return type */
export type IRescheduleFrontierResult = void;

/** 'RescheduleFrontier' query type */
export interface IRescheduleFrontierQuery {
  params: IRescheduleFrontierParams;
  result: IRescheduleFrontierResult;
}

const rescheduleFrontierIR: any = {"usedParamSet":{"nextRetryAt":true,"id":true},"params":[{"name":"nextRetryAt","required":false,"transform":{"type":"scalar"},"locs":[{"a":44,"b":55}]},{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":68,"b":70}]}],"statement":"UPDATE crawler.frontier\nSET next_fetch_at = :nextRetryAt\nWHERE id = :id::bigint"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE crawler.frontier
 * SET next_fetch_at = :nextRetryAt
 * WHERE id = :id::bigint
 * ```
 */
export const rescheduleFrontier = new PreparedQuery<IRescheduleFrontierParams,IRescheduleFrontierResult>(rescheduleFrontierIR);


/** 'InsertHtmlResponse' parameters type */
export interface IInsertHtmlResponseParams {
  content?: string | null | void;
  error?: string | null | void;
  frontierId?: NumberOrString | null | void;
  httpStatus?: number | null | void;
  url?: string | null | void;
}

/** 'InsertHtmlResponse' return type */
export type IInsertHtmlResponseResult = void;

/** 'InsertHtmlResponse' query type */
export interface IInsertHtmlResponseQuery {
  params: IInsertHtmlResponseParams;
  result: IInsertHtmlResponseResult;
}

const insertHtmlResponseIR: any = {"usedParamSet":{"frontierId":true,"url":true,"httpStatus":true,"error":true,"content":true},"params":[{"name":"frontierId","required":false,"transform":{"type":"scalar"},"locs":[{"a":90,"b":100}]},{"name":"url","required":false,"transform":{"type":"scalar"},"locs":[{"a":103,"b":106}]},{"name":"httpStatus","required":false,"transform":{"type":"scalar"},"locs":[{"a":109,"b":119}]},{"name":"error","required":false,"transform":{"type":"scalar"},"locs":[{"a":122,"b":127}]},{"name":"content","required":false,"transform":{"type":"scalar"},"locs":[{"a":130,"b":137}]}],"statement":"INSERT INTO crawler.html_response (frontier_id, url, http_status, error, content)\nVALUES (:frontierId, :url, :httpStatus, :error, :content)"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO crawler.html_response (frontier_id, url, http_status, error, content)
 * VALUES (:frontierId, :url, :httpStatus, :error, :content)
 * ```
 */
export const insertHtmlResponse = new PreparedQuery<IInsertHtmlResponseParams,IInsertHtmlResponseResult>(insertHtmlResponseIR);


/** 'InsertJsonResponse' parameters type */
export interface IInsertJsonResponseParams {
  content?: Json | null | void;
  error?: string | null | void;
  frontierId?: NumberOrString | null | void;
  httpStatus?: number | null | void;
  url?: string | null | void;
}

/** 'InsertJsonResponse' return type */
export type IInsertJsonResponseResult = void;

/** 'InsertJsonResponse' query type */
export interface IInsertJsonResponseQuery {
  params: IInsertJsonResponseParams;
  result: IInsertJsonResponseResult;
}

const insertJsonResponseIR: any = {"usedParamSet":{"frontierId":true,"url":true,"httpStatus":true,"error":true,"content":true},"params":[{"name":"frontierId","required":false,"transform":{"type":"scalar"},"locs":[{"a":90,"b":100}]},{"name":"url","required":false,"transform":{"type":"scalar"},"locs":[{"a":103,"b":106}]},{"name":"httpStatus","required":false,"transform":{"type":"scalar"},"locs":[{"a":109,"b":119}]},{"name":"error","required":false,"transform":{"type":"scalar"},"locs":[{"a":122,"b":127}]},{"name":"content","required":false,"transform":{"type":"scalar"},"locs":[{"a":130,"b":137}]}],"statement":"INSERT INTO crawler.json_response (frontier_id, url, http_status, error, content)\nVALUES (:frontierId, :url, :httpStatus, :error, :content::jsonb)"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO crawler.json_response (frontier_id, url, http_status, error, content)
 * VALUES (:frontierId, :url, :httpStatus, :error, :content::jsonb)
 * ```
 */
export const insertJsonResponse = new PreparedQuery<IInsertJsonResponseParams,IInsertJsonResponseResult>(insertJsonResponseIR);


/** 'InsertDiscoveredCstsMember' parameters type */
export interface IInsertDiscoveredCstsMemberParams {
  id?: string | null | void;
}

/** 'InsertDiscoveredCstsMember' return type */
export type IInsertDiscoveredCstsMemberResult = void;

/** 'InsertDiscoveredCstsMember' query type */
export interface IInsertDiscoveredCstsMemberQuery {
  params: IInsertDiscoveredCstsMemberParams;
  result: IInsertDiscoveredCstsMemberResult;
}

const insertDiscoveredCstsMemberIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":false,"transform":{"type":"scalar"},"locs":[{"a":103,"b":105}]}],"statement":"with frontier as (\n  INSERT INTO crawler.frontier (federation, kind, key)\n    VALUES ('csts','member', :id)\n    ON CONFLICT (federation, kind, key) DO NOTHING\n    RETURNING key\n)\nUPDATE crawler.incremental_ranges\nSET last_known = GREATEST(last_known, (SELECT key::bigint FROM frontier))\nFROM frontier\nWHERE federation = 'csts' AND kind = 'member_id'"};

/**
 * Query generated from SQL:
 * ```
 * with frontier as (
 *   INSERT INTO crawler.frontier (federation, kind, key)
 *     VALUES ('csts','member', :id)
 *     ON CONFLICT (federation, kind, key) DO NOTHING
 *     RETURNING key
 * )
 * UPDATE crawler.incremental_ranges
 * SET last_known = GREATEST(last_known, (SELECT key::bigint FROM frontier))
 * FROM frontier
 * WHERE federation = 'csts' AND kind = 'member_id'
 * ```
 */
export const insertDiscoveredCstsMember = new PreparedQuery<IInsertDiscoveredCstsMemberParams,IInsertDiscoveredCstsMemberResult>(insertDiscoveredCstsMemberIR);


/** 'UpsertFederationAthlete' parameters type */
export interface IUpsertFederationAthleteParams {
  canonicalName?: string | null | void;
  externalId?: string | null | void;
  federation?: string | null | void;
  gender?: gender | null | void;
}

/** 'UpsertFederationAthlete' return type */
export type IUpsertFederationAthleteResult = void;

/** 'UpsertFederationAthlete' query type */
export interface IUpsertFederationAthleteQuery {
  params: IUpsertFederationAthleteParams;
  result: IUpsertFederationAthleteResult;
}

const upsertFederationAthleteIR: any = {"usedParamSet":{"federation":true,"externalId":true,"canonicalName":true,"gender":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":105,"b":115},{"a":732,"b":742}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":142,"b":152},{"a":745,"b":755}]},{"name":"canonicalName","required":false,"transform":{"type":"scalar"},"locs":[{"a":259,"b":272}]},{"name":"gender","required":false,"transform":{"type":"scalar"},"locs":[{"a":275,"b":281}]}],"statement":"WITH existing AS (\n  SELECT fa.athlete_id\n  FROM federated.federation_athlete fa\n  WHERE fa.federation = :federation\n    AND fa.external_id = :externalId::text\n  FOR UPDATE\n), person_ins AS (\n  INSERT INTO federated.person (canonical_name, gender)\n    SELECT :canonicalName, :gender::federated.gender\n    WHERE NOT EXISTS (SELECT 1 FROM existing)\n    RETURNING id\n), athlete_ins AS (\n  INSERT INTO federated.athlete (person_id)\n    SELECT id\n    FROM person_ins\n    WHERE NOT EXISTS (SELECT 1 FROM existing)\n    RETURNING id AS athlete_id\n), athlete_final AS (\n  SELECT athlete_id FROM existing\n  UNION ALL\n  SELECT athlete_id FROM athlete_ins\n)\nINSERT INTO federated.federation_athlete (federation, external_id, athlete_id)\nSELECT :federation, :externalId, athlete_id\nFROM athlete_final\nON CONFLICT (federation, external_id)\n  DO UPDATE SET athlete_id = EXCLUDED.athlete_id"};

/**
 * Query generated from SQL:
 * ```
 * WITH existing AS (
 *   SELECT fa.athlete_id
 *   FROM federated.federation_athlete fa
 *   WHERE fa.federation = :federation
 *     AND fa.external_id = :externalId::text
 *   FOR UPDATE
 * ), person_ins AS (
 *   INSERT INTO federated.person (canonical_name, gender)
 *     SELECT :canonicalName, :gender::federated.gender
 *     WHERE NOT EXISTS (SELECT 1 FROM existing)
 *     RETURNING id
 * ), athlete_ins AS (
 *   INSERT INTO federated.athlete (person_id)
 *     SELECT id
 *     FROM person_ins
 *     WHERE NOT EXISTS (SELECT 1 FROM existing)
 *     RETURNING id AS athlete_id
 * ), athlete_final AS (
 *   SELECT athlete_id FROM existing
 *   UNION ALL
 *   SELECT athlete_id FROM athlete_ins
 * )
 * INSERT INTO federated.federation_athlete (federation, external_id, athlete_id)
 * SELECT :federation, :externalId, athlete_id
 * FROM athlete_final
 * ON CONFLICT (federation, external_id)
 *   DO UPDATE SET athlete_id = EXCLUDED.athlete_id
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
  id: string;
}

/** 'UpsertCategory' query type */
export interface IUpsertCategoryQuery {
  params: IUpsertCategoryParams;
  result: IUpsertCategoryResult;
}

const upsertCategoryIR: any = {"usedParamSet":{"series":true,"discipline":true,"ageGroup":true,"genderGroup":true,"class":true},"params":[{"name":"series","required":false,"transform":{"type":"scalar"},"locs":[{"a":115,"b":121},{"a":180,"b":186}]},{"name":"discipline","required":false,"transform":{"type":"scalar"},"locs":[{"a":126,"b":136},{"a":198,"b":208}]},{"name":"ageGroup","required":false,"transform":{"type":"scalar"},"locs":[{"a":141,"b":149},{"a":220,"b":228}]},{"name":"genderGroup","required":false,"transform":{"type":"scalar"},"locs":[{"a":154,"b":165}]},{"name":"class","required":false,"transform":{"type":"scalar"},"locs":[{"a":170,"b":175},{"a":240,"b":245}]}],"statement":"INSERT INTO federated.category (\n  series,\n  discipline,\n  age_group,\n  gender_group,\n  class,\n  name\n)\nVALUES (\n  :series,\n  :discipline,\n  :ageGroup,\n  :genderGroup,\n  :class,\n  :series || ' ' || :discipline || ' ' || :ageGroup || ' ' || :class\n)\nON CONFLICT (series, discipline, age_group, gender_group, class)\n  DO UPDATE SET name = EXCLUDED.name\nRETURNING id"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO federated.category (
 *   series,
 *   discipline,
 *   age_group,
 *   gender_group,
 *   class,
 *   name
 * )
 * VALUES (
 *   :series,
 *   :discipline,
 *   :ageGroup,
 *   :genderGroup,
 *   :class,
 *   :series || ' ' || :discipline || ' ' || :ageGroup || ' ' || :class
 * )
 * ON CONFLICT (series, discipline, age_group, gender_group, class)
 *   DO UPDATE SET name = EXCLUDED.name
 * RETURNING id
 * ```
 */
export const upsertCategory = new PreparedQuery<IUpsertCategoryParams,IUpsertCategoryResult>(upsertCategoryIR);


/** 'UpsertFederationCouple' parameters type */
export interface IUpsertFederationCoupleParams {
  competitorLabel?: string | null | void;
  externalCompetitorId?: string | null | void;
  externalFollowerId?: NumberOrString | null | void;
  externalLeadId?: NumberOrString | null | void;
  federation?: string | null | void;
}

/** 'UpsertFederationCouple' return type */
export interface IUpsertFederationCoupleResult {
  competitor_id: string | null;
}

/** 'UpsertFederationCouple' query type */
export interface IUpsertFederationCoupleQuery {
  params: IUpsertFederationCoupleParams;
  result: IUpsertFederationCoupleResult;
}

const upsertFederationCoupleIR: any = {"usedParamSet":{"federation":true,"externalCompetitorId":true,"competitorLabel":true,"externalLeadId":true,"externalFollowerId":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":105,"b":115},{"a":280,"b":290},{"a":1220,"b":1230}]},{"name":"externalCompetitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":139,"b":159},{"a":293,"b":313},{"a":1258,"b":1278}]},{"name":"competitorLabel","required":false,"transform":{"type":"scalar"},"locs":[{"a":600,"b":615},{"a":767,"b":782}]},{"name":"externalLeadId","required":false,"transform":{"type":"scalar"},"locs":[{"a":1513,"b":1527}]},{"name":"externalFollowerId","required":false,"transform":{"type":"scalar"},"locs":[{"a":1811,"b":1829}]}],"statement":"WITH fc_existing AS (\n  SELECT competitor_id\n  FROM federated.federation_competitor\n  WHERE federation = :federation\n    AND external_id = :externalCompetitorId::text\n    FOR UPDATE\n), fc_ins AS (\n  INSERT INTO federated.federation_competitor (federation, external_id)\n    SELECT :federation, :externalCompetitorId::text\n    WHERE NOT EXISTS (SELECT 1 FROM fc_existing)\n    RETURNING competitor_id\n), comp_seed AS (\n  SELECT competitor_id FROM fc_existing\n  UNION ALL\n  SELECT competitor_id FROM fc_ins\n), comp_ins AS (\n  INSERT INTO federated.competitor (competitor_type, name)\n    SELECT 'couple', :competitorLabel\n    FROM comp_seed\n    WHERE competitor_id IS NULL\n    RETURNING id AS competitor_id\n), comp_update AS (\n  UPDATE federated.competitor\n    SET name = :competitorLabel\n    FROM comp_seed\n    WHERE federated.competitor.id = comp_seed.competitor_id\n      AND comp_seed.competitor_id IS NOT NULL\n    RETURNING federated.competitor.id AS competitor_id\n), comp_final AS (\n  SELECT competitor_id FROM comp_update\n  UNION ALL\n  SELECT competitor_id FROM comp_ins\n), comp_link AS (\n  UPDATE federated.federation_competitor f\n    SET competitor_id = c.competitor_id\n    FROM comp_final c\n    WHERE f.federation = :federation\n      AND f.external_id = :externalCompetitorId::text\n    RETURNING c.competitor_id\n), ids AS (\n  SELECT competitor_id FROM comp_link LIMIT 1\n), comp_lead AS (\n  INSERT INTO federated.competitor_component (competitor_id, athlete_id, role)\n    SELECT\n      ids.competitor_id,\n      :externalLeadId::bigint,\n      'lead'::federated.competitor_role\n    FROM ids\n    ON CONFLICT (competitor_id, athlete_id) DO UPDATE SET role = EXCLUDED.role\n), comp_follow AS (\n  INSERT INTO federated.competitor_component (competitor_id, athlete_id, role)\n    SELECT\n      ids.competitor_id,\n      :externalFollowerId::bigint,\n      'follow'::federated.competitor_role\n    FROM ids\n    ON CONFLICT (competitor_id, athlete_id) DO UPDATE SET role = EXCLUDED.role\n)\nSELECT competitor_id FROM ids"};

/**
 * Query generated from SQL:
 * ```
 * WITH fc_existing AS (
 *   SELECT competitor_id
 *   FROM federated.federation_competitor
 *   WHERE federation = :federation
 *     AND external_id = :externalCompetitorId::text
 *     FOR UPDATE
 * ), fc_ins AS (
 *   INSERT INTO federated.federation_competitor (federation, external_id)
 *     SELECT :federation, :externalCompetitorId::text
 *     WHERE NOT EXISTS (SELECT 1 FROM fc_existing)
 *     RETURNING competitor_id
 * ), comp_seed AS (
 *   SELECT competitor_id FROM fc_existing
 *   UNION ALL
 *   SELECT competitor_id FROM fc_ins
 * ), comp_ins AS (
 *   INSERT INTO federated.competitor (competitor_type, name)
 *     SELECT 'couple', :competitorLabel
 *     FROM comp_seed
 *     WHERE competitor_id IS NULL
 *     RETURNING id AS competitor_id
 * ), comp_update AS (
 *   UPDATE federated.competitor
 *     SET name = :competitorLabel
 *     FROM comp_seed
 *     WHERE federated.competitor.id = comp_seed.competitor_id
 *       AND comp_seed.competitor_id IS NOT NULL
 *     RETURNING federated.competitor.id AS competitor_id
 * ), comp_final AS (
 *   SELECT competitor_id FROM comp_update
 *   UNION ALL
 *   SELECT competitor_id FROM comp_ins
 * ), comp_link AS (
 *   UPDATE federated.federation_competitor f
 *     SET competitor_id = c.competitor_id
 *     FROM comp_final c
 *     WHERE f.federation = :federation
 *       AND f.external_id = :externalCompetitorId::text
 *     RETURNING c.competitor_id
 * ), ids AS (
 *   SELECT competitor_id FROM comp_link LIMIT 1
 * ), comp_lead AS (
 *   INSERT INTO federated.competitor_component (competitor_id, athlete_id, role)
 *     SELECT
 *       ids.competitor_id,
 *       :externalLeadId::bigint,
 *       'lead'::federated.competitor_role
 *     FROM ids
 *     ON CONFLICT (competitor_id, athlete_id) DO UPDATE SET role = EXCLUDED.role
 * ), comp_follow AS (
 *   INSERT INTO federated.competitor_component (competitor_id, athlete_id, role)
 *     SELECT
 *       ids.competitor_id,
 *       :externalFollowerId::bigint,
 *       'follow'::federated.competitor_role
 *     FROM ids
 *     ON CONFLICT (competitor_id, athlete_id) DO UPDATE SET role = EXCLUDED.role
 * )
 * SELECT competitor_id FROM ids
 * ```
 */
export const upsertFederationCouple = new PreparedQuery<IUpsertFederationCoupleParams,IUpsertFederationCoupleResult>(upsertFederationCoupleIR);


/** 'UpsertFederationCoupleProgress' parameters type */
export interface IUpsertFederationCoupleProgressParams {
  categoryId?: NumberOrString | null | void;
  domesticFinale?: number | null | void;
  federatedCompetitorId?: NumberOrString | null | void;
  federation?: string | null | void;
  foreignFinale?: number | null | void;
  points?: NumberOrString | null | void;
}

/** 'UpsertFederationCoupleProgress' return type */
export type IUpsertFederationCoupleProgressResult = void;

/** 'UpsertFederationCoupleProgress' query type */
export interface IUpsertFederationCoupleProgressQuery {
  params: IUpsertFederationCoupleProgressParams;
  result: IUpsertFederationCoupleProgressResult;
}

const upsertFederationCoupleProgressIR: any = {"usedParamSet":{"federation":true,"federatedCompetitorId":true,"categoryId":true,"points":true,"domesticFinale":true,"foreignFinale":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":158,"b":168}]},{"name":"federatedCompetitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":173,"b":194}]},{"name":"categoryId","required":false,"transform":{"type":"scalar"},"locs":[{"a":199,"b":209}]},{"name":"points","required":false,"transform":{"type":"scalar"},"locs":[{"a":214,"b":220}]},{"name":"domesticFinale","required":false,"transform":{"type":"scalar"},"locs":[{"a":241,"b":255}]},{"name":"foreignFinale","required":false,"transform":{"type":"scalar"},"locs":[{"a":265,"b":278}]}],"statement":"INSERT INTO federated.competitor_category_progress (\n  federation,\n  competitor_id,\n  category_id,\n  points,\n  domestic_finale,\n  foreign_finale\n)\nVALUES (\n  :federation,\n  :federatedCompetitorId,\n  :categoryId,\n  :points::numeric(10, 3),\n  :domesticFinale::int,\n  :foreignFinale::int\n)\nON CONFLICT (federation, competitor_id, category_id)\n  DO UPDATE\n  SET points          = EXCLUDED.points,\n      domestic_finale = EXCLUDED.domestic_finale,\n      foreign_finale  = EXCLUDED.foreign_finale"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO federated.competitor_category_progress (
 *   federation,
 *   competitor_id,
 *   category_id,
 *   points,
 *   domestic_finale,
 *   foreign_finale
 * )
 * VALUES (
 *   :federation,
 *   :federatedCompetitorId,
 *   :categoryId,
 *   :points::numeric(10, 3),
 *   :domesticFinale::int,
 *   :foreignFinale::int
 * )
 * ON CONFLICT (federation, competitor_id, category_id)
 *   DO UPDATE
 *   SET points          = EXCLUDED.points,
 *       domestic_finale = EXCLUDED.domestic_finale,
 *       foreign_finale  = EXCLUDED.foreign_finale
 * ```
 */
export const upsertFederationCoupleProgress = new PreparedQuery<IUpsertFederationCoupleProgressParams,IUpsertFederationCoupleProgressResult>(upsertFederationCoupleProgressIR);


