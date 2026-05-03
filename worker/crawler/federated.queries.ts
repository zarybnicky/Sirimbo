/** Types generated for queries found in "crawler/federated.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type competitor_role = 'follow' | 'lead' | 'member' | 'substitute';

export type competitor_type = 'couple' | 'duo' | 'formation' | 'solo' | 'team' | 'trio';

export type gender = 'female' | 'male' | 'other' | 'unknown';

export type DateOrString = Date | string;

export type Json = null | boolean | number | string | Json[] | { [key: string]: Json };

export type NumberOrString = number | string;

export type NumberOrStringArray = (NumberOrString)[];

export type competitor_roleArray = (competitor_role)[];

export type competitor_typeArray = (competitor_type)[];

export type genderArray = (gender)[];

export type numberArray = (number)[];

export type stringArray = (string)[];

/** 'UpsertPeople' parameters type */
export interface IUpsertPeopleParams {
  canonicalName?: stringArray | null | void;
  externalId?: stringArray | null | void;
  federation?: stringArray | null | void;
  gender?: genderArray | null | void;
}

/** 'UpsertPeople' return type */
export type IUpsertPeopleResult = void;

/** 'UpsertPeople' query type */
export interface IUpsertPeopleQuery {
  params: IUpsertPeopleParams;
  result: IUpsertPeopleResult;
}

const upsertPeopleIR: any = {"usedParamSet":{"federation":true,"externalId":true,"canonicalName":true,"gender":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":173,"b":183}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":196,"b":206}]},{"name":"canonicalName","required":false,"transform":{"type":"scalar"},"locs":[{"a":219,"b":232}]},{"name":"gender","required":false,"transform":{"type":"scalar"},"locs":[{"a":245,"b":251}]}],"statement":"INSERT INTO federated.person (federation, external_id, canonical_name, gender)\nSELECT input.federation, input.external_id, input.canonical_name, input.gender\nFROM unnest(\n  :federation::text[],\n  :externalId::text[],\n  :canonicalName::text[],\n  :gender::federated.gender[]\n) AS input(federation, external_id, canonical_name, gender)\nON CONFLICT (federation, external_id)\n  DO UPDATE SET canonical_name = EXCLUDED.canonical_name\n  WHERE federated.person.canonical_name IS DISTINCT FROM EXCLUDED.canonical_name AND EXCLUDED.canonical_name <> ''"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO federated.person (federation, external_id, canonical_name, gender)
 * SELECT input.federation, input.external_id, input.canonical_name, input.gender
 * FROM unnest(
 *   :federation::text[],
 *   :externalId::text[],
 *   :canonicalName::text[],
 *   :gender::federated.gender[]
 * ) AS input(federation, external_id, canonical_name, gender)
 * ON CONFLICT (federation, external_id)
 *   DO UPDATE SET canonical_name = EXCLUDED.canonical_name
 *   WHERE federated.person.canonical_name IS DISTINCT FROM EXCLUDED.canonical_name AND EXCLUDED.canonical_name <> ''
 * ```
 */
export const upsertPeople = new PreparedQuery<IUpsertPeopleParams,IUpsertPeopleResult>(upsertPeopleIR);


/** 'UpdatePerson' parameters type */
export interface IUpdatePersonParams {
  ageGroup?: string | null | void;
  externalId?: string | null | void;
  federation?: string | null | void;
  medicalCheckupExpiration?: DateOrString | null | void;
}

/** 'UpdatePerson' return type */
export type IUpdatePersonResult = void;

/** 'UpdatePerson' query type */
export interface IUpdatePersonQuery {
  params: IUpdatePersonParams;
  result: IUpdatePersonResult;
}

const updatePersonIR: any = {"usedParamSet":{"ageGroup":true,"medicalCheckupExpiration":true,"federation":true,"externalId":true},"params":[{"name":"ageGroup","required":false,"transform":{"type":"scalar"},"locs":[{"a":40,"b":48}]},{"name":"medicalCheckupExpiration","required":false,"transform":{"type":"scalar"},"locs":[{"a":84,"b":108}]},{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":135,"b":145}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":167,"b":177}]}],"statement":"UPDATE federated.person\nSET age_group = :ageGroup,\n    medical_checkup_expiration = :medicalCheckupExpiration::date\nWHERE federation = :federation\n  AND external_id = :externalId"};

/**
 * Query generated from SQL:
 * ```
 * UPDATE federated.person
 * SET age_group = :ageGroup,
 *     medical_checkup_expiration = :medicalCheckupExpiration::date
 * WHERE federation = :federation
 *   AND external_id = :externalId
 * ```
 */
export const updatePerson = new PreparedQuery<IUpdatePersonParams,IUpdatePersonResult>(updatePersonIR);


/** 'GetAllCategories' parameters type */
export type IGetAllCategoriesParams = void;

/** 'GetAllCategories' return type */
export interface IGetAllCategoriesResult {
  ageGroup: string;
  class: string;
  competitorType: competitor_type;
  discipline: string;
  genderGroup: string;
  id: string;
  series: string;
}

/** 'GetAllCategories' query type */
export interface IGetAllCategoriesQuery {
  params: IGetAllCategoriesParams;
  result: IGetAllCategoriesResult;
}

const getAllCategoriesIR: any = {"usedParamSet":{},"params":[],"statement":"SELECT id, series, discipline, age_group AS \"ageGroup\",\n       gender_group AS \"genderGroup\", class, competitor_type AS \"competitorType\"\nFROM federated.category"};

/**
 * Query generated from SQL:
 * ```
 * SELECT id, series, discipline, age_group AS "ageGroup",
 *        gender_group AS "genderGroup", class, competitor_type AS "competitorType"
 * FROM federated.category
 * ```
 */
export const getAllCategories = new PreparedQuery<IGetAllCategoriesParams,IGetAllCategoriesResult>(getAllCategoriesIR);


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

const upsertCategoryIR: any = {"usedParamSet":{"series":true,"discipline":true,"ageGroup":true,"genderGroup":true,"class":true,"competitorType":true},"params":[{"name":"series","required":false,"transform":{"type":"scalar"},"locs":[{"a":101,"b":107}]},{"name":"discipline","required":false,"transform":{"type":"scalar"},"locs":[{"a":114,"b":124}]},{"name":"ageGroup","required":false,"transform":{"type":"scalar"},"locs":[{"a":131,"b":139}]},{"name":"genderGroup","required":false,"transform":{"type":"scalar"},"locs":[{"a":146,"b":157}]},{"name":"class","required":false,"transform":{"type":"scalar"},"locs":[{"a":164,"b":169}]},{"name":"competitorType","required":false,"transform":{"type":"scalar"},"locs":[{"a":176,"b":190}]}],"statement":"WITH input (series, discipline, age_group, gender_group, class, competitor_type) AS (\n  VALUES (\n    :series,\n    :discipline,\n    :ageGroup,\n    :genderGroup,\n    :class,\n    :competitorType::federated.competitor_type\n  )\n),\nins AS (\n  INSERT INTO federated.category (series, discipline, age_group, gender_group, class, competitor_type, name)\n  SELECT *, concat_ws(' ', series, age_group, nullif(competitor_type, 'couple'), nullif(class, ''), discipline)\n  FROM input\n  ON CONFLICT (series, discipline, age_group, gender_group, class, competitor_type) DO NOTHING\n  RETURNING id\n)\nSELECT id FROM ins\nUNION ALL\nSELECT c.id\nFROM federated.category c\nJOIN input i USING (series, discipline, age_group, gender_group, class, competitor_type)\nWHERE NOT EXISTS (SELECT 1 FROM ins)"};

/**
 * Query generated from SQL:
 * ```
 * WITH input (series, discipline, age_group, gender_group, class, competitor_type) AS (
 *   VALUES (
 *     :series,
 *     :discipline,
 *     :ageGroup,
 *     :genderGroup,
 *     :class,
 *     :competitorType::federated.competitor_type
 *   )
 * ),
 * ins AS (
 *   INSERT INTO federated.category (series, discipline, age_group, gender_group, class, competitor_type, name)
 *   SELECT *, concat_ws(' ', series, age_group, nullif(competitor_type, 'couple'), nullif(class, ''), discipline)
 *   FROM input
 *   ON CONFLICT (series, discipline, age_group, gender_group, class, competitor_type) DO NOTHING
 *   RETURNING id
 * )
 * SELECT id FROM ins
 * UNION ALL
 * SELECT c.id
 * FROM federated.category c
 * JOIN input i USING (series, discipline, age_group, gender_group, class, competitor_type)
 * WHERE NOT EXISTS (SELECT 1 FROM ins)
 * ```
 */
export const upsertCategory = new PreparedQuery<IUpsertCategoryParams,IUpsertCategoryResult>(upsertCategoryIR);


/** 'UpsertCompetitors' parameters type */
export interface IUpsertCompetitorsParams {
  externalId?: stringArray | null | void;
  federation?: stringArray | null | void;
  label?: stringArray | null | void;
  type?: competitor_typeArray | null | void;
}

/** 'UpsertCompetitors' return type */
export type IUpsertCompetitorsResult = void;

/** 'UpsertCompetitors' query type */
export interface IUpsertCompetitorsQuery {
  params: IUpsertCompetitorsParams;
  result: IUpsertCompetitorsResult;
}

const upsertCompetitorsIR: any = {"usedParamSet":{"federation":true,"externalId":true,"type":true,"label":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":175,"b":185}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":198,"b":208}]},{"name":"type","required":false,"transform":{"type":"scalar"},"locs":[{"a":221,"b":225}]},{"name":"label","required":false,"transform":{"type":"scalar"},"locs":[{"a":259,"b":264}]}],"statement":"INSERT INTO federated.competitor (federation, external_id, competitor_type, name)\nSELECT input.federation, input.external_id, input.competitor_type, input.name\nFROM unnest(\n  :federation::text[],\n  :externalId::text[],\n  :type::federated.competitor_type[],\n  :label::text[]\n) AS input(federation, external_id, competitor_type, name)\nON CONFLICT (federation, external_id) DO UPDATE\n  SET name = EXCLUDED.name\n  WHERE federated.competitor.name IS DISTINCT FROM EXCLUDED.name"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO federated.competitor (federation, external_id, competitor_type, name)
 * SELECT input.federation, input.external_id, input.competitor_type, input.name
 * FROM unnest(
 *   :federation::text[],
 *   :externalId::text[],
 *   :type::federated.competitor_type[],
 *   :label::text[]
 * ) AS input(federation, external_id, competitor_type, name)
 * ON CONFLICT (federation, external_id) DO UPDATE
 *   SET name = EXCLUDED.name
 *   WHERE federated.competitor.name IS DISTINCT FROM EXCLUDED.name
 * ```
 */
export const upsertCompetitors = new PreparedQuery<IUpsertCompetitorsParams,IUpsertCompetitorsResult>(upsertCompetitorsIR);


/** 'MergeCompetitorComponents' parameters type */
export interface IMergeCompetitorComponentsParams {
  competitorId?: stringArray | null | void;
  competitorIds?: stringArray | null | void;
  personId?: stringArray | null | void;
  role?: competitor_roleArray | null | void;
}

/** 'MergeCompetitorComponents' return type */
export type IMergeCompetitorComponentsResult = void;

/** 'MergeCompetitorComponents' query type */
export interface IMergeCompetitorComponentsQuery {
  params: IMergeCompetitorComponentsParams;
  result: IMergeCompetitorComponentsResult;
}

const mergeCompetitorComponentsIR: any = {"usedParamSet":{"competitorId":true,"personId":true,"role":true,"competitorIds":true},"params":[{"name":"competitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":165,"b":177}]},{"name":"personId","required":false,"transform":{"type":"scalar"},"locs":[{"a":192,"b":200}]},{"name":"role","required":false,"transform":{"type":"scalar"},"locs":[{"a":215,"b":219}]},{"name":"competitorIds","required":false,"transform":{"type":"scalar"},"locs":[{"a":628,"b":641}]}],"statement":"MERGE INTO federated.competitor_component AS t\nUSING (\n  SELECT DISTINCT ON (competitor_id, person_id)\n    competitor_id,\n    person_id,\n    role\n  FROM unnest(\n    :competitorId::text[],\n    :personId::text[],\n    :role::federated.competitor_role[]\n  ) AS input(competitor_id, person_id, role)\n) AS s\nON t.competitor_id = s.competitor_id AND t.person_id = s.person_id\nWHEN MATCHED AND t.role IS DISTINCT FROM s.role THEN\n  UPDATE SET role = s.role\nWHEN NOT MATCHED THEN\n  INSERT (competitor_id, person_id, role)\n  VALUES (s.competitor_id, s.person_id, s.role)\nWHEN NOT MATCHED BY SOURCE\n  AND t.competitor_id IN (SELECT unnest(:competitorIds::text[])) THEN\n  DELETE"};

/**
 * Query generated from SQL:
 * ```
 * MERGE INTO federated.competitor_component AS t
 * USING (
 *   SELECT DISTINCT ON (competitor_id, person_id)
 *     competitor_id,
 *     person_id,
 *     role
 *   FROM unnest(
 *     :competitorId::text[],
 *     :personId::text[],
 *     :role::federated.competitor_role[]
 *   ) AS input(competitor_id, person_id, role)
 * ) AS s
 * ON t.competitor_id = s.competitor_id AND t.person_id = s.person_id
 * WHEN MATCHED AND t.role IS DISTINCT FROM s.role THEN
 *   UPDATE SET role = s.role
 * WHEN NOT MATCHED THEN
 *   INSERT (competitor_id, person_id, role)
 *   VALUES (s.competitor_id, s.person_id, s.role)
 * WHEN NOT MATCHED BY SOURCE
 *   AND t.competitor_id IN (SELECT unnest(:competitorIds::text[])) THEN
 *   DELETE
 * ```
 */
export const mergeCompetitorComponents = new PreparedQuery<IMergeCompetitorComponentsParams,IMergeCompetitorComponentsResult>(mergeCompetitorComponentsIR);


/** 'MergeCompetitorProgress' parameters type */
export interface IMergeCompetitorProgressParams {
  categoryId?: NumberOrStringArray | null | void;
  competitorId?: stringArray | null | void;
  domesticFinals?: numberArray | null | void;
  foreignFinals?: numberArray | null | void;
  points?: NumberOrStringArray | null | void;
}

/** 'MergeCompetitorProgress' return type */
export type IMergeCompetitorProgressResult = void;

/** 'MergeCompetitorProgress' query type */
export interface IMergeCompetitorProgressQuery {
  params: IMergeCompetitorProgressParams;
  result: IMergeCompetitorProgressResult;
}

const mergeCompetitorProgressIR: any = {"usedParamSet":{"competitorId":true,"categoryId":true,"points":true,"domesticFinals":true,"foreignFinals":true},"params":[{"name":"competitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":209,"b":221},{"a":879,"b":891}]},{"name":"categoryId","required":false,"transform":{"type":"scalar"},"locs":[{"a":236,"b":246}]},{"name":"points","required":false,"transform":{"type":"scalar"},"locs":[{"a":263,"b":269}]},{"name":"domesticFinals","required":false,"transform":{"type":"scalar"},"locs":[{"a":293,"b":307}]},{"name":"foreignFinals","required":false,"transform":{"type":"scalar"},"locs":[{"a":321,"b":334}]}],"statement":"MERGE INTO federated.competitor_category_progress AS t\nUSING (\n  SELECT\n    entry.competitor_id,\n    entry.category_id,\n    entry.points,\n    entry.domestic_finals,\n    entry.foreign_finals\n  FROM unnest(\n    :competitorId::text[],\n    :categoryId::bigint[],\n    :points::numeric(10,3)[],\n    :domesticFinals::int[],\n    :foreignFinals::int[]\n  ) entry (competitor_id, category_id, points, domestic_finals, foreign_finals)\n) AS s\nON t.competitor_id = s.competitor_id AND t.category_id = s.category_id\nWHEN MATCHED THEN\n  UPDATE SET points = s.points, domestic_finals = s.domestic_finals, foreign_finals = s.foreign_finals\nWHEN NOT MATCHED THEN\n  INSERT (competitor_id, category_id, points, domestic_finals, foreign_finals)\n  VALUES (s.competitor_id, s.category_id, s.points, s.domestic_finals, s.foreign_finals)\nWHEN NOT MATCHED BY SOURCE\n  AND t.competitor_id IN (SELECT unnest(:competitorId::text[])) THEN\n  DELETE"};

/**
 * Query generated from SQL:
 * ```
 * MERGE INTO federated.competitor_category_progress AS t
 * USING (
 *   SELECT
 *     entry.competitor_id,
 *     entry.category_id,
 *     entry.points,
 *     entry.domestic_finals,
 *     entry.foreign_finals
 *   FROM unnest(
 *     :competitorId::text[],
 *     :categoryId::bigint[],
 *     :points::numeric(10,3)[],
 *     :domesticFinals::int[],
 *     :foreignFinals::int[]
 *   ) entry (competitor_id, category_id, points, domestic_finals, foreign_finals)
 * ) AS s
 * ON t.competitor_id = s.competitor_id AND t.category_id = s.category_id
 * WHEN MATCHED THEN
 *   UPDATE SET points = s.points, domestic_finals = s.domestic_finals, foreign_finals = s.foreign_finals
 * WHEN NOT MATCHED THEN
 *   INSERT (competitor_id, category_id, points, domestic_finals, foreign_finals)
 *   VALUES (s.competitor_id, s.category_id, s.points, s.domestic_finals, s.foreign_finals)
 * WHEN NOT MATCHED BY SOURCE
 *   AND t.competitor_id IN (SELECT unnest(:competitorId::text[])) THEN
 *   DELETE
 * ```
 */
export const mergeCompetitorProgress = new PreparedQuery<IMergeCompetitorProgressParams,IMergeCompetitorProgressResult>(mergeCompetitorProgressIR);


/** 'UpsertRanklistSnapshot' parameters type */
export interface IUpsertRanklistSnapshotParams {
  asOfDate?: DateOrString | null | void;
  categoryId?: NumberOrString | null | void;
  entries?: Json | null | void;
  federation?: string | null | void;
  kind?: string | null | void;
  ranklistName?: string | null | void;
}

/** 'UpsertRanklistSnapshot' return type */
export interface IUpsertRanklistSnapshotResult {
  id: string;
}

/** 'UpsertRanklistSnapshot' query type */
export interface IUpsertRanklistSnapshotQuery {
  params: IUpsertRanklistSnapshotParams;
  result: IUpsertRanklistSnapshotResult;
}

const upsertRanklistSnapshotIR: any = {"usedParamSet":{"federation":true,"categoryId":true,"ranklistName":true,"asOfDate":true,"kind":true,"entries":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":101,"b":111}]},{"name":"categoryId","required":false,"transform":{"type":"scalar"},"locs":[{"a":114,"b":124}]},{"name":"ranklistName","required":false,"transform":{"type":"scalar"},"locs":[{"a":127,"b":139}]},{"name":"asOfDate","required":false,"transform":{"type":"scalar"},"locs":[{"a":347,"b":355}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":367,"b":371}]},{"name":"entries","required":false,"transform":{"type":"scalar"},"locs":[{"a":871,"b":878}]}],"statement":"WITH upsert_ranklist AS (\n  INSERT INTO federated.ranklist (federation, category_id, name)\n  VALUES (:federation, :categoryId, :ranklistName)\n  ON CONFLICT (federation, category_id)\n    DO UPDATE SET name = EXCLUDED.name\n  RETURNING id\n),\nupsert_snapshot AS (\n  INSERT INTO federated.ranklist_snapshot (ranklist_id, as_of_date, kind)\n  SELECT id, :asOfDate, COALESCE(:kind, 'default')\n  FROM upsert_ranklist\n  ON CONFLICT (ranklist_id, as_of_date, kind)\n    DO UPDATE SET kind = EXCLUDED.kind\n  RETURNING id\n),\ndelete_old AS (\n  DELETE FROM federated.ranklist_entry WHERE snapshot_id = (SELECT id FROM upsert_snapshot)\n),\ninsert_entries AS (\n  INSERT INTO federated.ranklist_entry (snapshot_id, competitor_id, ranking, ranking_to, points)\n  SELECT s.id, e.competitor_id, e.ranking, e.ranking_to, e.points\n  FROM upsert_snapshot s\n  CROSS JOIN jsonb_to_recordset(COALESCE(:entries, '[]'::jsonb)) AS e(\n    competitor_id text,\n    ranking       integer,\n    ranking_to    integer,\n    points        numeric(10,3)\n  )\n)\nSELECT id FROM upsert_snapshot"};

/**
 * Query generated from SQL:
 * ```
 * WITH upsert_ranklist AS (
 *   INSERT INTO federated.ranklist (federation, category_id, name)
 *   VALUES (:federation, :categoryId, :ranklistName)
 *   ON CONFLICT (federation, category_id)
 *     DO UPDATE SET name = EXCLUDED.name
 *   RETURNING id
 * ),
 * upsert_snapshot AS (
 *   INSERT INTO federated.ranklist_snapshot (ranklist_id, as_of_date, kind)
 *   SELECT id, :asOfDate, COALESCE(:kind, 'default')
 *   FROM upsert_ranklist
 *   ON CONFLICT (ranklist_id, as_of_date, kind)
 *     DO UPDATE SET kind = EXCLUDED.kind
 *   RETURNING id
 * ),
 * delete_old AS (
 *   DELETE FROM federated.ranklist_entry WHERE snapshot_id = (SELECT id FROM upsert_snapshot)
 * ),
 * insert_entries AS (
 *   INSERT INTO federated.ranklist_entry (snapshot_id, competitor_id, ranking, ranking_to, points)
 *   SELECT s.id, e.competitor_id, e.ranking, e.ranking_to, e.points
 *   FROM upsert_snapshot s
 *   CROSS JOIN jsonb_to_recordset(COALESCE(:entries, '[]'::jsonb)) AS e(
 *     competitor_id text,
 *     ranking       integer,
 *     ranking_to    integer,
 *     points        numeric(10,3)
 *   )
 * )
 * SELECT id FROM upsert_snapshot
 * ```
 */
export const upsertRanklistSnapshot = new PreparedQuery<IUpsertRanklistSnapshotParams,IUpsertRanklistSnapshotResult>(upsertRanklistSnapshotIR);


/** 'UpsertEvent' parameters type */
export interface IUpsertEventParams {
  country?: string | null | void;
  endDate?: DateOrString | null | void;
  externalId?: string | null | void;
  federation?: string | null | void;
  location?: string | null | void;
  name?: string | null | void;
  organizingClubId?: NumberOrString | null | void;
  startDate?: DateOrString | null | void;
}

/** 'UpsertEvent' return type */
export type IUpsertEventResult = void;

/** 'UpsertEvent' query type */
export interface IUpsertEventQuery {
  params: IUpsertEventParams;
  result: IUpsertEventResult;
}

const upsertEventIR: any = {"usedParamSet":{"federation":true,"externalId":true,"name":true,"startDate":true,"endDate":true,"location":true,"country":true,"organizingClubId":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":129,"b":139}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":142,"b":152}]},{"name":"name","required":false,"transform":{"type":"scalar"},"locs":[{"a":155,"b":159}]},{"name":"startDate","required":false,"transform":{"type":"scalar"},"locs":[{"a":162,"b":171}]},{"name":"endDate","required":false,"transform":{"type":"scalar"},"locs":[{"a":180,"b":187}]},{"name":"location","required":false,"transform":{"type":"scalar"},"locs":[{"a":196,"b":204}]},{"name":"country","required":false,"transform":{"type":"scalar"},"locs":[{"a":207,"b":214}]},{"name":"organizingClubId","required":false,"transform":{"type":"scalar"},"locs":[{"a":217,"b":233}]}],"statement":"INSERT INTO federated.event (federation, external_id, name, start_date, end_date, location, country, organizing_club_id)\nVALUES (:federation, :externalId, :name, :startDate::date, :endDate::date, :location, :country, :organizingClubId)\nON CONFLICT (federation, external_id) DO UPDATE\n  SET name = EXCLUDED.name,\n      start_date = EXCLUDED.start_date,\n      end_date = EXCLUDED.end_date,\n      location = EXCLUDED.location,\n      country = EXCLUDED.country,\n      organizing_club_id = EXCLUDED.organizing_club_id"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO federated.event (federation, external_id, name, start_date, end_date, location, country, organizing_club_id)
 * VALUES (:federation, :externalId, :name, :startDate::date, :endDate::date, :location, :country, :organizingClubId)
 * ON CONFLICT (federation, external_id) DO UPDATE
 *   SET name = EXCLUDED.name,
 *       start_date = EXCLUDED.start_date,
 *       end_date = EXCLUDED.end_date,
 *       location = EXCLUDED.location,
 *       country = EXCLUDED.country,
 *       organizing_club_id = EXCLUDED.organizing_club_id
 * ```
 */
export const upsertEvent = new PreparedQuery<IUpsertEventParams,IUpsertEventResult>(upsertEventIR);


