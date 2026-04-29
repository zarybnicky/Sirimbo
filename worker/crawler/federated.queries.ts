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

export type numberArray = (number)[];

export type stringArray = (string)[];

/** 'UpsertPerson' parameters type */
export interface IUpsertPersonParams {
  canonicalName?: string | null | void;
  externalId?: string | null | void;
  federation?: string | null | void;
  gender?: gender | null | void;
}

/** 'UpsertPerson' return type */
export type IUpsertPersonResult = void;

/** 'UpsertPerson' query type */
export interface IUpsertPersonQuery {
  params: IUpsertPersonParams;
  result: IUpsertPersonResult;
}

const upsertPersonIR: any = {"usedParamSet":{"federation":true,"externalId":true,"canonicalName":true,"gender":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":97,"b":107}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":110,"b":120}]},{"name":"canonicalName","required":false,"transform":{"type":"scalar"},"locs":[{"a":123,"b":136}]},{"name":"gender","required":false,"transform":{"type":"scalar"},"locs":[{"a":139,"b":145}]}],"statement":"INSERT INTO federated.person AS person (federation, external_id, canonical_name, gender)\nVALUES (:federation, :externalId, :canonicalName, :gender)\nON CONFLICT (federation, external_id)\n  DO UPDATE SET canonical_name = EXCLUDED.canonical_name\n  WHERE person.canonical_name <> EXCLUDED.canonical_name"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO federated.person AS person (federation, external_id, canonical_name, gender)
 * VALUES (:federation, :externalId, :canonicalName, :gender)
 * ON CONFLICT (federation, external_id)
 *   DO UPDATE SET canonical_name = EXCLUDED.canonical_name
 *   WHERE person.canonical_name <> EXCLUDED.canonical_name
 * ```
 */
export const upsertPerson = new PreparedQuery<IUpsertPersonParams,IUpsertPersonResult>(upsertPersonIR);


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


/** 'UpsertCompetitor' parameters type */
export interface IUpsertCompetitorParams {
  component_person_ids?: stringArray | null | void;
  component_roles?: competitor_roleArray | null | void;
  externalId?: string | null | void;
  federation?: string | null | void;
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

const upsertCompetitorIR: any = {"usedParamSet":{"federation":true,"externalId":true,"type":true,"label":true,"component_person_ids":true,"component_roles":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":55,"b":65}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":88,"b":98}]},{"name":"type","required":false,"transform":{"type":"scalar"},"locs":[{"a":114,"b":118}]},{"name":"label","required":false,"transform":{"type":"scalar"},"locs":[{"a":162,"b":167}]},{"name":"component_person_ids","required":false,"transform":{"type":"scalar"},"locs":[{"a":290,"b":310}]},{"name":"component_roles","required":false,"transform":{"type":"scalar"},"locs":[{"a":327,"b":342}]}],"statement":"SELECT federated.upsert_competitor(\n  in_federation => :federation,\n  in_external_id => :externalId,\n  in_type => :type::federated.competitor_type,\n  in_label => :label,\n  in_components => ARRAY(\n    SELECT (u.person_id, u.role)::federated.competitor_component_input\n    FROM unnest(\n      :component_person_ids::text[],\n      :component_roles::federated.competitor_role[]\n    ) AS u(person_id, role)\n  )\n) as competitor_id"};

/**
 * Query generated from SQL:
 * ```
 * SELECT federated.upsert_competitor(
 *   in_federation => :federation,
 *   in_external_id => :externalId,
 *   in_type => :type::federated.competitor_type,
 *   in_label => :label,
 *   in_components => ARRAY(
 *     SELECT (u.person_id, u.role)::federated.competitor_component_input
 *     FROM unnest(
 *       :component_person_ids::text[],
 *       :component_roles::federated.competitor_role[]
 *     ) AS u(person_id, role)
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
export type IUpsertManyCompetitorsResult = void;

/** 'UpsertManyCompetitors' query type */
export interface IUpsertManyCompetitorsQuery {
  params: IUpsertManyCompetitorsParams;
  result: IUpsertManyCompetitorsResult;
}

const upsertManyCompetitorsIR: any = {"usedParamSet":{"federations":true,"external_ids":true,"types":true,"labels":true},"params":[{"name":"federations","required":false,"transform":{"type":"scalar"},"locs":[{"a":175,"b":186}]},{"name":"external_ids","required":false,"transform":{"type":"scalar"},"locs":[{"a":199,"b":211}]},{"name":"types","required":false,"transform":{"type":"scalar"},"locs":[{"a":224,"b":229}]},{"name":"labels","required":false,"transform":{"type":"scalar"},"locs":[{"a":263,"b":269}]}],"statement":"INSERT INTO federated.competitor (federation, external_id, competitor_type, name)\nSELECT input.federation, input.external_id, input.competitor_type, input.name\nFROM unnest(\n  :federations::text[],\n  :external_ids::text[],\n  :types::federated.competitor_type[],\n  :labels::text[]\n) AS input(federation, external_id, competitor_type, name)\nON CONFLICT DO NOTHING"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO federated.competitor (federation, external_id, competitor_type, name)
 * SELECT input.federation, input.external_id, input.competitor_type, input.name
 * FROM unnest(
 *   :federations::text[],
 *   :external_ids::text[],
 *   :types::federated.competitor_type[],
 *   :labels::text[]
 * ) AS input(federation, external_id, competitor_type, name)
 * ON CONFLICT DO NOTHING
 * ```
 */
export const upsertManyCompetitors = new PreparedQuery<IUpsertManyCompetitorsParams,IUpsertManyCompetitorsResult>(upsertManyCompetitorsIR);


/** 'ReplaceCompetitorProgress' parameters type */
export interface IReplaceCompetitorProgressParams {
  category_ids?: NumberOrStringArray | null | void;
  competitorId?: string | null | void;
  domestic_finales?: numberArray | null | void;
  foreign_finales?: numberArray | null | void;
  points?: NumberOrStringArray | null | void;
}

/** 'ReplaceCompetitorProgress' return type */
export interface IReplaceCompetitorProgressResult {
  replace_competitor_category_progress: undefined | null;
}

/** 'ReplaceCompetitorProgress' query type */
export interface IReplaceCompetitorProgressQuery {
  params: IReplaceCompetitorProgressParams;
  result: IReplaceCompetitorProgressResult;
}

const replaceCompetitorProgressIR: any = {"usedParamSet":{"competitorId":true,"category_ids":true,"points":true,"domestic_finales":true,"foreign_finales":true},"params":[{"name":"competitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":77,"b":89}]},{"name":"category_ids","required":false,"transform":{"type":"scalar"},"locs":[{"a":294,"b":306}]},{"name":"points","required":false,"transform":{"type":"scalar"},"locs":[{"a":325,"b":331}]},{"name":"domestic_finales","required":false,"transform":{"type":"scalar"},"locs":[{"a":357,"b":373}]},{"name":"foreign_finales","required":false,"transform":{"type":"scalar"},"locs":[{"a":389,"b":404}]}],"statement":"SELECT federated.replace_competitor_category_progress(\n  in_competitor_id => :competitorId,\n  in_entries       => ARRAY(\n    SELECT (\n      u.category_id,\n      u.points,\n      u.domestic_finale,\n      u.foreign_finale\n    )::federated.competitor_category_progress_input\n    FROM unnest(\n      :category_ids::bigint[],\n      :points::numeric(10,3)[],\n      :domestic_finales::int[],\n      :foreign_finales::int[]\n    ) AS u(category_id, points, domestic_finale, foreign_finale)\n  )\n)"};

/**
 * Query generated from SQL:
 * ```
 * SELECT federated.replace_competitor_category_progress(
 *   in_competitor_id => :competitorId,
 *   in_entries       => ARRAY(
 *     SELECT (
 *       u.category_id,
 *       u.points,
 *       u.domestic_finale,
 *       u.foreign_finale
 *     )::federated.competitor_category_progress_input
 *     FROM unnest(
 *       :category_ids::bigint[],
 *       :points::numeric(10,3)[],
 *       :domestic_finales::int[],
 *       :foreign_finales::int[]
 *     ) AS u(category_id, points, domestic_finale, foreign_finale)
 *   )
 * )
 * ```
 */
export const replaceCompetitorProgress = new PreparedQuery<IReplaceCompetitorProgressParams,IReplaceCompetitorProgressResult>(replaceCompetitorProgressIR);


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


