/** Types generated for queries found in "crawler/federated.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type competitor_type = 'couple' | 'duo' | 'formation' | 'solo' | 'team';

export type gender = 'female' | 'male' | 'other' | 'unknown';

export type Json = null | boolean | number | string | Json[] | { [key: string]: Json };

export type JsonArray = (Json)[];

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

const upsertFederationAthleteIR: any = {"usedParamSet":{"federation":true,"externalId":true,"canonicalName":true,"gender":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":105,"b":115},{"a":755,"b":765}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":142,"b":152},{"a":768,"b":778}]},{"name":"canonicalName","required":false,"transform":{"type":"scalar"},"locs":[{"a":261,"b":274}]},{"name":"gender","required":false,"transform":{"type":"scalar"},"locs":[{"a":277,"b":283}]}],"statement":"WITH existing AS (\n  SELECT fa.athlete_id\n  FROM federated.federation_athlete fa\n  WHERE fa.federation = :federation\n    AND fa.external_id = :externalId::text\n    FOR UPDATE\n), person_ins AS (\n  INSERT INTO federated.person (canonical_name, gender)\n    SELECT :canonicalName, :gender::federated.gender\n    WHERE NOT EXISTS (SELECT 1 FROM existing)\n    RETURNING id\n), athlete_ins AS (\n  INSERT INTO federated.athlete (person_id)\n    SELECT id\n    FROM person_ins\n    WHERE NOT EXISTS (SELECT 1 FROM existing)\n    RETURNING id AS athlete_id\n), athlete_final AS (\n  SELECT athlete_id FROM existing\n  UNION ALL\n  SELECT athlete_id FROM athlete_ins\n), fa_final as (\n  INSERT INTO federated.federation_athlete (federation, external_id, athlete_id)\n    SELECT :federation, :externalId, athlete_id\n    FROM athlete_final\n    ON CONFLICT (federation, external_id)\n      DO UPDATE SET athlete_id = EXCLUDED.athlete_id\n)\nSELECT athlete_id FROM athlete_final"};

/**
 * Query generated from SQL:
 * ```
 * WITH existing AS (
 *   SELECT fa.athlete_id
 *   FROM federated.federation_athlete fa
 *   WHERE fa.federation = :federation
 *     AND fa.external_id = :externalId::text
 *     FOR UPDATE
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
 * ), fa_final as (
 *   INSERT INTO federated.federation_athlete (federation, external_id, athlete_id)
 *     SELECT :federation, :externalId, athlete_id
 *     FROM athlete_final
 *     ON CONFLICT (federation, external_id)
 *       DO UPDATE SET athlete_id = EXCLUDED.athlete_id
 * )
 * SELECT athlete_id FROM athlete_final
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


/** 'UpsertCompetitor' parameters type */
export interface IUpsertCompetitorParams {
  components?: JsonArray | null | void;
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

const upsertCompetitorIR: any = {"usedParamSet":{"federation":true,"federationCompetitorId":true,"type":true,"label":true,"components":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":55,"b":65}]},{"name":"federationCompetitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":88,"b":110}]},{"name":"type","required":false,"transform":{"type":"scalar"},"locs":[{"a":126,"b":130}]},{"name":"label","required":false,"transform":{"type":"scalar"},"locs":[{"a":174,"b":179}]},{"name":"components","required":false,"transform":{"type":"scalar"},"locs":[{"a":309,"b":319}]}],"statement":"SELECT federated.upsert_competitor(\n  in_federation => :federation,\n  in_external_id => :federationCompetitorId,\n  in_type => :type::federated.competitor_type,\n  in_label => :label,\n  in_components => (select array_agg(x) from json_populate_recordset(null::federated.competitor_component_input, array_to_json(:components::json[])) x)\n) as competitor_id"};

/**
 * Query generated from SQL:
 * ```
 * SELECT federated.upsert_competitor(
 *   in_federation => :federation,
 *   in_external_id => :federationCompetitorId,
 *   in_type => :type::federated.competitor_type,
 *   in_label => :label,
 *   in_components => (select array_agg(x) from json_populate_recordset(null::federated.competitor_component_input, array_to_json(:components::json[])) x)
 * ) as competitor_id
 * ```
 */
export const upsertCompetitor = new PreparedQuery<IUpsertCompetitorParams,IUpsertCompetitorResult>(upsertCompetitorIR);


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
export type IUpsertCompetitorProgressResult = void;

/** 'UpsertCompetitorProgress' query type */
export interface IUpsertCompetitorProgressQuery {
  params: IUpsertCompetitorProgressParams;
  result: IUpsertCompetitorProgressResult;
}

const upsertCompetitorProgressIR: any = {"usedParamSet":{"federation":true,"competitorId":true,"categoryId":true,"points":true,"domesticFinale":true,"foreignFinale":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":158,"b":168}]},{"name":"competitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":173,"b":185}]},{"name":"categoryId","required":false,"transform":{"type":"scalar"},"locs":[{"a":190,"b":200}]},{"name":"points","required":false,"transform":{"type":"scalar"},"locs":[{"a":205,"b":211}]},{"name":"domesticFinale","required":false,"transform":{"type":"scalar"},"locs":[{"a":232,"b":246}]},{"name":"foreignFinale","required":false,"transform":{"type":"scalar"},"locs":[{"a":256,"b":269}]}],"statement":"INSERT INTO federated.competitor_category_progress (\n  federation,\n  competitor_id,\n  category_id,\n  points,\n  domestic_finale,\n  foreign_finale\n)\nVALUES (\n  :federation,\n  :competitorId,\n  :categoryId,\n  :points::numeric(10, 3),\n  :domesticFinale::int,\n  :foreignFinale::int\n)\nON CONFLICT (federation, competitor_id, category_id)\n  DO UPDATE\n  SET points          = EXCLUDED.points,\n      domestic_finale = EXCLUDED.domestic_finale,\n      foreign_finale  = EXCLUDED.foreign_finale"};

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
 *   :competitorId,
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
export const upsertCompetitorProgress = new PreparedQuery<IUpsertCompetitorProgressParams,IUpsertCompetitorProgressResult>(upsertCompetitorProgressIR);


