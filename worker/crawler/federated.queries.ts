/** Types generated for queries found in "crawler/federated.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type gender = 'female' | 'male' | 'other' | 'unknown';

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


/** 'UpsertFederationCouple' parameters type */
export interface IUpsertFederationCoupleParams {
  competitorLabel?: string | null | void;
  externalCompetitorId?: string | null | void;
  federatedFollowerId?: NumberOrString | null | void;
  federatedLeadId?: NumberOrString | null | void;
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

const upsertFederationCoupleIR: any = {"usedParamSet":{"federation":true,"externalCompetitorId":true,"competitorLabel":true,"federatedLeadId":true,"federatedFollowerId":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":183,"b":193},{"a":1035,"b":1045}]},{"name":"externalCompetitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":196,"b":216},{"a":1073,"b":1093}]},{"name":"competitorLabel","required":false,"transform":{"type":"scalar"},"locs":[{"a":520,"b":535},{"a":1219,"b":1234}]},{"name":"federatedLeadId","required":false,"transform":{"type":"scalar"},"locs":[{"a":1429,"b":1444}]},{"name":"federatedFollowerId","required":false,"transform":{"type":"scalar"},"locs":[{"a":1741,"b":1760}]}],"statement":"WITH fc AS (\n  -- Ensure federation_competitor row exists, get current competitor_id (may be NULL)\n  INSERT INTO federated.federation_competitor (federation, external_id)\n    VALUES (:federation, :externalCompetitorId::text)\n    ON CONFLICT (federation, external_id)\n      DO UPDATE SET external_id = EXCLUDED.external_id\n    RETURNING competitor_id\n), comp_ins AS (\n  -- If there's no competitor yet, create one\n  INSERT INTO federated.competitor (competitor_type, name)\n    SELECT 'couple'::federated.competitor_type, :competitorLabel\n    WHERE (SELECT competitor_id FROM fc) IS NULL\n    RETURNING id AS competitor_id\n), comp_final AS (\n  -- Final competitor_id: existing or newly inserted\n  SELECT COALESCE(\n           (SELECT competitor_id FROM fc),\n           (SELECT competitor_id FROM comp_ins)\n         ) AS competitor_id\n), comp_link AS (\n  -- Link federation_competitor to the final competitor\n  UPDATE federated.federation_competitor f\n    SET competitor_id = c.competitor_id\n    FROM comp_final c\n    WHERE f.federation  = :federation\n      AND f.external_id = :externalCompetitorId::text\n), comp_name AS (\n  -- Keep competitor name in sync with latest label\n  UPDATE federated.competitor co\n    SET name = :competitorLabel\n    FROM comp_final c\n    WHERE co.id = c.competitor_id\n), comp_lead AS (\n  INSERT INTO federated.competitor_component (competitor_id, athlete_id, role)\n    SELECT\n      c.competitor_id,\n      :federatedLeadId::bigint,\n      'lead'::federated.competitor_role\n    FROM comp_final c\n    ON CONFLICT (competitor_id, athlete_id)\n      DO UPDATE SET role = EXCLUDED.role\n), comp_follow AS (\n  INSERT INTO federated.competitor_component (competitor_id, athlete_id, role)\n    SELECT\n      c.competitor_id,\n      :federatedFollowerId::bigint,\n      'follow'::federated.competitor_role\n    FROM comp_final c\n    ON CONFLICT (competitor_id, athlete_id)\n      DO UPDATE SET role = EXCLUDED.role\n)\nSELECT competitor_id FROM comp_final"};

/**
 * Query generated from SQL:
 * ```
 * WITH fc AS (
 *   -- Ensure federation_competitor row exists, get current competitor_id (may be NULL)
 *   INSERT INTO federated.federation_competitor (federation, external_id)
 *     VALUES (:federation, :externalCompetitorId::text)
 *     ON CONFLICT (federation, external_id)
 *       DO UPDATE SET external_id = EXCLUDED.external_id
 *     RETURNING competitor_id
 * ), comp_ins AS (
 *   -- If there's no competitor yet, create one
 *   INSERT INTO federated.competitor (competitor_type, name)
 *     SELECT 'couple'::federated.competitor_type, :competitorLabel
 *     WHERE (SELECT competitor_id FROM fc) IS NULL
 *     RETURNING id AS competitor_id
 * ), comp_final AS (
 *   -- Final competitor_id: existing or newly inserted
 *   SELECT COALESCE(
 *            (SELECT competitor_id FROM fc),
 *            (SELECT competitor_id FROM comp_ins)
 *          ) AS competitor_id
 * ), comp_link AS (
 *   -- Link federation_competitor to the final competitor
 *   UPDATE federated.federation_competitor f
 *     SET competitor_id = c.competitor_id
 *     FROM comp_final c
 *     WHERE f.federation  = :federation
 *       AND f.external_id = :externalCompetitorId::text
 * ), comp_name AS (
 *   -- Keep competitor name in sync with latest label
 *   UPDATE federated.competitor co
 *     SET name = :competitorLabel
 *     FROM comp_final c
 *     WHERE co.id = c.competitor_id
 * ), comp_lead AS (
 *   INSERT INTO federated.competitor_component (competitor_id, athlete_id, role)
 *     SELECT
 *       c.competitor_id,
 *       :federatedLeadId::bigint,
 *       'lead'::federated.competitor_role
 *     FROM comp_final c
 *     ON CONFLICT (competitor_id, athlete_id)
 *       DO UPDATE SET role = EXCLUDED.role
 * ), comp_follow AS (
 *   INSERT INTO federated.competitor_component (competitor_id, athlete_id, role)
 *     SELECT
 *       c.competitor_id,
 *       :federatedFollowerId::bigint,
 *       'follow'::federated.competitor_role
 *     FROM comp_final c
 *     ON CONFLICT (competitor_id, athlete_id)
 *       DO UPDATE SET role = EXCLUDED.role
 * )
 * SELECT competitor_id FROM comp_final
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

const upsertFederationCoupleProgressIR: any = {"usedParamSet":{"federation":true,"federatedCompetitorId":true,"categoryId":true,"points":true,"domesticFinale":true,"foreignFinale":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":165,"b":175}]},{"name":"federatedCompetitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":187,"b":208}]},{"name":"categoryId","required":false,"transform":{"type":"scalar"},"locs":[{"a":220,"b":230}]},{"name":"points","required":false,"transform":{"type":"scalar"},"locs":[{"a":242,"b":248}]},{"name":"domesticFinale","required":false,"transform":{"type":"scalar"},"locs":[{"a":276,"b":290}]},{"name":"foreignFinale","required":false,"transform":{"type":"scalar"},"locs":[{"a":307,"b":320}]}],"statement":"INSERT INTO federated.competitor_category_progress (\n  federation,\n  competitor_id,\n  category_id,\n  points,\n  domestic_finale,\n  foreign_finale\n)\nVALUES (\n         :federation,\n         :federatedCompetitorId,\n         :categoryId,\n         :points::numeric(10, 3),\n         :domesticFinale::int,\n         :foreignFinale::int\n       )\nON CONFLICT (federation, competitor_id, category_id)\n  DO UPDATE\n  SET points          = EXCLUDED.points,\n      domestic_finale = EXCLUDED.domestic_finale,\n      foreign_finale  = EXCLUDED.foreign_finale"};

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
 *          :federation,
 *          :federatedCompetitorId,
 *          :categoryId,
 *          :points::numeric(10, 3),
 *          :domesticFinale::int,
 *          :foreignFinale::int
 *        )
 * ON CONFLICT (federation, competitor_id, category_id)
 *   DO UPDATE
 *   SET points          = EXCLUDED.points,
 *       domestic_finale = EXCLUDED.domestic_finale,
 *       foreign_finale  = EXCLUDED.foreign_finale
 * ```
 */
export const upsertFederationCoupleProgress = new PreparedQuery<IUpsertFederationCoupleProgressParams,IUpsertFederationCoupleProgressResult>(upsertFederationCoupleProgressIR);


