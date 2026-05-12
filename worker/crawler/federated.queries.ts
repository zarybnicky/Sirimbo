/** Types generated for queries found in "crawler/federated.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type competition_type = 'championship' | 'cup' | 'g_cup' | 'league' | 'ranking' | 'super_league' | 'top_level' | 'unknown';

export type competitor_role = 'follow' | 'lead' | 'member' | 'substitute';

export type competitor_type = 'couple' | 'duo' | 'formation' | 'group' | 'solo' | 'team' | 'trio';

export type gender = 'female' | 'male' | 'other' | 'unknown';

export type official_role = 'adjudicator' | 'chairperson' | 'invigilator' | 'lead_scrutineer' | 'scrutineer';

export type person_license_discipline = 'breaking' | 'caribbean' | 'disco' | 'formation_latin' | 'formation_standard' | 'general' | 'hiphop' | 'latin' | 'pd_latin' | 'pd_show_dance_latin' | 'pd_show_dance_standard' | 'pd_standard' | 'pd_ten_dance' | 'show_dance_latin' | 'show_dance_standard' | 'smooth' | 'solo_syncro_choreo' | 'stage' | 'standard' | 'ten_dance' | 'unknown';

export type person_license_kind = 'adjudicator' | 'athlete' | 'chairperson' | 'dj' | 'examiner' | 'head_judge' | 'invigilator' | 'lead_scrutineer' | 'official' | 'scrutineer' | 'trainer';

export type person_license_status = 'active' | 'aspiring' | 'expired' | 'resting' | 'retired' | 'revoked' | 'suspended' | 'unknown';

export type score_component = 'ajs_cp' | 'ajs_mm' | 'ajs_ps' | 'ajs_reduction' | 'ajs_tq' | 'mark' | 'places';

export type scoring_method = 'ajs-3.0' | 'skating_marks' | 'skating_places';

export type DateOrString = Date | string;

export type DateOrStringArray = (DateOrString)[];

export type NumberOrString = number | string;

export type NumberOrStringArray = (NumberOrString)[];

export type booleanArray = (boolean)[];

export type competition_typeArray = (competition_type)[];

export type competitor_roleArray = (competitor_role)[];

export type competitor_typeArray = (competitor_type)[];

export type genderArray = (gender)[];

export type numberArray = (number)[];

export type official_roleArray = (official_role)[];

export type person_license_disciplineArray = (person_license_discipline)[];

export type person_license_kindArray = (person_license_kind)[];

export type person_license_statusArray = (person_license_status)[];

export type score_componentArray = (score_component)[];

export type scoring_methodArray = (scoring_method)[];

export type stringArray = (string)[];

/** 'EnsurePeople' parameters type */
export interface IEnsurePeopleParams {
  canonicalName?: stringArray | null | void;
  externalId?: NumberOrStringArray | null | void;
  federation?: stringArray | null | void;
  gender?: genderArray | null | void;
}

/** 'EnsurePeople' return type */
export type IEnsurePeopleResult = void;

/** 'EnsurePeople' query type */
export interface IEnsurePeopleQuery {
  params: IEnsurePeopleParams;
  result: IEnsurePeopleResult;
}

const ensurePeopleIR: any = {"usedParamSet":{"federation":true,"externalId":true,"canonicalName":true,"gender":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":149,"b":159}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":172,"b":182}]},{"name":"canonicalName","required":false,"transform":{"type":"scalar"},"locs":[{"a":197,"b":210}]},{"name":"gender","required":false,"transform":{"type":"scalar"},"locs":[{"a":223,"b":229}]}],"statement":"INSERT INTO federated.person (federation, external_id, canonical_name, gender)\nSELECT federation, external_id, canonical_name, gender\nFROM unnest(\n  :federation::text[],\n  :externalId::bigint[],\n  :canonicalName::text[],\n  :gender::federated.gender[]\n) AS i (federation, external_id, canonical_name, gender)\nON CONFLICT (id) DO NOTHING"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO federated.person (federation, external_id, canonical_name, gender)
 * SELECT federation, external_id, canonical_name, gender
 * FROM unnest(
 *   :federation::text[],
 *   :externalId::bigint[],
 *   :canonicalName::text[],
 *   :gender::federated.gender[]
 * ) AS i (federation, external_id, canonical_name, gender)
 * ON CONFLICT (id) DO NOTHING
 * ```
 */
export const ensurePeople = new PreparedQuery<IEnsurePeopleParams,IEnsurePeopleResult>(ensurePeopleIR);


/** 'MergePersonLicenses' parameters type */
export interface IMergePersonLicensesParams {
  canonicalName?: stringArray | null | void;
  discipline?: person_license_disciplineArray | null | void;
  externalId?: NumberOrStringArray | null | void;
  gender?: genderArray | null | void;
  grade?: stringArray | null | void;
  kind?: person_license_kindArray | null | void;
  managedDiscipline?: person_license_disciplineArray | null | void;
  managedKind?: person_license_kindArray | null | void;
  scopeFederation?: string | null | void;
  scopePersonId?: stringArray | null | void;
  status?: person_license_statusArray | null | void;
  validUntil?: stringArray | null | void;
}

/** 'MergePersonLicenses' return type */
export type IMergePersonLicensesResult = void;

/** 'MergePersonLicenses' query type */
export interface IMergePersonLicensesQuery {
  params: IMergePersonLicensesParams;
  result: IMergePersonLicensesResult;
}

const mergePersonLicensesIR: any = {"usedParamSet":{"scopeFederation":true,"externalId":true,"canonicalName":true,"gender":true,"kind":true,"discipline":true,"grade":true,"validUntil":true,"status":true,"scopePersonId":true,"managedKind":true,"managedDiscipline":true},"params":[{"name":"scopeFederation","required":false,"transform":{"type":"scalar"},"locs":[{"a":29,"b":44},{"a":2098,"b":2113}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":176,"b":186}]},{"name":"canonicalName","required":false,"transform":{"type":"scalar"},"locs":[{"a":203,"b":216}]},{"name":"gender","required":false,"transform":{"type":"scalar"},"locs":[{"a":231,"b":237}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":264,"b":268}]},{"name":"discipline","required":false,"transform":{"type":"scalar"},"locs":[{"a":308,"b":318}]},{"name":"grade","required":false,"transform":{"type":"scalar"},"locs":[{"a":364,"b":369}]},{"name":"validUntil","required":false,"transform":{"type":"scalar"},"locs":[{"a":384,"b":394}]},{"name":"status","required":false,"transform":{"type":"scalar"},"locs":[{"a":409,"b":415}]},{"name":"scopePersonId","required":false,"transform":{"type":"scalar"},"locs":[{"a":1159,"b":1172}]},{"name":"managedKind","required":false,"transform":{"type":"scalar"},"locs":[{"a":1264,"b":1275}]},{"name":"managedDiscipline","required":false,"transform":{"type":"scalar"},"locs":[{"a":1315,"b":1332}]}],"statement":"WITH input AS (\n  SELECT\n    :scopeFederation::text AS federation,\n    external_id, canonical_name, gender, kind, discipline,\n    grade, valid_until, status\n  FROM unnest(\n    :externalId::bigint[],\n    :canonicalName::text[],\n    :gender::federated.gender[],\n    :kind::federated.person_license_kind[],\n    :discipline::federated.person_license_discipline[],\n    :grade::text[],\n    :validUntil::text[],\n    :status::federated.person_license_status[]\n  ) AS input(\n    external_id, canonical_name, gender, kind, discipline,\n    grade, valid_until, status\n  )\n), inserted_person AS (\n  INSERT INTO federated.person (federation, external_id, canonical_name, gender)\n  SELECT DISTINCT federation, external_id, canonical_name, gender\n  FROM input\n  ON CONFLICT (id) DO NOTHING\n), source AS (\n  SELECT DISTINCT ON (federation, external_id, kind, discipline)\n    federation || ':' || external_id::text AS person_id,\n    federation,\n    kind,\n    discipline,\n    nullif(grade, '') AS grade,\n    nullif(valid_until, '')::date AS valid_until,\n    status\n  FROM input\n  ORDER BY federation, external_id, kind, discipline\n), scope AS (\n  SELECT person_id\n  FROM unnest(:scopePersonId::text[]) AS scope(person_id)\n), managed AS (\n  SELECT kind, discipline\n  FROM unnest(\n    :managedKind::federated.person_license_kind[],\n    :managedDiscipline::federated.person_license_discipline[]\n  ) AS managed(kind, discipline)\n), upserted AS (\n  INSERT INTO federated.person_license (\n    person_id, federation, kind, discipline, grade, valid_until, status\n  )\n  SELECT\n    person_id, federation, kind, discipline, grade, valid_until, status\n  FROM source\n  ON CONFLICT (person_id, kind, discipline) DO UPDATE SET\n    grade = EXCLUDED.grade,\n    valid_until = EXCLUDED.valid_until,\n    status = EXCLUDED.status\n  WHERE\n       federated.person_license.grade IS DISTINCT FROM EXCLUDED.grade\n    OR federated.person_license.valid_until IS DISTINCT FROM EXCLUDED.valid_until\n    OR federated.person_license.status IS DISTINCT FROM EXCLUDED.status\n  RETURNING 1\n)\nDELETE FROM federated.person_license t\nWHERE t.federation = :scopeFederation\n  AND (\n    EXISTS (SELECT 1 FROM scope WHERE scope.person_id = t.person_id)\n    OR (\n      NOT EXISTS (SELECT 1 FROM scope)\n      AND EXISTS (\n        SELECT 1\n        FROM managed\n        WHERE managed.kind = t.kind\n          AND managed.discipline = t.discipline\n      )\n    )\n  )\n  AND NOT EXISTS (\n    SELECT 1\n    FROM source s\n    WHERE s.person_id = t.person_id\n      AND s.kind = t.kind\n      AND s.discipline = t.discipline\n  )"};

/**
 * Query generated from SQL:
 * ```
 * WITH input AS (
 *   SELECT
 *     :scopeFederation::text AS federation,
 *     external_id, canonical_name, gender, kind, discipline,
 *     grade, valid_until, status
 *   FROM unnest(
 *     :externalId::bigint[],
 *     :canonicalName::text[],
 *     :gender::federated.gender[],
 *     :kind::federated.person_license_kind[],
 *     :discipline::federated.person_license_discipline[],
 *     :grade::text[],
 *     :validUntil::text[],
 *     :status::federated.person_license_status[]
 *   ) AS input(
 *     external_id, canonical_name, gender, kind, discipline,
 *     grade, valid_until, status
 *   )
 * ), inserted_person AS (
 *   INSERT INTO federated.person (federation, external_id, canonical_name, gender)
 *   SELECT DISTINCT federation, external_id, canonical_name, gender
 *   FROM input
 *   ON CONFLICT (id) DO NOTHING
 * ), source AS (
 *   SELECT DISTINCT ON (federation, external_id, kind, discipline)
 *     federation || ':' || external_id::text AS person_id,
 *     federation,
 *     kind,
 *     discipline,
 *     nullif(grade, '') AS grade,
 *     nullif(valid_until, '')::date AS valid_until,
 *     status
 *   FROM input
 *   ORDER BY federation, external_id, kind, discipline
 * ), scope AS (
 *   SELECT person_id
 *   FROM unnest(:scopePersonId::text[]) AS scope(person_id)
 * ), managed AS (
 *   SELECT kind, discipline
 *   FROM unnest(
 *     :managedKind::federated.person_license_kind[],
 *     :managedDiscipline::federated.person_license_discipline[]
 *   ) AS managed(kind, discipline)
 * ), upserted AS (
 *   INSERT INTO federated.person_license (
 *     person_id, federation, kind, discipline, grade, valid_until, status
 *   )
 *   SELECT
 *     person_id, federation, kind, discipline, grade, valid_until, status
 *   FROM source
 *   ON CONFLICT (person_id, kind, discipline) DO UPDATE SET
 *     grade = EXCLUDED.grade,
 *     valid_until = EXCLUDED.valid_until,
 *     status = EXCLUDED.status
 *   WHERE
 *        federated.person_license.grade IS DISTINCT FROM EXCLUDED.grade
 *     OR federated.person_license.valid_until IS DISTINCT FROM EXCLUDED.valid_until
 *     OR federated.person_license.status IS DISTINCT FROM EXCLUDED.status
 *   RETURNING 1
 * )
 * DELETE FROM federated.person_license t
 * WHERE t.federation = :scopeFederation
 *   AND (
 *     EXISTS (SELECT 1 FROM scope WHERE scope.person_id = t.person_id)
 *     OR (
 *       NOT EXISTS (SELECT 1 FROM scope)
 *       AND EXISTS (
 *         SELECT 1
 *         FROM managed
 *         WHERE managed.kind = t.kind
 *           AND managed.discipline = t.discipline
 *       )
 *     )
 *   )
 *   AND NOT EXISTS (
 *     SELECT 1
 *     FROM source s
 *     WHERE s.person_id = t.person_id
 *       AND s.kind = t.kind
 *       AND s.discipline = t.discipline
 *   )
 * ```
 */
export const mergePersonLicenses = new PreparedQuery<IMergePersonLicensesParams,IMergePersonLicensesResult>(mergePersonLicensesIR);


/** 'ReplacePersonLicensesForPerson' parameters type */
export interface IReplacePersonLicensesForPersonParams {
  canonicalName?: stringArray | null | void;
  discipline?: person_license_disciplineArray | null | void;
  externalId?: NumberOrStringArray | null | void;
  federation?: string | null | void;
  gender?: genderArray | null | void;
  grade?: stringArray | null | void;
  kind?: person_license_kindArray | null | void;
  personId?: string | null | void;
  status?: person_license_statusArray | null | void;
  validUntil?: stringArray | null | void;
}

/** 'ReplacePersonLicensesForPerson' return type */
export type IReplacePersonLicensesForPersonResult = void;

/** 'ReplacePersonLicensesForPerson' query type */
export interface IReplacePersonLicensesForPersonQuery {
  params: IReplacePersonLicensesForPersonParams;
  result: IReplacePersonLicensesForPersonResult;
}

const replacePersonLicensesForPersonIR: any = {"usedParamSet":{"federation":true,"externalId":true,"canonicalName":true,"gender":true,"kind":true,"discipline":true,"grade":true,"validUntil":true,"status":true,"personId":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":29,"b":39},{"a":1585,"b":1595}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":171,"b":181}]},{"name":"canonicalName","required":false,"transform":{"type":"scalar"},"locs":[{"a":198,"b":211}]},{"name":"gender","required":false,"transform":{"type":"scalar"},"locs":[{"a":226,"b":232}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":259,"b":263}]},{"name":"discipline","required":false,"transform":{"type":"scalar"},"locs":[{"a":303,"b":313}]},{"name":"grade","required":false,"transform":{"type":"scalar"},"locs":[{"a":359,"b":364}]},{"name":"validUntil","required":false,"transform":{"type":"scalar"},"locs":[{"a":379,"b":389}]},{"name":"status","required":false,"transform":{"type":"scalar"},"locs":[{"a":404,"b":410}]},{"name":"personId","required":false,"transform":{"type":"scalar"},"locs":[{"a":1617,"b":1625}]}],"statement":"WITH input AS (\n  SELECT\n    :federation::text AS federation,\n    external_id, canonical_name, gender, kind, discipline,\n    grade, valid_until, status\n  FROM unnest(\n    :externalId::bigint[],\n    :canonicalName::text[],\n    :gender::federated.gender[],\n    :kind::federated.person_license_kind[],\n    :discipline::federated.person_license_discipline[],\n    :grade::text[],\n    :validUntil::text[],\n    :status::federated.person_license_status[]\n  ) AS input(\n    external_id, canonical_name, gender, kind, discipline,\n    grade, valid_until, status\n  )\n), source AS (\n  SELECT DISTINCT ON (federation, external_id, kind, discipline)\n    federation || ':' || external_id::text AS person_id,\n    federation,\n    kind,\n    discipline,\n    nullif(grade, '') AS grade,\n    nullif(valid_until, '')::date AS valid_until,\n    status\n  FROM input\n  ORDER BY federation, external_id, kind, discipline\n), upserted AS (\n  INSERT INTO federated.person_license (\n    person_id, federation, kind, discipline, grade, valid_until, status\n  )\n  SELECT\n    person_id, federation, kind, discipline, grade, valid_until, status\n  FROM source\n  ON CONFLICT (person_id, kind, discipline) DO UPDATE SET\n    grade = EXCLUDED.grade,\n    valid_until = EXCLUDED.valid_until,\n    status = EXCLUDED.status\n  WHERE\n       federated.person_license.grade IS DISTINCT FROM EXCLUDED.grade\n    OR federated.person_license.valid_until IS DISTINCT FROM EXCLUDED.valid_until\n    OR federated.person_license.status IS DISTINCT FROM EXCLUDED.status\n  RETURNING 1\n)\nDELETE FROM federated.person_license t\nWHERE t.federation = :federation\n  AND t.person_id = :personId\n  AND NOT EXISTS (\n    SELECT 1\n    FROM source s\n    WHERE s.person_id = t.person_id\n      AND s.kind = t.kind\n      AND s.discipline = t.discipline\n  )"};

/**
 * Query generated from SQL:
 * ```
 * WITH input AS (
 *   SELECT
 *     :federation::text AS federation,
 *     external_id, canonical_name, gender, kind, discipline,
 *     grade, valid_until, status
 *   FROM unnest(
 *     :externalId::bigint[],
 *     :canonicalName::text[],
 *     :gender::federated.gender[],
 *     :kind::federated.person_license_kind[],
 *     :discipline::federated.person_license_discipline[],
 *     :grade::text[],
 *     :validUntil::text[],
 *     :status::federated.person_license_status[]
 *   ) AS input(
 *     external_id, canonical_name, gender, kind, discipline,
 *     grade, valid_until, status
 *   )
 * ), source AS (
 *   SELECT DISTINCT ON (federation, external_id, kind, discipline)
 *     federation || ':' || external_id::text AS person_id,
 *     federation,
 *     kind,
 *     discipline,
 *     nullif(grade, '') AS grade,
 *     nullif(valid_until, '')::date AS valid_until,
 *     status
 *   FROM input
 *   ORDER BY federation, external_id, kind, discipline
 * ), upserted AS (
 *   INSERT INTO federated.person_license (
 *     person_id, federation, kind, discipline, grade, valid_until, status
 *   )
 *   SELECT
 *     person_id, federation, kind, discipline, grade, valid_until, status
 *   FROM source
 *   ON CONFLICT (person_id, kind, discipline) DO UPDATE SET
 *     grade = EXCLUDED.grade,
 *     valid_until = EXCLUDED.valid_until,
 *     status = EXCLUDED.status
 *   WHERE
 *        federated.person_license.grade IS DISTINCT FROM EXCLUDED.grade
 *     OR federated.person_license.valid_until IS DISTINCT FROM EXCLUDED.valid_until
 *     OR federated.person_license.status IS DISTINCT FROM EXCLUDED.status
 *   RETURNING 1
 * )
 * DELETE FROM federated.person_license t
 * WHERE t.federation = :federation
 *   AND t.person_id = :personId
 *   AND NOT EXISTS (
 *     SELECT 1
 *     FROM source s
 *     WHERE s.person_id = t.person_id
 *       AND s.kind = t.kind
 *       AND s.discipline = t.discipline
 *   )
 * ```
 */
export const replacePersonLicensesForPerson = new PreparedQuery<IReplacePersonLicensesForPersonParams,IReplacePersonLicensesForPersonResult>(replacePersonLicensesForPersonIR);


/** 'UpsertPeopleDetailed' parameters type */
export interface IUpsertPeopleDetailedParams {
  ageGroup?: stringArray | null | void;
  canonicalName?: stringArray | null | void;
  externalId?: NumberOrStringArray | null | void;
  federation?: stringArray | null | void;
  firstName?: stringArray | null | void;
  gender?: genderArray | null | void;
  lastName?: stringArray | null | void;
  medicalCheckupExpiration?: stringArray | null | void;
  nationality?: stringArray | null | void;
}

/** 'UpsertPeopleDetailed' return type */
export type IUpsertPeopleDetailedResult = void;

/** 'UpsertPeopleDetailed' query type */
export interface IUpsertPeopleDetailedQuery {
  params: IUpsertPeopleDetailedParams;
  result: IUpsertPeopleDetailedResult;
}

const upsertPeopleDetailedIR: any = {"usedParamSet":{"federation":true,"externalId":true,"canonicalName":true,"firstName":true,"lastName":true,"gender":true,"nationality":true,"ageGroup":true,"medicalCheckupExpiration":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":407,"b":417}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":430,"b":440}]},{"name":"canonicalName","required":false,"transform":{"type":"scalar"},"locs":[{"a":455,"b":468}]},{"name":"firstName","required":false,"transform":{"type":"scalar"},"locs":[{"a":481,"b":490}]},{"name":"lastName","required":false,"transform":{"type":"scalar"},"locs":[{"a":503,"b":511}]},{"name":"gender","required":false,"transform":{"type":"scalar"},"locs":[{"a":524,"b":530}]},{"name":"nationality","required":false,"transform":{"type":"scalar"},"locs":[{"a":555,"b":566}]},{"name":"ageGroup","required":false,"transform":{"type":"scalar"},"locs":[{"a":579,"b":587}]},{"name":"medicalCheckupExpiration","required":false,"transform":{"type":"scalar"},"locs":[{"a":600,"b":624}]}],"statement":"INSERT INTO federated.person (\n  federation, external_id, canonical_name, first_name, last_name, gender, nationality, age_group, medical_checkup_expiration\n)\nSELECT\n  federation,\n  external_id,\n  nullif(canonical_name, ''),\n  nullif(first_name, ''),\n  nullif(last_name, ''),\n  gender,\n  nullif(nationality, ''),\n  nullif(age_group, ''),\n  CAST(NULLIF(medical_checkup_expiration, '') AS date)\nFROM unnest(\n  :federation::text[],\n  :externalId::bigint[],\n  :canonicalName::text[],\n  :firstName::text[],\n  :lastName::text[],\n  :gender::federated.gender[],\n  :nationality::text[],\n  :ageGroup::text[],\n  :medicalCheckupExpiration::text[]\n) AS input(\n  federation, external_id, canonical_name, first_name, last_name, gender, nationality, age_group, medical_checkup_expiration)\nON CONFLICT (id)\n  DO UPDATE SET\n    canonical_name = COALESCE(EXCLUDED.canonical_name, federated.person.canonical_name),\n    first_name = EXCLUDED.first_name,\n    last_name = EXCLUDED.last_name,\n    gender = CASE\n      WHEN EXCLUDED.gender IS NOT NULL AND EXCLUDED.gender <> 'unknown'\n      THEN EXCLUDED.gender\n      ELSE federated.person.gender\n    END,\n    nationality = EXCLUDED.nationality,\n    age_group = EXCLUDED.age_group,\n    medical_checkup_expiration = EXCLUDED.medical_checkup_expiration\n  WHERE federated.person.canonical_name IS DISTINCT FROM COALESCE(EXCLUDED.canonical_name, federated.person.canonical_name)\n     OR federated.person.first_name IS DISTINCT FROM EXCLUDED.first_name\n     OR federated.person.last_name IS DISTINCT FROM EXCLUDED.last_name\n     OR federated.person.gender IS DISTINCT FROM CASE\n          WHEN EXCLUDED.gender IS NOT NULL AND EXCLUDED.gender <> 'unknown'\n          THEN EXCLUDED.gender\n          ELSE federated.person.gender\n        END\n     OR federated.person.nationality IS DISTINCT FROM EXCLUDED.nationality\n     OR federated.person.age_group IS DISTINCT FROM EXCLUDED.age_group\n     OR federated.person.medical_checkup_expiration IS DISTINCT FROM EXCLUDED.medical_checkup_expiration"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO federated.person (
 *   federation, external_id, canonical_name, first_name, last_name, gender, nationality, age_group, medical_checkup_expiration
 * )
 * SELECT
 *   federation,
 *   external_id,
 *   nullif(canonical_name, ''),
 *   nullif(first_name, ''),
 *   nullif(last_name, ''),
 *   gender,
 *   nullif(nationality, ''),
 *   nullif(age_group, ''),
 *   CAST(NULLIF(medical_checkup_expiration, '') AS date)
 * FROM unnest(
 *   :federation::text[],
 *   :externalId::bigint[],
 *   :canonicalName::text[],
 *   :firstName::text[],
 *   :lastName::text[],
 *   :gender::federated.gender[],
 *   :nationality::text[],
 *   :ageGroup::text[],
 *   :medicalCheckupExpiration::text[]
 * ) AS input(
 *   federation, external_id, canonical_name, first_name, last_name, gender, nationality, age_group, medical_checkup_expiration)
 * ON CONFLICT (id)
 *   DO UPDATE SET
 *     canonical_name = COALESCE(EXCLUDED.canonical_name, federated.person.canonical_name),
 *     first_name = EXCLUDED.first_name,
 *     last_name = EXCLUDED.last_name,
 *     gender = CASE
 *       WHEN EXCLUDED.gender IS NOT NULL AND EXCLUDED.gender <> 'unknown'
 *       THEN EXCLUDED.gender
 *       ELSE federated.person.gender
 *     END,
 *     nationality = EXCLUDED.nationality,
 *     age_group = EXCLUDED.age_group,
 *     medical_checkup_expiration = EXCLUDED.medical_checkup_expiration
 *   WHERE federated.person.canonical_name IS DISTINCT FROM COALESCE(EXCLUDED.canonical_name, federated.person.canonical_name)
 *      OR federated.person.first_name IS DISTINCT FROM EXCLUDED.first_name
 *      OR federated.person.last_name IS DISTINCT FROM EXCLUDED.last_name
 *      OR federated.person.gender IS DISTINCT FROM CASE
 *           WHEN EXCLUDED.gender IS NOT NULL AND EXCLUDED.gender <> 'unknown'
 *           THEN EXCLUDED.gender
 *           ELSE federated.person.gender
 *         END
 *      OR federated.person.nationality IS DISTINCT FROM EXCLUDED.nationality
 *      OR federated.person.age_group IS DISTINCT FROM EXCLUDED.age_group
 *      OR federated.person.medical_checkup_expiration IS DISTINCT FROM EXCLUDED.medical_checkup_expiration
 * ```
 */
export const upsertPeopleDetailed = new PreparedQuery<IUpsertPeopleDetailedParams,IUpsertPeopleDetailedResult>(upsertPeopleDetailedIR);


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


/** 'UpsertCategories' parameters type */
export interface IUpsertCategoriesParams {
  ageGroup?: stringArray | null | void;
  class?: stringArray | null | void;
  competitorType?: competitor_typeArray | null | void;
  discipline?: stringArray | null | void;
  genderGroup?: stringArray | null | void;
  series?: stringArray | null | void;
}

/** 'UpsertCategories' return type */
export interface IUpsertCategoriesResult {
  ageGroup: string;
  class: string;
  competitorType: competitor_type;
  discipline: string;
  genderGroup: string;
  id: string;
  series: string;
}

/** 'UpsertCategories' query type */
export interface IUpsertCategoriesQuery {
  params: IUpsertCategoriesParams;
  result: IUpsertCategoriesResult;
}

const upsertCategoriesIR: any = {"usedParamSet":{"series":true,"discipline":true,"ageGroup":true,"genderGroup":true,"class":true,"competitorType":true},"params":[{"name":"series","required":false,"transform":{"type":"scalar"},"locs":[{"a":112,"b":118}]},{"name":"discipline","required":false,"transform":{"type":"scalar"},"locs":[{"a":133,"b":143}]},{"name":"ageGroup","required":false,"transform":{"type":"scalar"},"locs":[{"a":158,"b":166}]},{"name":"genderGroup","required":false,"transform":{"type":"scalar"},"locs":[{"a":181,"b":192}]},{"name":"class","required":false,"transform":{"type":"scalar"},"locs":[{"a":207,"b":212}]},{"name":"competitorType","required":false,"transform":{"type":"scalar"},"locs":[{"a":227,"b":241}]}],"statement":"WITH input AS (\n  SELECT series, discipline, age_group, gender_group, class, competitor_type\n  FROM unnest(\n    :series::text[],\n    :discipline::text[],\n    :ageGroup::text[],\n    :genderGroup::text[],\n    :class::text[],\n    :competitorType::federated.competitor_type[]\n  ) AS input(series, discipline, age_group, gender_group, class, competitor_type)\n), inserted AS (\n  INSERT INTO federated.category (series, discipline, age_group, gender_group, class, competitor_type, name)\n  SELECT *, concat_ws(' ', series, age_group, nullif(competitor_type, 'couple'), nullif(class, ''), discipline)\n  FROM input\n  ON CONFLICT (series, discipline, age_group, gender_group, class, competitor_type) DO NOTHING\n  RETURNING *\n), result AS (\n  SELECT * FROM inserted\n  UNION\n  SELECT c.* FROM federated.category c\n  JOIN input i USING (series, discipline, age_group, gender_group, class, competitor_type)\n)\nSELECT\n  id AS \"id!\",\n  class AS \"class!\",\n  series AS \"series!\",\n  discipline AS \"discipline!\",\n  age_group AS \"ageGroup!\",\n  gender_group AS \"genderGroup!\",\n  competitor_type AS \"competitorType!\"\nFROM result"};

/**
 * Query generated from SQL:
 * ```
 * WITH input AS (
 *   SELECT series, discipline, age_group, gender_group, class, competitor_type
 *   FROM unnest(
 *     :series::text[],
 *     :discipline::text[],
 *     :ageGroup::text[],
 *     :genderGroup::text[],
 *     :class::text[],
 *     :competitorType::federated.competitor_type[]
 *   ) AS input(series, discipline, age_group, gender_group, class, competitor_type)
 * ), inserted AS (
 *   INSERT INTO federated.category (series, discipline, age_group, gender_group, class, competitor_type, name)
 *   SELECT *, concat_ws(' ', series, age_group, nullif(competitor_type, 'couple'), nullif(class, ''), discipline)
 *   FROM input
 *   ON CONFLICT (series, discipline, age_group, gender_group, class, competitor_type) DO NOTHING
 *   RETURNING *
 * ), result AS (
 *   SELECT * FROM inserted
 *   UNION
 *   SELECT c.* FROM federated.category c
 *   JOIN input i USING (series, discipline, age_group, gender_group, class, competitor_type)
 * )
 * SELECT
 *   id AS "id!",
 *   class AS "class!",
 *   series AS "series!",
 *   discipline AS "discipline!",
 *   age_group AS "ageGroup!",
 *   gender_group AS "genderGroup!",
 *   competitor_type AS "competitorType!"
 * FROM result
 * ```
 */
export const upsertCategories = new PreparedQuery<IUpsertCategoriesParams,IUpsertCategoriesResult>(upsertCategoriesIR);


/** 'EnsureCompetitors' parameters type */
export interface IEnsureCompetitorsParams {
  externalId?: NumberOrStringArray | null | void;
  federation?: stringArray | null | void;
  label?: stringArray | null | void;
  type?: competitor_typeArray | null | void;
}

/** 'EnsureCompetitors' return type */
export type IEnsureCompetitorsResult = void;

/** 'EnsureCompetitors' query type */
export interface IEnsureCompetitorsQuery {
  params: IEnsureCompetitorsParams;
  result: IEnsureCompetitorsResult;
}

const ensureCompetitorsIR: any = {"usedParamSet":{"federation":true,"externalId":true,"type":true,"label":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":175,"b":185}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":198,"b":208}]},{"name":"type","required":false,"transform":{"type":"scalar"},"locs":[{"a":223,"b":227}]},{"name":"label","required":false,"transform":{"type":"scalar"},"locs":[{"a":261,"b":266}]}],"statement":"INSERT INTO federated.competitor (federation, external_id, competitor_type, name)\nSELECT input.federation, input.external_id, input.competitor_type, input.name\nFROM unnest(\n  :federation::text[],\n  :externalId::bigint[],\n  :type::federated.competitor_type[],\n  :label::text[]\n) AS input(federation, external_id, competitor_type, name)\nON CONFLICT (id) DO NOTHING"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO federated.competitor (federation, external_id, competitor_type, name)
 * SELECT input.federation, input.external_id, input.competitor_type, input.name
 * FROM unnest(
 *   :federation::text[],
 *   :externalId::bigint[],
 *   :type::federated.competitor_type[],
 *   :label::text[]
 * ) AS input(federation, external_id, competitor_type, name)
 * ON CONFLICT (id) DO NOTHING
 * ```
 */
export const ensureCompetitors = new PreparedQuery<IEnsureCompetitorsParams,IEnsureCompetitorsResult>(ensureCompetitorsIR);


/** 'EnsureCompetitorsWithComponents' parameters type */
export interface IEnsureCompetitorsWithComponentsParams {
  componentCompetitorId?: stringArray | null | void;
  componentRole?: competitor_roleArray | null | void;
  externalId?: NumberOrStringArray | null | void;
  federation?: stringArray | null | void;
  label?: stringArray | null | void;
  personCanonicalName?: stringArray | null | void;
  personExternalId?: NumberOrStringArray | null | void;
  personFederation?: stringArray | null | void;
  personGender?: genderArray | null | void;
  personId?: stringArray | null | void;
  type?: competitor_typeArray | null | void;
}

/** 'EnsureCompetitorsWithComponents' return type */
export type IEnsureCompetitorsWithComponentsResult = void;

/** 'EnsureCompetitorsWithComponents' query type */
export interface IEnsureCompetitorsWithComponentsQuery {
  params: IEnsureCompetitorsWithComponentsParams;
  result: IEnsureCompetitorsWithComponentsResult;
}

const ensureCompetitorsWithComponentsIR: any = {"usedParamSet":{"federation":true,"externalId":true,"type":true,"label":true,"componentCompetitorId":true,"personId":true,"personFederation":true,"personExternalId":true,"personCanonicalName":true,"personGender":true,"componentRole":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":148,"b":158}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":173,"b":183}]},{"name":"type","required":false,"transform":{"type":"scalar"},"locs":[{"a":200,"b":204}]},{"name":"label","required":false,"transform":{"type":"scalar"},"locs":[{"a":240,"b":245}]},{"name":"componentCompetitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":726,"b":747}]},{"name":"personId","required":false,"transform":{"type":"scalar"},"locs":[{"a":762,"b":770}]},{"name":"personFederation","required":false,"transform":{"type":"scalar"},"locs":[{"a":785,"b":801}]},{"name":"personExternalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":816,"b":832}]},{"name":"personCanonicalName","required":false,"transform":{"type":"scalar"},"locs":[{"a":849,"b":868}]},{"name":"personGender","required":false,"transform":{"type":"scalar"},"locs":[{"a":883,"b":895}]},{"name":"componentRole","required":false,"transform":{"type":"scalar"},"locs":[{"a":922,"b":935}]}],"statement":"WITH competitor_input AS (\n  SELECT federation, external_id, federation || ':' || external_id::text AS id, competitor_type, name\n  FROM unnest(\n    :federation::text[],\n    :externalId::bigint[],\n    :type::federated.competitor_type[],\n    :label::text[]\n  ) AS input(federation, external_id, competitor_type, name)\n), inserted_competitor AS (\n  INSERT INTO federated.competitor (federation, external_id, competitor_type, name)\n  SELECT federation, external_id, competitor_type, name\n  FROM competitor_input\n  ON CONFLICT (id) DO NOTHING\n), component_input AS (\n  SELECT\n    component_competitor_id, person_id, person_federation, person_external_id,\n    person_canonical_name, person_gender, component_role\n  FROM unnest(\n    :componentCompetitorId::text[],\n    :personId::text[],\n    :personFederation::text[],\n    :personExternalId::bigint[],\n    :personCanonicalName::text[],\n    :personGender::federated.gender[],\n    :componentRole::federated.competitor_role[]\n  ) AS input(\n    component_competitor_id, person_id, person_federation, person_external_id,\n    person_canonical_name, person_gender, component_role\n  )\n), target_component AS (\n  SELECT component_input.*\n  FROM component_input\n  JOIN competitor_input ON competitor_input.id = component_input.component_competitor_id\n  WHERE NOT EXISTS (\n    SELECT 1\n    FROM federated.competitor_component component\n    WHERE component.competitor_id = component_input.component_competitor_id\n  )\n), inserted_person AS (\n  INSERT INTO federated.person (federation, external_id, canonical_name, gender)\n  SELECT DISTINCT person_federation, person_external_id, person_canonical_name, person_gender\n  FROM target_component\n  ON CONFLICT (id) DO NOTHING\n)\nINSERT INTO federated.competitor_component (competitor_id, person_id, role)\nSELECT component_competitor_id, person_id, component_role\nFROM target_component\nON CONFLICT (competitor_id, person_id) DO NOTHING"};

/**
 * Query generated from SQL:
 * ```
 * WITH competitor_input AS (
 *   SELECT federation, external_id, federation || ':' || external_id::text AS id, competitor_type, name
 *   FROM unnest(
 *     :federation::text[],
 *     :externalId::bigint[],
 *     :type::federated.competitor_type[],
 *     :label::text[]
 *   ) AS input(federation, external_id, competitor_type, name)
 * ), inserted_competitor AS (
 *   INSERT INTO federated.competitor (federation, external_id, competitor_type, name)
 *   SELECT federation, external_id, competitor_type, name
 *   FROM competitor_input
 *   ON CONFLICT (id) DO NOTHING
 * ), component_input AS (
 *   SELECT
 *     component_competitor_id, person_id, person_federation, person_external_id,
 *     person_canonical_name, person_gender, component_role
 *   FROM unnest(
 *     :componentCompetitorId::text[],
 *     :personId::text[],
 *     :personFederation::text[],
 *     :personExternalId::bigint[],
 *     :personCanonicalName::text[],
 *     :personGender::federated.gender[],
 *     :componentRole::federated.competitor_role[]
 *   ) AS input(
 *     component_competitor_id, person_id, person_federation, person_external_id,
 *     person_canonical_name, person_gender, component_role
 *   )
 * ), target_component AS (
 *   SELECT component_input.*
 *   FROM component_input
 *   JOIN competitor_input ON competitor_input.id = component_input.component_competitor_id
 *   WHERE NOT EXISTS (
 *     SELECT 1
 *     FROM federated.competitor_component component
 *     WHERE component.competitor_id = component_input.component_competitor_id
 *   )
 * ), inserted_person AS (
 *   INSERT INTO federated.person (federation, external_id, canonical_name, gender)
 *   SELECT DISTINCT person_federation, person_external_id, person_canonical_name, person_gender
 *   FROM target_component
 *   ON CONFLICT (id) DO NOTHING
 * )
 * INSERT INTO federated.competitor_component (competitor_id, person_id, role)
 * SELECT component_competitor_id, person_id, component_role
 * FROM target_component
 * ON CONFLICT (competitor_id, person_id) DO NOTHING
 * ```
 */
export const ensureCompetitorsWithComponents = new PreparedQuery<IEnsureCompetitorsWithComponentsParams,IEnsureCompetitorsWithComponentsResult>(ensureCompetitorsWithComponentsIR);


/** 'UpsertCompetitorsDetailed' parameters type */
export interface IUpsertCompetitorsDetailedParams {
  externalId?: NumberOrStringArray | null | void;
  federation?: stringArray | null | void;
  label?: stringArray | null | void;
  type?: competitor_typeArray | null | void;
}

/** 'UpsertCompetitorsDetailed' return type */
export type IUpsertCompetitorsDetailedResult = void;

/** 'UpsertCompetitorsDetailed' query type */
export interface IUpsertCompetitorsDetailedQuery {
  params: IUpsertCompetitorsDetailedParams;
  result: IUpsertCompetitorsDetailedResult;
}

const upsertCompetitorsDetailedIR: any = {"usedParamSet":{"federation":true,"externalId":true,"type":true,"label":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":175,"b":185}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":198,"b":208}]},{"name":"type","required":false,"transform":{"type":"scalar"},"locs":[{"a":223,"b":227}]},{"name":"label","required":false,"transform":{"type":"scalar"},"locs":[{"a":261,"b":266}]}],"statement":"INSERT INTO federated.competitor (federation, external_id, competitor_type, name)\nSELECT input.federation, input.external_id, input.competitor_type, input.name\nFROM unnest(\n  :federation::text[],\n  :externalId::bigint[],\n  :type::federated.competitor_type[],\n  :label::text[]\n) AS input(federation, external_id, competitor_type, name)\nON CONFLICT (id) DO UPDATE\n  SET competitor_type = EXCLUDED.competitor_type,\n      name = CASE\n        WHEN EXCLUDED.name <> '' THEN EXCLUDED.name\n        ELSE federated.competitor.name\n      END\n  WHERE federated.competitor.competitor_type IS DISTINCT FROM EXCLUDED.competitor_type\n     OR (\n       EXCLUDED.name <> ''\n       AND federated.competitor.name IS DISTINCT FROM EXCLUDED.name\n     )"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO federated.competitor (federation, external_id, competitor_type, name)
 * SELECT input.federation, input.external_id, input.competitor_type, input.name
 * FROM unnest(
 *   :federation::text[],
 *   :externalId::bigint[],
 *   :type::federated.competitor_type[],
 *   :label::text[]
 * ) AS input(federation, external_id, competitor_type, name)
 * ON CONFLICT (id) DO UPDATE
 *   SET competitor_type = EXCLUDED.competitor_type,
 *       name = CASE
 *         WHEN EXCLUDED.name <> '' THEN EXCLUDED.name
 *         ELSE federated.competitor.name
 *       END
 *   WHERE federated.competitor.competitor_type IS DISTINCT FROM EXCLUDED.competitor_type
 *      OR (
 *        EXCLUDED.name <> ''
 *        AND federated.competitor.name IS DISTINCT FROM EXCLUDED.name
 *      )
 * ```
 */
export const upsertCompetitorsDetailed = new PreparedQuery<IUpsertCompetitorsDetailedParams,IUpsertCompetitorsDetailedResult>(upsertCompetitorsDetailedIR);


/** 'MergeCompetitorComponents' parameters type */
export interface IMergeCompetitorComponentsParams {
  competitorId?: stringArray | null | void;
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

const mergeCompetitorComponentsIR: any = {"usedParamSet":{"competitorId":true,"personId":true,"role":true},"params":[{"name":"competitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":76,"b":88},{"a":266,"b":278}]},{"name":"personId","required":false,"transform":{"type":"scalar"},"locs":[{"a":103,"b":111}]},{"name":"role","required":false,"transform":{"type":"scalar"},"locs":[{"a":126,"b":130}]}],"statement":"WITH source AS (\n  SELECT competitor_id, person_id, role\n  FROM unnest(\n    :competitorId::text[],\n    :personId::text[],\n    :role::federated.competitor_role[]\n  ) AS input(competitor_id, person_id, role)\n), scope AS (\n  SELECT DISTINCT competitor_id\n  FROM unnest(:competitorId::text[]) AS input(competitor_id)\n), upserted AS (\n  INSERT INTO federated.competitor_component (competitor_id, person_id, role)\n  SELECT s.competitor_id, s.person_id, s.role\n  FROM source s\n  ON CONFLICT (competitor_id, person_id) DO UPDATE\n    SET role = EXCLUDED.role\n    WHERE federated.competitor_component.role IS DISTINCT FROM EXCLUDED.role\n)\nDELETE FROM federated.competitor_component t\nWHERE t.competitor_id IN (SELECT competitor_id FROM scope)\n  AND NOT EXISTS (\n    SELECT 1\n    FROM source s\n    WHERE s.competitor_id = t.competitor_id\n      AND s.person_id = t.person_id\n  )"};

/**
 * Query generated from SQL:
 * ```
 * WITH source AS (
 *   SELECT competitor_id, person_id, role
 *   FROM unnest(
 *     :competitorId::text[],
 *     :personId::text[],
 *     :role::federated.competitor_role[]
 *   ) AS input(competitor_id, person_id, role)
 * ), scope AS (
 *   SELECT DISTINCT competitor_id
 *   FROM unnest(:competitorId::text[]) AS input(competitor_id)
 * ), upserted AS (
 *   INSERT INTO federated.competitor_component (competitor_id, person_id, role)
 *   SELECT s.competitor_id, s.person_id, s.role
 *   FROM source s
 *   ON CONFLICT (competitor_id, person_id) DO UPDATE
 *     SET role = EXCLUDED.role
 *     WHERE federated.competitor_component.role IS DISTINCT FROM EXCLUDED.role
 * )
 * DELETE FROM federated.competitor_component t
 * WHERE t.competitor_id IN (SELECT competitor_id FROM scope)
 *   AND NOT EXISTS (
 *     SELECT 1
 *     FROM source s
 *     WHERE s.competitor_id = t.competitor_id
 *       AND s.person_id = t.person_id
 *   )
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

const mergeCompetitorProgressIR: any = {"usedParamSet":{"competitorId":true,"categoryId":true,"points":true,"domesticFinals":true,"foreignFinals":true},"params":[{"name":"competitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":147,"b":159},{"a":421,"b":433}]},{"name":"categoryId","required":false,"transform":{"type":"scalar"},"locs":[{"a":174,"b":184}]},{"name":"points","required":false,"transform":{"type":"scalar"},"locs":[{"a":201,"b":207}]},{"name":"domesticFinals","required":false,"transform":{"type":"scalar"},"locs":[{"a":231,"b":245}]},{"name":"foreignFinals","required":false,"transform":{"type":"scalar"},"locs":[{"a":259,"b":272}]}],"statement":"WITH source AS (\n  SELECT\n    entry.competitor_id, entry.category_id, entry.points, entry.domestic_finals, entry.foreign_finals\n  FROM unnest(\n    :competitorId::text[],\n    :categoryId::bigint[],\n    :points::numeric(10,3)[],\n    :domesticFinals::int[],\n    :foreignFinals::int[]\n  ) entry (competitor_id, category_id, points, domestic_finals, foreign_finals)\n), scope AS (\n  SELECT DISTINCT competitor_id\n  FROM unnest(:competitorId::text[]) AS input(competitor_id)\n), upserted AS (\n  INSERT INTO federated.competitor_category_progress (\n    competitor_id, category_id, points, domestic_finals, foreign_finals\n  )\n  SELECT s.competitor_id, s.category_id, s.points, s.domestic_finals, s.foreign_finals\n  FROM source s\n  ON CONFLICT (competitor_id, category_id) DO UPDATE\n    SET points = EXCLUDED.points,\n        domestic_finals = EXCLUDED.domestic_finals,\n        foreign_finals = EXCLUDED.foreign_finals\n    WHERE federated.competitor_category_progress.points IS DISTINCT FROM EXCLUDED.points\n       OR federated.competitor_category_progress.domestic_finals IS DISTINCT FROM EXCLUDED.domestic_finals\n       OR federated.competitor_category_progress.foreign_finals IS DISTINCT FROM EXCLUDED.foreign_finals\n)\nDELETE FROM federated.competitor_category_progress t\nWHERE t.competitor_id IN (SELECT competitor_id FROM scope)\n  AND NOT EXISTS (\n    SELECT 1\n    FROM source s\n    WHERE s.competitor_id = t.competitor_id\n      AND s.category_id = t.category_id\n  )"};

/**
 * Query generated from SQL:
 * ```
 * WITH source AS (
 *   SELECT
 *     entry.competitor_id, entry.category_id, entry.points, entry.domestic_finals, entry.foreign_finals
 *   FROM unnest(
 *     :competitorId::text[],
 *     :categoryId::bigint[],
 *     :points::numeric(10,3)[],
 *     :domesticFinals::int[],
 *     :foreignFinals::int[]
 *   ) entry (competitor_id, category_id, points, domestic_finals, foreign_finals)
 * ), scope AS (
 *   SELECT DISTINCT competitor_id
 *   FROM unnest(:competitorId::text[]) AS input(competitor_id)
 * ), upserted AS (
 *   INSERT INTO federated.competitor_category_progress (
 *     competitor_id, category_id, points, domestic_finals, foreign_finals
 *   )
 *   SELECT s.competitor_id, s.category_id, s.points, s.domestic_finals, s.foreign_finals
 *   FROM source s
 *   ON CONFLICT (competitor_id, category_id) DO UPDATE
 *     SET points = EXCLUDED.points,
 *         domestic_finals = EXCLUDED.domestic_finals,
 *         foreign_finals = EXCLUDED.foreign_finals
 *     WHERE federated.competitor_category_progress.points IS DISTINCT FROM EXCLUDED.points
 *        OR federated.competitor_category_progress.domestic_finals IS DISTINCT FROM EXCLUDED.domestic_finals
 *        OR federated.competitor_category_progress.foreign_finals IS DISTINCT FROM EXCLUDED.foreign_finals
 * )
 * DELETE FROM federated.competitor_category_progress t
 * WHERE t.competitor_id IN (SELECT competitor_id FROM scope)
 *   AND NOT EXISTS (
 *     SELECT 1
 *     FROM source s
 *     WHERE s.competitor_id = t.competitor_id
 *       AND s.category_id = t.category_id
 *   )
 * ```
 */
export const mergeCompetitorProgress = new PreparedQuery<IMergeCompetitorProgressParams,IMergeCompetitorProgressResult>(mergeCompetitorProgressIR);


/** 'UpsertRanklistSnapshot' parameters type */
export interface IUpsertRanklistSnapshotParams {
  asOfDate?: DateOrString | null | void;
  categoryId?: NumberOrString | null | void;
  entryCompetitorId?: stringArray | null | void;
  entryPoints?: NumberOrStringArray | null | void;
  entryRanking?: numberArray | null | void;
  entryRankingTo?: numberArray | null | void;
  federation?: string | null | void;
  kind?: string | null | void;
  ranklistName?: string | null | void;
}

/** 'UpsertRanklistSnapshot' return type */
export type IUpsertRanklistSnapshotResult = void;

/** 'UpsertRanklistSnapshot' query type */
export interface IUpsertRanklistSnapshotQuery {
  params: IUpsertRanklistSnapshotParams;
  result: IUpsertRanklistSnapshotResult;
}

const upsertRanklistSnapshotIR: any = {"usedParamSet":{"federation":true,"categoryId":true,"ranklistName":true,"asOfDate":true,"kind":true,"entryCompetitorId":true,"entryRanking":true,"entryRankingTo":true,"entryPoints":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":101,"b":111}]},{"name":"categoryId","required":false,"transform":{"type":"scalar"},"locs":[{"a":114,"b":124}]},{"name":"ranklistName","required":false,"transform":{"type":"scalar"},"locs":[{"a":127,"b":139}]},{"name":"asOfDate","required":false,"transform":{"type":"scalar"},"locs":[{"a":347,"b":355}]},{"name":"kind","required":false,"transform":{"type":"scalar"},"locs":[{"a":367,"b":371}]},{"name":"entryCompetitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":654,"b":671}]},{"name":"entryRanking","required":false,"transform":{"type":"scalar"},"locs":[{"a":686,"b":698}]},{"name":"entryRankingTo","required":false,"transform":{"type":"scalar"},"locs":[{"a":712,"b":726}]},{"name":"entryPoints","required":false,"transform":{"type":"scalar"},"locs":[{"a":740,"b":751}]}],"statement":"WITH upsert_ranklist AS (\n  INSERT INTO federated.ranklist (federation, category_id, name)\n  VALUES (:federation, :categoryId, :ranklistName)\n  ON CONFLICT (federation, category_id)\n    DO UPDATE SET name = EXCLUDED.name\n  RETURNING id\n),\nupsert_snapshot AS (\n  INSERT INTO federated.ranklist_snapshot (ranklist_id, as_of_date, kind)\n  SELECT id, :asOfDate, COALESCE(:kind, 'default')\n  FROM upsert_ranklist\n  ON CONFLICT (ranklist_id, as_of_date, kind)\n    DO UPDATE SET kind = EXCLUDED.kind\n  RETURNING id\n),\nsource AS (\n  SELECT s.id AS snapshot_id, e.competitor_id, e.ranking, e.ranking_to, e.points\n  FROM upsert_snapshot s\n  CROSS JOIN unnest(\n    :entryCompetitorId::text[],\n    :entryRanking::int[],\n    :entryRankingTo::int[],\n    :entryPoints::numeric(10,3)[]\n  ) AS e(competitor_id, ranking, ranking_to, points)\n), upsert_entry AS (\n  INSERT INTO federated.ranklist_entry (snapshot_id, competitor_id, ranking, ranking_to, points)\n  SELECT s.snapshot_id, s.competitor_id, s.ranking, s.ranking_to, s.points\n  FROM source s\n  ON CONFLICT (snapshot_id, competitor_id) DO UPDATE\n    SET ranking = EXCLUDED.ranking,\n        ranking_to = EXCLUDED.ranking_to,\n        points = EXCLUDED.points\n    WHERE federated.ranklist_entry.ranking IS DISTINCT FROM EXCLUDED.ranking\n       OR federated.ranklist_entry.ranking_to IS DISTINCT FROM EXCLUDED.ranking_to\n       OR federated.ranklist_entry.points IS DISTINCT FROM EXCLUDED.points\n)\nDELETE FROM federated.ranklist_entry t\nWHERE t.snapshot_id = (SELECT id FROM upsert_snapshot)\n  AND NOT EXISTS (\n    SELECT 1\n    FROM source s\n    WHERE s.snapshot_id = t.snapshot_id\n      AND s.competitor_id = t.competitor_id\n  )"};

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
 * source AS (
 *   SELECT s.id AS snapshot_id, e.competitor_id, e.ranking, e.ranking_to, e.points
 *   FROM upsert_snapshot s
 *   CROSS JOIN unnest(
 *     :entryCompetitorId::text[],
 *     :entryRanking::int[],
 *     :entryRankingTo::int[],
 *     :entryPoints::numeric(10,3)[]
 *   ) AS e(competitor_id, ranking, ranking_to, points)
 * ), upsert_entry AS (
 *   INSERT INTO federated.ranklist_entry (snapshot_id, competitor_id, ranking, ranking_to, points)
 *   SELECT s.snapshot_id, s.competitor_id, s.ranking, s.ranking_to, s.points
 *   FROM source s
 *   ON CONFLICT (snapshot_id, competitor_id) DO UPDATE
 *     SET ranking = EXCLUDED.ranking,
 *         ranking_to = EXCLUDED.ranking_to,
 *         points = EXCLUDED.points
 *     WHERE federated.ranklist_entry.ranking IS DISTINCT FROM EXCLUDED.ranking
 *        OR federated.ranklist_entry.ranking_to IS DISTINCT FROM EXCLUDED.ranking_to
 *        OR federated.ranklist_entry.points IS DISTINCT FROM EXCLUDED.points
 * )
 * DELETE FROM federated.ranklist_entry t
 * WHERE t.snapshot_id = (SELECT id FROM upsert_snapshot)
 *   AND NOT EXISTS (
 *     SELECT 1
 *     FROM source s
 *     WHERE s.snapshot_id = t.snapshot_id
 *       AND s.competitor_id = t.competitor_id
 *   )
 * ```
 */
export const upsertRanklistSnapshot = new PreparedQuery<IUpsertRanklistSnapshotParams,IUpsertRanklistSnapshotResult>(upsertRanklistSnapshotIR);


/** 'UpsertFederationClubs' parameters type */
export interface IUpsertFederationClubsParams {
  city?: stringArray | null | void;
  country?: stringArray | null | void;
  externalId?: stringArray | null | void;
  federation?: stringArray | null | void;
  name?: stringArray | null | void;
}

/** 'UpsertFederationClubs' return type */
export type IUpsertFederationClubsResult = void;

/** 'UpsertFederationClubs' query type */
export interface IUpsertFederationClubsQuery {
  params: IUpsertFederationClubsParams;
  result: IUpsertFederationClubsResult;
}

const upsertFederationClubsIR: any = {"usedParamSet":{"federation":true,"externalId":true,"name":true,"city":true,"country":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":176,"b":186}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":199,"b":209}]},{"name":"name","required":false,"transform":{"type":"scalar"},"locs":[{"a":222,"b":226}]},{"name":"city","required":false,"transform":{"type":"scalar"},"locs":[{"a":239,"b":243}]},{"name":"country","required":false,"transform":{"type":"scalar"},"locs":[{"a":256,"b":263}]}],"statement":"INSERT INTO federated.federation_club (federation, external_id, name, city, country)\nSELECT federation, external_id, name, nullif(city, ''), nullif(country, '')\nFROM unnest(\n  :federation::text[],\n  :externalId::text[],\n  :name::text[],\n  :city::text[],\n  :country::text[]\n) AS input(federation, external_id, name, city, country)\nON CONFLICT (federation, external_id) DO UPDATE\n  SET name = EXCLUDED.name,\n      city = EXCLUDED.city,\n      country = EXCLUDED.country\n  WHERE federated.federation_club.name IS DISTINCT FROM EXCLUDED.name\n     OR federated.federation_club.city IS DISTINCT FROM EXCLUDED.city\n     OR federated.federation_club.country IS DISTINCT FROM EXCLUDED.country"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO federated.federation_club (federation, external_id, name, city, country)
 * SELECT federation, external_id, name, nullif(city, ''), nullif(country, '')
 * FROM unnest(
 *   :federation::text[],
 *   :externalId::text[],
 *   :name::text[],
 *   :city::text[],
 *   :country::text[]
 * ) AS input(federation, external_id, name, city, country)
 * ON CONFLICT (federation, external_id) DO UPDATE
 *   SET name = EXCLUDED.name,
 *       city = EXCLUDED.city,
 *       country = EXCLUDED.country
 *   WHERE federated.federation_club.name IS DISTINCT FROM EXCLUDED.name
 *      OR federated.federation_club.city IS DISTINCT FROM EXCLUDED.city
 *      OR federated.federation_club.country IS DISTINCT FROM EXCLUDED.country
 * ```
 */
export const upsertFederationClubs = new PreparedQuery<IUpsertFederationClubsParams,IUpsertFederationClubsResult>(upsertFederationClubsIR);


/** 'UpsertEvents' parameters type */
export interface IUpsertEventsParams {
  city?: stringArray | null | void;
  country?: stringArray | null | void;
  endDate?: DateOrStringArray | null | void;
  externalId?: stringArray | null | void;
  federation?: stringArray | null | void;
  location?: stringArray | null | void;
  name?: stringArray | null | void;
  organizingClubId?: stringArray | null | void;
  startDate?: DateOrStringArray | null | void;
}

/** 'UpsertEvents' return type */
export type IUpsertEventsResult = void;

/** 'UpsertEvents' query type */
export interface IUpsertEventsQuery {
  params: IUpsertEventsParams;
  result: IUpsertEventsResult;
}

const upsertEventsIR: any = {"usedParamSet":{"federation":true,"externalId":true,"name":true,"startDate":true,"endDate":true,"location":true,"city":true,"country":true,"organizingClubId":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":344,"b":354}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":367,"b":377}]},{"name":"name","required":false,"transform":{"type":"scalar"},"locs":[{"a":390,"b":394}]},{"name":"startDate","required":false,"transform":{"type":"scalar"},"locs":[{"a":407,"b":416}]},{"name":"endDate","required":false,"transform":{"type":"scalar"},"locs":[{"a":429,"b":436}]},{"name":"location","required":false,"transform":{"type":"scalar"},"locs":[{"a":449,"b":457}]},{"name":"city","required":false,"transform":{"type":"scalar"},"locs":[{"a":470,"b":474}]},{"name":"country","required":false,"transform":{"type":"scalar"},"locs":[{"a":487,"b":494}]},{"name":"organizingClubId","required":false,"transform":{"type":"scalar"},"locs":[{"a":507,"b":523}]}],"statement":"INSERT INTO federated.event (\n  federation, external_id, name, start_date, end_date, location, city, country, organizing_club_id\n)\nSELECT\n  federation,\n  external_id,\n  nullif(name, ''),\n  start_date,\n  end_date,\n  nullif(location, ''),\n  nullif(city, ''),\n  nullif(country, ''),\n  CAST(NULLIF(organizing_club_id, '') AS bigint)\nFROM unnest(\n  :federation::text[],\n  :externalId::text[],\n  :name::text[],\n  :startDate::date[],\n  :endDate::date[],\n  :location::text[],\n  :city::text[],\n  :country::text[],\n  :organizingClubId::text[]\n) AS input(\n  federation, external_id, name, start_date, end_date, location, city, country, organizing_club_id\n)\nON CONFLICT (federation, external_id) DO UPDATE\n  SET name = COALESCE(EXCLUDED.name, federated.event.name),\n      start_date = EXCLUDED.start_date,\n      end_date = COALESCE(EXCLUDED.end_date, federated.event.end_date),\n      location = COALESCE(EXCLUDED.location, federated.event.location),\n      city = COALESCE(EXCLUDED.city, federated.event.city),\n      country = COALESCE(EXCLUDED.country, federated.event.country),\n      organizing_club_id = COALESCE(EXCLUDED.organizing_club_id, federated.event.organizing_club_id)"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO federated.event (
 *   federation, external_id, name, start_date, end_date, location, city, country, organizing_club_id
 * )
 * SELECT
 *   federation,
 *   external_id,
 *   nullif(name, ''),
 *   start_date,
 *   end_date,
 *   nullif(location, ''),
 *   nullif(city, ''),
 *   nullif(country, ''),
 *   CAST(NULLIF(organizing_club_id, '') AS bigint)
 * FROM unnest(
 *   :federation::text[],
 *   :externalId::text[],
 *   :name::text[],
 *   :startDate::date[],
 *   :endDate::date[],
 *   :location::text[],
 *   :city::text[],
 *   :country::text[],
 *   :organizingClubId::text[]
 * ) AS input(
 *   federation, external_id, name, start_date, end_date, location, city, country, organizing_club_id
 * )
 * ON CONFLICT (federation, external_id) DO UPDATE
 *   SET name = COALESCE(EXCLUDED.name, federated.event.name),
 *       start_date = EXCLUDED.start_date,
 *       end_date = COALESCE(EXCLUDED.end_date, federated.event.end_date),
 *       location = COALESCE(EXCLUDED.location, federated.event.location),
 *       city = COALESCE(EXCLUDED.city, federated.event.city),
 *       country = COALESCE(EXCLUDED.country, federated.event.country),
 *       organizing_club_id = COALESCE(EXCLUDED.organizing_club_id, federated.event.organizing_club_id)
 * ```
 */
export const upsertEvents = new PreparedQuery<IUpsertEventsParams,IUpsertEventsResult>(upsertEventsIR);


/** 'UpsertEventsDetailed' parameters type */
export interface IUpsertEventsDetailedParams {
  addressNote?: stringArray | null | void;
  city?: stringArray | null | void;
  contactEmail?: stringArray | null | void;
  contactName?: stringArray | null | void;
  contactPhone?: stringArray | null | void;
  country?: stringArray | null | void;
  endDate?: DateOrStringArray | null | void;
  externalId?: stringArray | null | void;
  federation?: stringArray | null | void;
  floorSize?: stringArray | null | void;
  geoReference?: stringArray | null | void;
  location?: stringArray | null | void;
  name?: stringArray | null | void;
  organizingClubId?: stringArray | null | void;
  postalCode?: stringArray | null | void;
  startDate?: DateOrStringArray | null | void;
  streetAddress?: stringArray | null | void;
  websiteUrl?: stringArray | null | void;
}

/** 'UpsertEventsDetailed' return type */
export type IUpsertEventsDetailedResult = void;

/** 'UpsertEventsDetailed' query type */
export interface IUpsertEventsDetailedQuery {
  params: IUpsertEventsDetailedParams;
  result: IUpsertEventsDetailedResult;
}

const upsertEventsDetailedIR: any = {"usedParamSet":{"federation":true,"externalId":true,"name":true,"startDate":true,"endDate":true,"location":true,"city":true,"country":true,"streetAddress":true,"postalCode":true,"addressNote":true,"geoReference":true,"floorSize":true,"contactName":true,"contactPhone":true,"contactEmail":true,"websiteUrl":true,"organizingClubId":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":726,"b":736}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":749,"b":759}]},{"name":"name","required":false,"transform":{"type":"scalar"},"locs":[{"a":772,"b":776}]},{"name":"startDate","required":false,"transform":{"type":"scalar"},"locs":[{"a":789,"b":798}]},{"name":"endDate","required":false,"transform":{"type":"scalar"},"locs":[{"a":811,"b":818}]},{"name":"location","required":false,"transform":{"type":"scalar"},"locs":[{"a":831,"b":839}]},{"name":"city","required":false,"transform":{"type":"scalar"},"locs":[{"a":852,"b":856}]},{"name":"country","required":false,"transform":{"type":"scalar"},"locs":[{"a":869,"b":876}]},{"name":"streetAddress","required":false,"transform":{"type":"scalar"},"locs":[{"a":889,"b":902}]},{"name":"postalCode","required":false,"transform":{"type":"scalar"},"locs":[{"a":915,"b":925}]},{"name":"addressNote","required":false,"transform":{"type":"scalar"},"locs":[{"a":938,"b":949}]},{"name":"geoReference","required":false,"transform":{"type":"scalar"},"locs":[{"a":962,"b":974}]},{"name":"floorSize","required":false,"transform":{"type":"scalar"},"locs":[{"a":987,"b":996}]},{"name":"contactName","required":false,"transform":{"type":"scalar"},"locs":[{"a":1009,"b":1020}]},{"name":"contactPhone","required":false,"transform":{"type":"scalar"},"locs":[{"a":1033,"b":1045}]},{"name":"contactEmail","required":false,"transform":{"type":"scalar"},"locs":[{"a":1058,"b":1070}]},{"name":"websiteUrl","required":false,"transform":{"type":"scalar"},"locs":[{"a":1083,"b":1093}]},{"name":"organizingClubId","required":false,"transform":{"type":"scalar"},"locs":[{"a":1106,"b":1122}]}],"statement":"INSERT INTO federated.event (\n  federation, external_id, name, start_date, end_date, location, city, country, street_address, postal_code,\n  address_note, geo_reference, floor_size, contact_name, contact_phone, contact_email, website_url, organizing_club_id\n)\nSELECT\n  federation,\n  external_id,\n  nullif(name, ''),\n  start_date,\n  end_date,\n  nullif(location, ''),\n  nullif(city, ''),\n  nullif(country, ''),\n  nullif(street_address, ''),\n  nullif(postal_code, ''),\n  nullif(address_note, ''),\n  nullif(geo_reference, ''),\n  nullif(floor_size, ''),\n  nullif(contact_name, ''),\n  nullif(contact_phone, ''),\n  nullif(contact_email, ''),\n  nullif(website_url, ''),\n  CAST(NULLIF(organizing_club_id, '') AS bigint)\nFROM unnest(\n  :federation::text[],\n  :externalId::text[],\n  :name::text[],\n  :startDate::date[],\n  :endDate::date[],\n  :location::text[],\n  :city::text[],\n  :country::text[],\n  :streetAddress::text[],\n  :postalCode::text[],\n  :addressNote::text[],\n  :geoReference::text[],\n  :floorSize::text[],\n  :contactName::text[],\n  :contactPhone::text[],\n  :contactEmail::text[],\n  :websiteUrl::text[],\n  :organizingClubId::text[]\n) AS input(\n  federation, external_id, name, start_date, end_date, location, city, country, street_address, postal_code,\n  address_note, geo_reference, floor_size, contact_name, contact_phone, contact_email, website_url, organizing_club_id\n)\nON CONFLICT (federation, external_id) DO UPDATE\n  SET name = COALESCE(EXCLUDED.name, federated.event.name),\n      start_date = EXCLUDED.start_date,\n      end_date = COALESCE(EXCLUDED.end_date, federated.event.end_date),\n      location = COALESCE(EXCLUDED.location, federated.event.location),\n      city = COALESCE(EXCLUDED.city, federated.event.city),\n      country = COALESCE(EXCLUDED.country, federated.event.country),\n      street_address = EXCLUDED.street_address,\n      postal_code = EXCLUDED.postal_code,\n      address_note = EXCLUDED.address_note,\n      geo_reference = EXCLUDED.geo_reference,\n      floor_size = EXCLUDED.floor_size,\n      contact_name = EXCLUDED.contact_name,\n      contact_phone = EXCLUDED.contact_phone,\n      contact_email = EXCLUDED.contact_email,\n      website_url = EXCLUDED.website_url,\n      organizing_club_id = COALESCE(EXCLUDED.organizing_club_id, federated.event.organizing_club_id)"};

/**
 * Query generated from SQL:
 * ```
 * INSERT INTO federated.event (
 *   federation, external_id, name, start_date, end_date, location, city, country, street_address, postal_code,
 *   address_note, geo_reference, floor_size, contact_name, contact_phone, contact_email, website_url, organizing_club_id
 * )
 * SELECT
 *   federation,
 *   external_id,
 *   nullif(name, ''),
 *   start_date,
 *   end_date,
 *   nullif(location, ''),
 *   nullif(city, ''),
 *   nullif(country, ''),
 *   nullif(street_address, ''),
 *   nullif(postal_code, ''),
 *   nullif(address_note, ''),
 *   nullif(geo_reference, ''),
 *   nullif(floor_size, ''),
 *   nullif(contact_name, ''),
 *   nullif(contact_phone, ''),
 *   nullif(contact_email, ''),
 *   nullif(website_url, ''),
 *   CAST(NULLIF(organizing_club_id, '') AS bigint)
 * FROM unnest(
 *   :federation::text[],
 *   :externalId::text[],
 *   :name::text[],
 *   :startDate::date[],
 *   :endDate::date[],
 *   :location::text[],
 *   :city::text[],
 *   :country::text[],
 *   :streetAddress::text[],
 *   :postalCode::text[],
 *   :addressNote::text[],
 *   :geoReference::text[],
 *   :floorSize::text[],
 *   :contactName::text[],
 *   :contactPhone::text[],
 *   :contactEmail::text[],
 *   :websiteUrl::text[],
 *   :organizingClubId::text[]
 * ) AS input(
 *   federation, external_id, name, start_date, end_date, location, city, country, street_address, postal_code,
 *   address_note, geo_reference, floor_size, contact_name, contact_phone, contact_email, website_url, organizing_club_id
 * )
 * ON CONFLICT (federation, external_id) DO UPDATE
 *   SET name = COALESCE(EXCLUDED.name, federated.event.name),
 *       start_date = EXCLUDED.start_date,
 *       end_date = COALESCE(EXCLUDED.end_date, federated.event.end_date),
 *       location = COALESCE(EXCLUDED.location, federated.event.location),
 *       city = COALESCE(EXCLUDED.city, federated.event.city),
 *       country = COALESCE(EXCLUDED.country, federated.event.country),
 *       street_address = EXCLUDED.street_address,
 *       postal_code = EXCLUDED.postal_code,
 *       address_note = EXCLUDED.address_note,
 *       geo_reference = EXCLUDED.geo_reference,
 *       floor_size = EXCLUDED.floor_size,
 *       contact_name = EXCLUDED.contact_name,
 *       contact_phone = EXCLUDED.contact_phone,
 *       contact_email = EXCLUDED.contact_email,
 *       website_url = EXCLUDED.website_url,
 *       organizing_club_id = COALESCE(EXCLUDED.organizing_club_id, federated.event.organizing_club_id)
 * ```
 */
export const upsertEventsDetailed = new PreparedQuery<IUpsertEventsDetailedParams,IUpsertEventsDetailedResult>(upsertEventsDetailedIR);


/** 'GetAllDancePrograms' parameters type */
export type IGetAllDanceProgramsParams = void;

/** 'GetAllDancePrograms' return type */
export interface IGetAllDanceProgramsResult {
  code: string | null;
  id: string;
}

/** 'GetAllDancePrograms' query type */
export interface IGetAllDanceProgramsQuery {
  params: IGetAllDanceProgramsParams;
  result: IGetAllDanceProgramsResult;
}

const getAllDanceProgramsIR: any = {"usedParamSet":{},"params":[],"statement":"SELECT id, code FROM federated.dance_program"};

/**
 * Query generated from SQL:
 * ```
 * SELECT id, code FROM federated.dance_program
 * ```
 */
export const getAllDancePrograms = new PreparedQuery<IGetAllDanceProgramsParams,IGetAllDanceProgramsResult>(getAllDanceProgramsIR);


/** 'UpsertDancePrograms' parameters type */
export interface IUpsertDanceProgramsParams {
  code?: stringArray | null | void;
  danceCode?: stringArray | null | void;
  danceOrder?: numberArray | null | void;
  discipline?: stringArray | null | void;
  name?: stringArray | null | void;
  programCode?: stringArray | null | void;
}

/** 'UpsertDancePrograms' return type */
export interface IUpsertDanceProgramsResult {
  code: string;
  id: string;
}

/** 'UpsertDancePrograms' query type */
export interface IUpsertDanceProgramsQuery {
  params: IUpsertDanceProgramsParams;
  result: IUpsertDanceProgramsResult;
}

const upsertDanceProgramsIR: any = {"usedParamSet":{"code":true,"name":true,"discipline":true,"programCode":true,"danceCode":true,"danceOrder":true},"params":[{"name":"code","required":false,"transform":{"type":"scalar"},"locs":[{"a":101,"b":105}]},{"name":"name","required":false,"transform":{"type":"scalar"},"locs":[{"a":120,"b":124}]},{"name":"discipline","required":false,"transform":{"type":"scalar"},"locs":[{"a":139,"b":149}]},{"name":"programCode","required":false,"transform":{"type":"scalar"},"locs":[{"a":538,"b":549}]},{"name":"danceCode","required":false,"transform":{"type":"scalar"},"locs":[{"a":564,"b":573}]},{"name":"danceOrder","required":false,"transform":{"type":"scalar"},"locs":[{"a":588,"b":598}]}],"statement":"WITH input_program AS (\n  SELECT code, name, nullif(discipline, '') AS discipline\n  FROM unnest(\n    :code::text[],\n    :name::text[],\n    :discipline::text[]\n  ) AS input(code, name, discipline)\n), programs AS (\n  INSERT INTO federated.dance_program (code, name, discipline)\n  SELECT code, name, discipline\n  FROM input_program\n  ON CONFLICT (code) DO UPDATE\n    SET name = EXCLUDED.name,\n        discipline = EXCLUDED.discipline\n  RETURNING id, code\n), input_dance AS (\n  SELECT program_code, dance_code, dance_order\n  FROM unnest(\n    :programCode::text[],\n    :danceCode::text[],\n    :danceOrder::int[]\n  ) AS input(program_code, dance_code, dance_order)\n), deleted AS (\n  DELETE FROM federated.dance_program_dance dpd\n  USING programs p\n  WHERE dpd.program_id = p.id\n    AND NOT EXISTS (\n      SELECT 1\n      FROM input_dance keep\n      WHERE keep.program_code = p.code\n        AND keep.dance_code = dpd.dance_code\n    )\n), upserted AS (\n  INSERT INTO federated.dance_program_dance (program_id, dance_code, dance_order)\n  SELECT p.id, input_dance.dance_code, input_dance.dance_order\n  FROM input_dance\n  JOIN programs p ON p.code = input_dance.program_code\n  ON CONFLICT (program_id, dance_code) DO UPDATE\n    SET dance_order = EXCLUDED.dance_order\n)\nSELECT p.id AS \"id!\", p.code AS \"code!\"\nFROM programs p"};

/**
 * Query generated from SQL:
 * ```
 * WITH input_program AS (
 *   SELECT code, name, nullif(discipline, '') AS discipline
 *   FROM unnest(
 *     :code::text[],
 *     :name::text[],
 *     :discipline::text[]
 *   ) AS input(code, name, discipline)
 * ), programs AS (
 *   INSERT INTO federated.dance_program (code, name, discipline)
 *   SELECT code, name, discipline
 *   FROM input_program
 *   ON CONFLICT (code) DO UPDATE
 *     SET name = EXCLUDED.name,
 *         discipline = EXCLUDED.discipline
 *   RETURNING id, code
 * ), input_dance AS (
 *   SELECT program_code, dance_code, dance_order
 *   FROM unnest(
 *     :programCode::text[],
 *     :danceCode::text[],
 *     :danceOrder::int[]
 *   ) AS input(program_code, dance_code, dance_order)
 * ), deleted AS (
 *   DELETE FROM federated.dance_program_dance dpd
 *   USING programs p
 *   WHERE dpd.program_id = p.id
 *     AND NOT EXISTS (
 *       SELECT 1
 *       FROM input_dance keep
 *       WHERE keep.program_code = p.code
 *         AND keep.dance_code = dpd.dance_code
 *     )
 * ), upserted AS (
 *   INSERT INTO federated.dance_program_dance (program_id, dance_code, dance_order)
 *   SELECT p.id, input_dance.dance_code, input_dance.dance_order
 *   FROM input_dance
 *   JOIN programs p ON p.code = input_dance.program_code
 *   ON CONFLICT (program_id, dance_code) DO UPDATE
 *     SET dance_order = EXCLUDED.dance_order
 * )
 * SELECT p.id AS "id!", p.code AS "code!"
 * FROM programs p
 * ```
 */
export const upsertDancePrograms = new PreparedQuery<IUpsertDanceProgramsParams,IUpsertDanceProgramsResult>(upsertDanceProgramsIR);


/** 'UpsertCompetitions' parameters type */
export interface IUpsertCompetitionsParams {
  categoryId?: NumberOrStringArray | null | void;
  checkInEnd?: stringArray | null | void;
  competitionType?: competition_typeArray | null | void;
  completedAt?: stringArray | null | void;
  endDate?: DateOrStringArray | null | void;
  eventExternalId?: string | null | void;
  excusedTotal?: numberArray | null | void;
  externalId?: stringArray | null | void;
  federation?: string | null | void;
  participantsTotal?: numberArray | null | void;
  registrationFee?: stringArray | null | void;
  startDate?: DateOrStringArray | null | void;
}

/** 'UpsertCompetitions' return type */
export type IUpsertCompetitionsResult = void;

/** 'UpsertCompetitions' query type */
export interface IUpsertCompetitionsQuery {
  params: IUpsertCompetitionsParams;
  result: IUpsertCompetitionsResult;
}

const upsertCompetitionsIR: any = {"usedParamSet":{"federation":true,"eventExternalId":true,"externalId":true,"categoryId":true,"startDate":true,"endDate":true,"participantsTotal":true,"checkInEnd":true,"completedAt":true,"registrationFee":true,"excusedTotal":true,"competitionType":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":72,"b":82},{"a":839,"b":849}]},{"name":"eventExternalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":102,"b":117}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":163,"b":173}]},{"name":"categoryId","required":false,"transform":{"type":"scalar"},"locs":[{"a":188,"b":198}]},{"name":"startDate","required":false,"transform":{"type":"scalar"},"locs":[{"a":215,"b":224}]},{"name":"endDate","required":false,"transform":{"type":"scalar"},"locs":[{"a":239,"b":246}]},{"name":"participantsTotal","required":false,"transform":{"type":"scalar"},"locs":[{"a":261,"b":278}]},{"name":"checkInEnd","required":false,"transform":{"type":"scalar"},"locs":[{"a":292,"b":302}]},{"name":"completedAt","required":false,"transform":{"type":"scalar"},"locs":[{"a":317,"b":328}]},{"name":"registrationFee","required":false,"transform":{"type":"scalar"},"locs":[{"a":343,"b":358}]},{"name":"excusedTotal","required":false,"transform":{"type":"scalar"},"locs":[{"a":373,"b":385}]},{"name":"competitionType","required":false,"transform":{"type":"scalar"},"locs":[{"a":399,"b":414}]}],"statement":"WITH event AS (\n  SELECT id\n  FROM federated.event\n  WHERE federation = :federation AND external_id = :eventExternalId\n), input AS (\n  SELECT *\n  FROM unnest(\n    :externalId::text[],\n    :categoryId::bigint[],\n    :startDate::date[],\n    :endDate::date[],\n    :participantsTotal::int[],\n    :checkInEnd::text[],\n    :completedAt::text[],\n    :registrationFee::text[],\n    :excusedTotal::int[],\n    :competitionType::federated.competition_type[]\n  ) AS input(\n    external_id, category_id, start_date, end_date, participants_total,\n    check_in_end, completed_at, registration_fee, excused_total, competition_type\n  )\n)\nINSERT INTO federated.competition (\n  federation, external_id, event_id, category_id, start_date, end_date, participants_total,\n  check_in_end, completed_at, registration_fee, excused_total, competition_type\n)\nSELECT\n  :federation,\n  input.external_id,\n  event.id,\n  input.category_id,\n  input.start_date,\n  input.end_date,\n  input.participants_total,\n  nullif(input.check_in_end, '')::time,\n  nullif(input.completed_at, '')::timestamptz,\n  nullif(input.registration_fee, '')::numeric(10,3),\n  input.excused_total,\n  input.competition_type\nFROM input\nCROSS JOIN event\nON CONFLICT (federation, external_id) DO UPDATE\n  SET event_id = EXCLUDED.event_id,\n      category_id = EXCLUDED.category_id,\n      start_date = EXCLUDED.start_date,\n      end_date = EXCLUDED.end_date,\n      participants_total = EXCLUDED.participants_total,\n      check_in_end = EXCLUDED.check_in_end,\n      completed_at = EXCLUDED.completed_at,\n      registration_fee = EXCLUDED.registration_fee,\n      excused_total = EXCLUDED.excused_total,\n      competition_type = EXCLUDED.competition_type"};

/**
 * Query generated from SQL:
 * ```
 * WITH event AS (
 *   SELECT id
 *   FROM federated.event
 *   WHERE federation = :federation AND external_id = :eventExternalId
 * ), input AS (
 *   SELECT *
 *   FROM unnest(
 *     :externalId::text[],
 *     :categoryId::bigint[],
 *     :startDate::date[],
 *     :endDate::date[],
 *     :participantsTotal::int[],
 *     :checkInEnd::text[],
 *     :completedAt::text[],
 *     :registrationFee::text[],
 *     :excusedTotal::int[],
 *     :competitionType::federated.competition_type[]
 *   ) AS input(
 *     external_id, category_id, start_date, end_date, participants_total,
 *     check_in_end, completed_at, registration_fee, excused_total, competition_type
 *   )
 * )
 * INSERT INTO federated.competition (
 *   federation, external_id, event_id, category_id, start_date, end_date, participants_total,
 *   check_in_end, completed_at, registration_fee, excused_total, competition_type
 * )
 * SELECT
 *   :federation,
 *   input.external_id,
 *   event.id,
 *   input.category_id,
 *   input.start_date,
 *   input.end_date,
 *   input.participants_total,
 *   nullif(input.check_in_end, '')::time,
 *   nullif(input.completed_at, '')::timestamptz,
 *   nullif(input.registration_fee, '')::numeric(10,3),
 *   input.excused_total,
 *   input.competition_type
 * FROM input
 * CROSS JOIN event
 * ON CONFLICT (federation, external_id) DO UPDATE
 *   SET event_id = EXCLUDED.event_id,
 *       category_id = EXCLUDED.category_id,
 *       start_date = EXCLUDED.start_date,
 *       end_date = EXCLUDED.end_date,
 *       participants_total = EXCLUDED.participants_total,
 *       check_in_end = EXCLUDED.check_in_end,
 *       completed_at = EXCLUDED.completed_at,
 *       registration_fee = EXCLUDED.registration_fee,
 *       excused_total = EXCLUDED.excused_total,
 *       competition_type = EXCLUDED.competition_type
 * ```
 */
export const upsertCompetitions = new PreparedQuery<IUpsertCompetitionsParams,IUpsertCompetitionsResult>(upsertCompetitionsIR);


/** 'MergeEventOfficials' parameters type */
export interface IMergeEventOfficialsParams {
  discipline?: stringArray | null | void;
  eventExternalId?: string | null | void;
  federation?: string | null | void;
  grade?: stringArray | null | void;
  personId?: stringArray | null | void;
  role?: official_roleArray | null | void;
}

/** 'MergeEventOfficials' return type */
export type IMergeEventOfficialsResult = void;

/** 'MergeEventOfficials' query type */
export interface IMergeEventOfficialsQuery {
  params: IMergeEventOfficialsParams;
  result: IMergeEventOfficialsResult;
}

const mergeEventOfficialsIR: any = {"usedParamSet":{"federation":true,"eventExternalId":true,"personId":true,"role":true,"discipline":true,"grade":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":72,"b":82}]},{"name":"eventExternalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":106,"b":121}]},{"name":"personId","required":false,"transform":{"type":"scalar"},"locs":[{"a":309,"b":317}]},{"name":"role","required":false,"transform":{"type":"scalar"},"locs":[{"a":332,"b":336}]},{"name":"discipline","required":false,"transform":{"type":"scalar"},"locs":[{"a":370,"b":380}]},{"name":"grade","required":false,"transform":{"type":"scalar"},"locs":[{"a":395,"b":400}]}],"statement":"WITH event AS (\n  SELECT id\n  FROM federated.event\n  WHERE federation = :federation\n    AND external_id = :eventExternalId\n), source AS (\n  SELECT\n    event.id AS event_id,\n    person_id,\n    role,\n    nullif(discipline, '') AS discipline,\n    nullif(grade, '') AS grade\n  FROM event\n  CROSS JOIN unnest(\n    :personId::text[],\n    :role::federated.official_role[],\n    :discipline::text[],\n    :grade::text[]\n  ) AS input(person_id, role, discipline, grade)\n), upserted AS (\n  INSERT INTO federated.event_official (event_id, person_id, role, discipline, grade)\n  SELECT s.event_id, s.person_id, s.role, coalesce(s.discipline, ''), s.grade\n  FROM source s\n  ON CONFLICT (event_id, person_id, role, discipline) DO UPDATE\n    SET grade = EXCLUDED.grade\n    WHERE federated.event_official.grade IS DISTINCT FROM EXCLUDED.grade\n)\nDELETE FROM federated.event_official t\nWHERE t.event_id IN (SELECT id FROM event)\n  AND NOT EXISTS (\n    SELECT 1\n    FROM source s\n    WHERE s.event_id = t.event_id\n      AND s.person_id = t.person_id\n      AND s.role = t.role\n      AND coalesce(s.discipline, '') = t.discipline\n  )"};

/**
 * Query generated from SQL:
 * ```
 * WITH event AS (
 *   SELECT id
 *   FROM federated.event
 *   WHERE federation = :federation
 *     AND external_id = :eventExternalId
 * ), source AS (
 *   SELECT
 *     event.id AS event_id,
 *     person_id,
 *     role,
 *     nullif(discipline, '') AS discipline,
 *     nullif(grade, '') AS grade
 *   FROM event
 *   CROSS JOIN unnest(
 *     :personId::text[],
 *     :role::federated.official_role[],
 *     :discipline::text[],
 *     :grade::text[]
 *   ) AS input(person_id, role, discipline, grade)
 * ), upserted AS (
 *   INSERT INTO federated.event_official (event_id, person_id, role, discipline, grade)
 *   SELECT s.event_id, s.person_id, s.role, coalesce(s.discipline, ''), s.grade
 *   FROM source s
 *   ON CONFLICT (event_id, person_id, role, discipline) DO UPDATE
 *     SET grade = EXCLUDED.grade
 *     WHERE federated.event_official.grade IS DISTINCT FROM EXCLUDED.grade
 * )
 * DELETE FROM federated.event_official t
 * WHERE t.event_id IN (SELECT id FROM event)
 *   AND NOT EXISTS (
 *     SELECT 1
 *     FROM source s
 *     WHERE s.event_id = t.event_id
 *       AND s.person_id = t.person_id
 *       AND s.role = t.role
 *       AND coalesce(s.discipline, '') = t.discipline
 *   )
 * ```
 */
export const mergeEventOfficials = new PreparedQuery<IMergeEventOfficialsParams,IMergeEventOfficialsResult>(mergeEventOfficialsIR);


/** 'MergeCompetitionOfficials' parameters type */
export interface IMergeCompetitionOfficialsParams {
  competitionExternalId?: stringArray | null | void;
  federation?: string | null | void;
  personId?: stringArray | null | void;
  role?: official_roleArray | null | void;
  scopeCompetitionExternalId?: stringArray | null | void;
}

/** 'MergeCompetitionOfficials' return type */
export type IMergeCompetitionOfficialsResult = void;

/** 'MergeCompetitionOfficials' query type */
export interface IMergeCompetitionOfficialsQuery {
  params: IMergeCompetitionOfficialsParams;
  result: IMergeCompetitionOfficialsResult;
}

const mergeCompetitionOfficialsIR: any = {"usedParamSet":{"competitionExternalId":true,"personId":true,"role":true,"federation":true,"scopeCompetitionExternalId":true},"params":[{"name":"competitionExternalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":85,"b":106}]},{"name":"personId","required":false,"transform":{"type":"scalar"},"locs":[{"a":121,"b":129}]},{"name":"role","required":false,"transform":{"type":"scalar"},"locs":[{"a":144,"b":148}]},{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":328,"b":338}]},{"name":"scopeCompetitionExternalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":378,"b":404}]}],"statement":"WITH input AS (\n  SELECT competition_external_id, person_id, role\n  FROM unnest(\n    :competitionExternalId::text[],\n    :personId::text[],\n    :role::federated.official_role[]\n  ) AS input(competition_external_id, person_id, role)\n), competitions AS (\n  SELECT id, external_id\n  FROM federated.competition\n  WHERE federation = :federation\n    AND external_id IN (SELECT unnest(:scopeCompetitionExternalId::text[]))\n), source AS (\n  SELECT competitions.id AS competition_id, input.person_id, input.role\n  FROM input\n  JOIN competitions ON competitions.external_id = input.competition_external_id\n), inserted_official AS (\n  INSERT INTO federated.competition_official (competition_id, person_id, role)\n  SELECT s.competition_id, s.person_id, s.role\n  FROM source s\n  ON CONFLICT (competition_id, person_id, role) DO NOTHING\n)\nDELETE FROM federated.competition_official t\nWHERE t.competition_id IN (SELECT id FROM competitions)\n  AND NOT EXISTS (\n    SELECT 1\n    FROM source s\n    WHERE s.competition_id = t.competition_id\n      AND s.person_id = t.person_id\n      AND s.role = t.role\n  )"};

/**
 * Query generated from SQL:
 * ```
 * WITH input AS (
 *   SELECT competition_external_id, person_id, role
 *   FROM unnest(
 *     :competitionExternalId::text[],
 *     :personId::text[],
 *     :role::federated.official_role[]
 *   ) AS input(competition_external_id, person_id, role)
 * ), competitions AS (
 *   SELECT id, external_id
 *   FROM federated.competition
 *   WHERE federation = :federation
 *     AND external_id IN (SELECT unnest(:scopeCompetitionExternalId::text[]))
 * ), source AS (
 *   SELECT competitions.id AS competition_id, input.person_id, input.role
 *   FROM input
 *   JOIN competitions ON competitions.external_id = input.competition_external_id
 * ), inserted_official AS (
 *   INSERT INTO federated.competition_official (competition_id, person_id, role)
 *   SELECT s.competition_id, s.person_id, s.role
 *   FROM source s
 *   ON CONFLICT (competition_id, person_id, role) DO NOTHING
 * )
 * DELETE FROM federated.competition_official t
 * WHERE t.competition_id IN (SELECT id FROM competitions)
 *   AND NOT EXISTS (
 *     SELECT 1
 *     FROM source s
 *     WHERE s.competition_id = t.competition_id
 *       AND s.person_id = t.person_id
 *       AND s.role = t.role
 *   )
 * ```
 */
export const mergeCompetitionOfficials = new PreparedQuery<IMergeCompetitionOfficialsParams,IMergeCompetitionOfficialsResult>(mergeCompetitionOfficialsIR);


/** 'GetCompetitionContext' parameters type */
export interface IGetCompetitionContextParams {
  externalId?: string | null | void;
  federation?: string | null | void;
}

/** 'GetCompetitionContext' return type */
export interface IGetCompetitionContextResult {
  categoryId: string;
  competitorType: competitor_type;
  eventId: string;
  id: string;
  startDate: Date;
}

/** 'GetCompetitionContext' query type */
export interface IGetCompetitionContextQuery {
  params: IGetCompetitionContextParams;
  result: IGetCompetitionContextResult;
}

const getCompetitionContextIR: any = {"usedParamSet":{"federation":true,"externalId":true},"params":[{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":252,"b":262}]},{"name":"externalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":286,"b":296}]}],"statement":"SELECT\n  c.id,\n  c.event_id AS \"eventId\",\n  c.category_id AS \"categoryId\",\n  c.start_date AS \"startDate\",\n  cat.competitor_type AS \"competitorType\"\nFROM federated.competition c\nJOIN federated.category cat ON cat.id = c.category_id\nWHERE c.federation = :federation\n  AND c.external_id = :externalId"};

/**
 * Query generated from SQL:
 * ```
 * SELECT
 *   c.id,
 *   c.event_id AS "eventId",
 *   c.category_id AS "categoryId",
 *   c.start_date AS "startDate",
 *   cat.competitor_type AS "competitorType"
 * FROM federated.competition c
 * JOIN federated.category cat ON cat.id = c.category_id
 * WHERE c.federation = :federation
 *   AND c.external_id = :externalId
 * ```
 */
export const getCompetitionContext = new PreparedQuery<IGetCompetitionContextParams,IGetCompetitionContextResult>(getCompetitionContextIR);


/** 'MergeCompetitionEntriesByEventId' parameters type */
export interface IMergeCompetitionEntriesByEventIdParams {
  cancelled?: booleanArray | null | void;
  competitionExternalId?: stringArray | null | void;
  competitorId?: stringArray | null | void;
  eventId?: string | null | void;
  federation?: string | null | void;
}

/** 'MergeCompetitionEntriesByEventId' return type */
export type IMergeCompetitionEntriesByEventIdResult = void;

/** 'MergeCompetitionEntriesByEventId' query type */
export interface IMergeCompetitionEntriesByEventIdQuery {
  params: IMergeCompetitionEntriesByEventIdParams;
  result: IMergeCompetitionEntriesByEventIdResult;
}

const mergeCompetitionEntriesByEventIdIR: any = {"usedParamSet":{"competitionExternalId":true,"competitorId":true,"cancelled":true,"federation":true,"eventId":true},"params":[{"name":"competitionExternalId","required":false,"transform":{"type":"scalar"},"locs":[{"a":94,"b":115}]},{"name":"competitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":130,"b":142}]},{"name":"cancelled","required":false,"transform":{"type":"scalar"},"locs":[{"a":157,"b":166}]},{"name":"federation","required":false,"transform":{"type":"scalar"},"locs":[{"a":393,"b":403}]},{"name":"eventId","required":false,"transform":{"type":"scalar"},"locs":[{"a":425,"b":432}]}],"statement":"WITH input AS (\n  SELECT competition_external_id, competitor_id, cancelled\n  FROM unnest(\n    :competitionExternalId::text[],\n    :competitorId::text[],\n    :cancelled::boolean[]\n  ) AS input(competition_external_id, competitor_id, cancelled)\n), competitions AS (\n  SELECT c.id, c.external_id\n  FROM federated.competition c\n  JOIN federated.event e ON e.id = c.event_id\n  WHERE c.federation = :federation AND e.external_id = :eventId\n), source AS (\n  SELECT competitions.id AS competition_id, input.competitor_id, input.cancelled\n  FROM input\n  JOIN competitions ON competitions.external_id = input.competition_external_id\n), upserted AS (\n  INSERT INTO federated.competition_entry (competition_id, competitor_id, cancelled)\n  SELECT s.competition_id, s.competitor_id, s.cancelled\n  FROM source s\n  ON CONFLICT (competition_id, competitor_id) DO UPDATE\n    SET cancelled = EXCLUDED.cancelled\n    WHERE federated.competition_entry.cancelled IS DISTINCT FROM EXCLUDED.cancelled\n)\nDELETE FROM federated.competition_entry t\nWHERE t.competition_id IN (SELECT id FROM competitions)\n  AND NOT EXISTS (\n    SELECT 1\n    FROM source s\n    WHERE s.competition_id = t.competition_id\n      AND s.competitor_id = t.competitor_id\n  )"};

/**
 * Query generated from SQL:
 * ```
 * WITH input AS (
 *   SELECT competition_external_id, competitor_id, cancelled
 *   FROM unnest(
 *     :competitionExternalId::text[],
 *     :competitorId::text[],
 *     :cancelled::boolean[]
 *   ) AS input(competition_external_id, competitor_id, cancelled)
 * ), competitions AS (
 *   SELECT c.id, c.external_id
 *   FROM federated.competition c
 *   JOIN federated.event e ON e.id = c.event_id
 *   WHERE c.federation = :federation AND e.external_id = :eventId
 * ), source AS (
 *   SELECT competitions.id AS competition_id, input.competitor_id, input.cancelled
 *   FROM input
 *   JOIN competitions ON competitions.external_id = input.competition_external_id
 * ), upserted AS (
 *   INSERT INTO federated.competition_entry (competition_id, competitor_id, cancelled)
 *   SELECT s.competition_id, s.competitor_id, s.cancelled
 *   FROM source s
 *   ON CONFLICT (competition_id, competitor_id) DO UPDATE
 *     SET cancelled = EXCLUDED.cancelled
 *     WHERE federated.competition_entry.cancelled IS DISTINCT FROM EXCLUDED.cancelled
 * )
 * DELETE FROM federated.competition_entry t
 * WHERE t.competition_id IN (SELECT id FROM competitions)
 *   AND NOT EXISTS (
 *     SELECT 1
 *     FROM source s
 *     WHERE s.competition_id = t.competition_id
 *       AND s.competitor_id = t.competitor_id
 *   )
 * ```
 */
export const mergeCompetitionEntriesByEventId = new PreparedQuery<IMergeCompetitionEntriesByEventIdParams,IMergeCompetitionEntriesByEventIdResult>(mergeCompetitionEntriesByEventIdIR);


/** 'MergeCompetitionResults' parameters type */
export interface IMergeCompetitionResultsParams {
  competitionId?: NumberOrString | null | void;
  competitorId?: stringArray | null | void;
  completionStatus?: stringArray | null | void;
  finalGain?: stringArray | null | void;
  isFinal?: booleanArray | null | void;
  lastDance?: stringArray | null | void;
  lastRound?: stringArray | null | void;
  pointGain?: stringArray | null | void;
  ranking?: numberArray | null | void;
  rankingTo?: numberArray | null | void;
  startNumber?: stringArray | null | void;
}

/** 'MergeCompetitionResults' return type */
export type IMergeCompetitionResultsResult = void;

/** 'MergeCompetitionResults' query type */
export interface IMergeCompetitionResultsQuery {
  params: IMergeCompetitionResultsParams;
  result: IMergeCompetitionResultsResult;
}

const mergeCompetitionResultsIR: any = {"usedParamSet":{"competitorId":true,"startNumber":true,"ranking":true,"rankingTo":true,"pointGain":true,"finalGain":true,"isFinal":true,"completionStatus":true,"lastRound":true,"lastDance":true,"competitionId":true},"params":[{"name":"competitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":406,"b":418}]},{"name":"startNumber","required":false,"transform":{"type":"scalar"},"locs":[{"a":433,"b":444}]},{"name":"ranking","required":false,"transform":{"type":"scalar"},"locs":[{"a":459,"b":466}]},{"name":"rankingTo","required":false,"transform":{"type":"scalar"},"locs":[{"a":480,"b":489}]},{"name":"pointGain","required":false,"transform":{"type":"scalar"},"locs":[{"a":503,"b":512}]},{"name":"finalGain","required":false,"transform":{"type":"scalar"},"locs":[{"a":527,"b":536}]},{"name":"isFinal","required":false,"transform":{"type":"scalar"},"locs":[{"a":551,"b":558}]},{"name":"completionStatus","required":false,"transform":{"type":"scalar"},"locs":[{"a":576,"b":592}]},{"name":"lastRound","required":false,"transform":{"type":"scalar"},"locs":[{"a":607,"b":616}]},{"name":"lastDance","required":false,"transform":{"type":"scalar"},"locs":[{"a":631,"b":640}]},{"name":"competitionId","required":false,"transform":{"type":"scalar"},"locs":[{"a":1031,"b":1044},{"a":2515,"b":2528}]}],"statement":"WITH source AS (\n  SELECT\n    competitor_id,\n    nullif(start_number, '') AS start_number,\n    ranking,\n    ranking_to,\n    nullif(point_gain, '')::numeric(10,3) AS point_gain,\n    nullif(final_gain, '')::numeric(10,3) AS final_gain,\n    is_final,\n    nullif(completion_status, '') AS completion_status,\n    nullif(last_round, '') AS last_round,\n    nullif(last_dance, '') AS last_dance\n  FROM unnest(\n    :competitorId::text[],\n    :startNumber::text[],\n    :ranking::int[],\n    :rankingTo::int[],\n    :pointGain::text[],\n    :finalGain::text[],\n    :isFinal::boolean[],\n    :completionStatus::text[],\n    :lastRound::text[],\n    :lastDance::text[]\n  ) AS input(\n    competitor_id, start_number, ranking, ranking_to, point_gain, final_gain,\n    is_final, completion_status, last_round, last_dance\n  )\n), upserted AS (\n  INSERT INTO federated.competition_result (\n    competition_id, competitor_id, start_number, ranking, ranking_to, point_gain, final_gain,\n    is_final, completion_status, last_round, last_dance\n  )\n  SELECT\n    :competitionId, s.competitor_id, s.start_number, s.ranking, s.ranking_to, s.point_gain,\n    s.final_gain, s.is_final, s.completion_status, s.last_round, s.last_dance\n  FROM source s\n  ON CONFLICT (competition_id, competitor_id) DO UPDATE\n    SET start_number = EXCLUDED.start_number,\n        ranking = EXCLUDED.ranking,\n        ranking_to = EXCLUDED.ranking_to,\n        point_gain = EXCLUDED.point_gain,\n        final_gain = EXCLUDED.final_gain,\n        is_final = EXCLUDED.is_final,\n        completion_status = EXCLUDED.completion_status,\n        last_round = EXCLUDED.last_round,\n        last_dance = EXCLUDED.last_dance\n    WHERE federated.competition_result.start_number IS DISTINCT FROM EXCLUDED.start_number\n       OR federated.competition_result.ranking IS DISTINCT FROM EXCLUDED.ranking\n       OR federated.competition_result.ranking_to IS DISTINCT FROM EXCLUDED.ranking_to\n       OR federated.competition_result.point_gain IS DISTINCT FROM EXCLUDED.point_gain\n       OR federated.competition_result.final_gain IS DISTINCT FROM EXCLUDED.final_gain\n       OR federated.competition_result.is_final IS DISTINCT FROM EXCLUDED.is_final\n       OR federated.competition_result.completion_status IS DISTINCT FROM EXCLUDED.completion_status\n       OR federated.competition_result.last_round IS DISTINCT FROM EXCLUDED.last_round\n       OR federated.competition_result.last_dance IS DISTINCT FROM EXCLUDED.last_dance\n)\nDELETE FROM federated.competition_result t\nWHERE t.competition_id = :competitionId\n  AND NOT EXISTS (\n    SELECT 1\n    FROM source s\n    WHERE s.competitor_id = t.competitor_id\n  )"};

/**
 * Query generated from SQL:
 * ```
 * WITH source AS (
 *   SELECT
 *     competitor_id,
 *     nullif(start_number, '') AS start_number,
 *     ranking,
 *     ranking_to,
 *     nullif(point_gain, '')::numeric(10,3) AS point_gain,
 *     nullif(final_gain, '')::numeric(10,3) AS final_gain,
 *     is_final,
 *     nullif(completion_status, '') AS completion_status,
 *     nullif(last_round, '') AS last_round,
 *     nullif(last_dance, '') AS last_dance
 *   FROM unnest(
 *     :competitorId::text[],
 *     :startNumber::text[],
 *     :ranking::int[],
 *     :rankingTo::int[],
 *     :pointGain::text[],
 *     :finalGain::text[],
 *     :isFinal::boolean[],
 *     :completionStatus::text[],
 *     :lastRound::text[],
 *     :lastDance::text[]
 *   ) AS input(
 *     competitor_id, start_number, ranking, ranking_to, point_gain, final_gain,
 *     is_final, completion_status, last_round, last_dance
 *   )
 * ), upserted AS (
 *   INSERT INTO federated.competition_result (
 *     competition_id, competitor_id, start_number, ranking, ranking_to, point_gain, final_gain,
 *     is_final, completion_status, last_round, last_dance
 *   )
 *   SELECT
 *     :competitionId, s.competitor_id, s.start_number, s.ranking, s.ranking_to, s.point_gain,
 *     s.final_gain, s.is_final, s.completion_status, s.last_round, s.last_dance
 *   FROM source s
 *   ON CONFLICT (competition_id, competitor_id) DO UPDATE
 *     SET start_number = EXCLUDED.start_number,
 *         ranking = EXCLUDED.ranking,
 *         ranking_to = EXCLUDED.ranking_to,
 *         point_gain = EXCLUDED.point_gain,
 *         final_gain = EXCLUDED.final_gain,
 *         is_final = EXCLUDED.is_final,
 *         completion_status = EXCLUDED.completion_status,
 *         last_round = EXCLUDED.last_round,
 *         last_dance = EXCLUDED.last_dance
 *     WHERE federated.competition_result.start_number IS DISTINCT FROM EXCLUDED.start_number
 *        OR federated.competition_result.ranking IS DISTINCT FROM EXCLUDED.ranking
 *        OR federated.competition_result.ranking_to IS DISTINCT FROM EXCLUDED.ranking_to
 *        OR federated.competition_result.point_gain IS DISTINCT FROM EXCLUDED.point_gain
 *        OR federated.competition_result.final_gain IS DISTINCT FROM EXCLUDED.final_gain
 *        OR federated.competition_result.is_final IS DISTINCT FROM EXCLUDED.is_final
 *        OR federated.competition_result.completion_status IS DISTINCT FROM EXCLUDED.completion_status
 *        OR federated.competition_result.last_round IS DISTINCT FROM EXCLUDED.last_round
 *        OR federated.competition_result.last_dance IS DISTINCT FROM EXCLUDED.last_dance
 * )
 * DELETE FROM federated.competition_result t
 * WHERE t.competition_id = :competitionId
 *   AND NOT EXISTS (
 *     SELECT 1
 *     FROM source s
 *     WHERE s.competitor_id = t.competitor_id
 *   )
 * ```
 */
export const mergeCompetitionResults = new PreparedQuery<IMergeCompetitionResultsParams,IMergeCompetitionResultsResult>(mergeCompetitionResultsIR);


/** 'UpsertCompetitionRounds' parameters type */
export interface IUpsertCompetitionRoundsParams {
  competitionId?: NumberOrString | null | void;
  danceProgramId?: NumberOrStringArray | null | void;
  roundIndex?: numberArray | null | void;
  roundKey?: stringArray | null | void;
  roundLabel?: stringArray | null | void;
  scoringMethod?: scoring_methodArray | null | void;
}

/** 'UpsertCompetitionRounds' return type */
export interface IUpsertCompetitionRoundsResult {
  id: string;
  roundKey: string;
}

/** 'UpsertCompetitionRounds' query type */
export interface IUpsertCompetitionRoundsQuery {
  params: IUpsertCompetitionRoundsParams;
  result: IUpsertCompetitionRoundsResult;
}

const upsertCompetitionRoundsIR: any = {"usedParamSet":{"roundKey":true,"roundLabel":true,"roundIndex":true,"danceProgramId":true,"scoringMethod":true,"competitionId":true},"params":[{"name":"roundKey","required":false,"transform":{"type":"scalar"},"locs":[{"a":46,"b":54}]},{"name":"roundLabel","required":false,"transform":{"type":"scalar"},"locs":[{"a":69,"b":79}]},{"name":"roundIndex","required":false,"transform":{"type":"scalar"},"locs":[{"a":94,"b":104}]},{"name":"danceProgramId","required":false,"transform":{"type":"scalar"},"locs":[{"a":118,"b":132}]},{"name":"scoringMethod","required":false,"transform":{"type":"scalar"},"locs":[{"a":149,"b":162}]},{"name":"competitionId","required":false,"transform":{"type":"scalar"},"locs":[{"a":378,"b":391},{"a":784,"b":797}]}],"statement":"WITH input AS (\n  SELECT *\n  FROM unnest(\n    :roundKey::text[],\n    :roundLabel::text[],\n    :roundIndex::int[],\n    :danceProgramId::bigint[],\n    :scoringMethod::federated.scoring_method[]\n  ) AS input(round_key, round_label, round_index, dance_program_id, scoring_method)\n), stale_rounds AS (\n  SELECT cr.id\n  FROM federated.competition_round cr\n  WHERE cr.competition_id = :competitionId\n    AND NOT EXISTS (\n      SELECT 1\n      FROM input\n      WHERE input.round_key = cr.round_key\n    )\n), deleted_rounds AS (\n  DELETE FROM federated.competition_round cr\n  USING stale_rounds\n  WHERE cr.id = stale_rounds.id\n), upserted AS (\n  INSERT INTO federated.competition_round (\n    competition_id, round_key, round_label, round_index, dance_program_id, scoring_method\n  )\n  SELECT\n    :competitionId, round_key, round_label, round_index, dance_program_id, scoring_method\n  FROM input\n  ON CONFLICT (competition_id, round_key) DO UPDATE\n    SET round_label = EXCLUDED.round_label,\n        round_index = EXCLUDED.round_index,\n        dance_program_id = EXCLUDED.dance_program_id,\n        scoring_method = EXCLUDED.scoring_method\n  RETURNING id, round_key\n)\nSELECT id AS \"id!\", round_key AS \"roundKey!\"\nFROM upserted"};

/**
 * Query generated from SQL:
 * ```
 * WITH input AS (
 *   SELECT *
 *   FROM unnest(
 *     :roundKey::text[],
 *     :roundLabel::text[],
 *     :roundIndex::int[],
 *     :danceProgramId::bigint[],
 *     :scoringMethod::federated.scoring_method[]
 *   ) AS input(round_key, round_label, round_index, dance_program_id, scoring_method)
 * ), stale_rounds AS (
 *   SELECT cr.id
 *   FROM federated.competition_round cr
 *   WHERE cr.competition_id = :competitionId
 *     AND NOT EXISTS (
 *       SELECT 1
 *       FROM input
 *       WHERE input.round_key = cr.round_key
 *     )
 * ), deleted_rounds AS (
 *   DELETE FROM federated.competition_round cr
 *   USING stale_rounds
 *   WHERE cr.id = stale_rounds.id
 * ), upserted AS (
 *   INSERT INTO federated.competition_round (
 *     competition_id, round_key, round_label, round_index, dance_program_id, scoring_method
 *   )
 *   SELECT
 *     :competitionId, round_key, round_label, round_index, dance_program_id, scoring_method
 *   FROM input
 *   ON CONFLICT (competition_id, round_key) DO UPDATE
 *     SET round_label = EXCLUDED.round_label,
 *         round_index = EXCLUDED.round_index,
 *         dance_program_id = EXCLUDED.dance_program_id,
 *         scoring_method = EXCLUDED.scoring_method
 *   RETURNING id, round_key
 * )
 * SELECT id AS "id!", round_key AS "roundKey!"
 * FROM upserted
 * ```
 */
export const upsertCompetitionRounds = new PreparedQuery<IUpsertCompetitionRoundsParams,IUpsertCompetitionRoundsResult>(upsertCompetitionRoundsIR);


/** 'ClearRoundDetails' parameters type */
export interface IClearRoundDetailsParams {
  roundId?: NumberOrStringArray | null | void;
}

/** 'ClearRoundDetails' return type */
export type IClearRoundDetailsResult = void;

/** 'ClearRoundDetails' query type */
export interface IClearRoundDetailsQuery {
  params: IClearRoundDetailsParams;
  result: IClearRoundDetailsResult;
}

const clearRoundDetailsIR: any = {"usedParamSet":{"roundId":true},"params":[{"name":"roundId","required":false,"transform":{"type":"scalar"},"locs":[{"a":33,"b":40}]}],"statement":"WITH rounds AS (\n  SELECT unnest(:roundId::bigint[]) AS id\n), deleted_judges AS (\n  DELETE FROM federated.competition_round_judge crj\n  USING rounds\n  WHERE crj.round_id = rounds.id\n), deleted_results AS (\n  DELETE FROM federated.competition_round_result crr\n  USING rounds\n  WHERE crr.round_id = rounds.id\n)\nDELETE FROM federated.round_dance rd\nUSING rounds\nWHERE rd.round_id = rounds.id"};

/**
 * Query generated from SQL:
 * ```
 * WITH rounds AS (
 *   SELECT unnest(:roundId::bigint[]) AS id
 * ), deleted_judges AS (
 *   DELETE FROM federated.competition_round_judge crj
 *   USING rounds
 *   WHERE crj.round_id = rounds.id
 * ), deleted_results AS (
 *   DELETE FROM federated.competition_round_result crr
 *   USING rounds
 *   WHERE crr.round_id = rounds.id
 * )
 * DELETE FROM federated.round_dance rd
 * USING rounds
 * WHERE rd.round_id = rounds.id
 * ```
 */
export const clearRoundDetails = new PreparedQuery<IClearRoundDetailsParams,IClearRoundDetailsResult>(clearRoundDetailsIR);


/** 'InsertRoundDetails' parameters type */
export interface IInsertRoundDetailsParams {
  danceCode?: stringArray | null | void;
  danceOrder?: numberArray | null | void;
  danceResults?: stringArray | null | void;
  danceRoundId?: NumberOrStringArray | null | void;
  judgeIndex?: numberArray | null | void;
  judgeLabel?: stringArray | null | void;
  judgeRoundId?: NumberOrStringArray | null | void;
  overallRanking?: numberArray | null | void;
  overallRankingTo?: numberArray | null | void;
  overallScore?: NumberOrStringArray | null | void;
  personJudgeId?: stringArray | null | void;
  qualifiedNext?: booleanArray | null | void;
  rawScore?: stringArray | null | void;
  resultCompetitorId?: stringArray | null | void;
  resultRoundId?: NumberOrStringArray | null | void;
  score?: NumberOrStringArray | null | void;
  scoreCategoryId?: NumberOrString | null | void;
  scoreCompetitionId?: NumberOrString | null | void;
  scoreCompetitorId?: NumberOrStringArray | null | void;
  scoreComponent?: score_componentArray | null | void;
  scoreDanceCode?: stringArray | null | void;
  scoreDanceOrder?: numberArray | null | void;
  scoreEventDate?: DateOrString | null | void;
  scoreEventId?: NumberOrString | null | void;
  scoreFederation?: string | null | void;
  scoreJudgePersonId?: NumberOrStringArray | null | void;
  scoreRoundId?: NumberOrStringArray | null | void;
}

/** 'InsertRoundDetails' return type */
export type IInsertRoundDetailsResult = void;

/** 'InsertRoundDetails' query type */
export interface IInsertRoundDetailsQuery {
  params: IInsertRoundDetailsParams;
  result: IInsertRoundDetailsResult;
}

const insertRoundDetailsIR: any = {"usedParamSet":{"danceRoundId":true,"danceCode":true,"danceOrder":true,"judgeRoundId":true,"personJudgeId":true,"judgeIndex":true,"judgeLabel":true,"resultRoundId":true,"resultCompetitorId":true,"overallRanking":true,"overallRankingTo":true,"qualifiedNext":true,"overallScore":true,"danceResults":true,"scoreFederation":true,"scoreEventDate":true,"scoreEventId":true,"scoreCompetitionId":true,"scoreCategoryId":true,"scoreRoundId":true,"scoreDanceOrder":true,"scoreDanceCode":true,"scoreJudgePersonId":true,"scoreCompetitorId":true,"scoreComponent":true,"score":true,"rawScore":true},"params":[{"name":"danceRoundId","required":false,"transform":{"type":"scalar"},"locs":[{"a":217,"b":229}]},{"name":"danceCode","required":false,"transform":{"type":"scalar"},"locs":[{"a":246,"b":255}]},{"name":"danceOrder","required":false,"transform":{"type":"scalar"},"locs":[{"a":270,"b":280}]},{"name":"judgeRoundId","required":false,"transform":{"type":"scalar"},"locs":[{"a":627,"b":639}]},{"name":"personJudgeId","required":false,"transform":{"type":"scalar"},"locs":[{"a":656,"b":669}]},{"name":"judgeIndex","required":false,"transform":{"type":"scalar"},"locs":[{"a":684,"b":694}]},{"name":"judgeLabel","required":false,"transform":{"type":"scalar"},"locs":[{"a":708,"b":718}]},{"name":"resultRoundId","required":false,"transform":{"type":"scalar"},"locs":[{"a":1218,"b":1231}]},{"name":"resultCompetitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":1248,"b":1266}]},{"name":"overallRanking","required":false,"transform":{"type":"scalar"},"locs":[{"a":1281,"b":1295}]},{"name":"overallRankingTo","required":false,"transform":{"type":"scalar"},"locs":[{"a":1309,"b":1325}]},{"name":"qualifiedNext","required":false,"transform":{"type":"scalar"},"locs":[{"a":1339,"b":1352}]},{"name":"overallScore","required":false,"transform":{"type":"scalar"},"locs":[{"a":1370,"b":1382}]},{"name":"danceResults","required":false,"transform":{"type":"scalar"},"locs":[{"a":1406,"b":1418}]},{"name":"scoreFederation","required":false,"transform":{"type":"scalar"},"locs":[{"a":1768,"b":1783}]},{"name":"scoreEventDate","required":false,"transform":{"type":"scalar"},"locs":[{"a":1792,"b":1806}]},{"name":"scoreEventId","required":false,"transform":{"type":"scalar"},"locs":[{"a":1815,"b":1827}]},{"name":"scoreCompetitionId","required":false,"transform":{"type":"scalar"},"locs":[{"a":1838,"b":1856}]},{"name":"scoreCategoryId","required":false,"transform":{"type":"scalar"},"locs":[{"a":1869,"b":1884}]},{"name":"scoreRoundId","required":false,"transform":{"type":"scalar"},"locs":[{"a":2007,"b":2019}]},{"name":"scoreDanceOrder","required":false,"transform":{"type":"scalar"},"locs":[{"a":2034,"b":2049}]},{"name":"scoreDanceCode","required":false,"transform":{"type":"scalar"},"locs":[{"a":2061,"b":2075}]},{"name":"scoreJudgePersonId","required":false,"transform":{"type":"scalar"},"locs":[{"a":2088,"b":2106}]},{"name":"scoreCompetitorId","required":false,"transform":{"type":"scalar"},"locs":[{"a":2121,"b":2138}]},{"name":"scoreComponent","required":false,"transform":{"type":"scalar"},"locs":[{"a":2153,"b":2167}]},{"name":"score","required":false,"transform":{"type":"scalar"},"locs":[{"a":2201,"b":2206}]},{"name":"rawScore","required":false,"transform":{"type":"scalar"},"locs":[{"a":2228,"b":2236}]}],"statement":"WITH inserted_dances AS (\n  INSERT INTO federated.round_dance (round_id, dance_program_id, dance_code, dance_order)\n  SELECT input.round_id, cr.dance_program_id, input.dance_code, input.dance_order\n  FROM unnest(\n    :danceRoundId::bigint[],\n    :danceCode::text[],\n    :danceOrder::int[]\n  ) AS input(round_id, dance_code, dance_order)\n  JOIN federated.competition_round cr ON cr.id = input.round_id\n), inserted_judges AS (\n  INSERT INTO federated.competition_round_judge (\n    round_id, person_judge_id, judge_index, judge_label\n  )\n  SELECT round_id, person_judge_id, judge_index, nullif(judge_label, '')\n  FROM unnest(\n    :judgeRoundId::bigint[],\n    :personJudgeId::text[],\n    :judgeIndex::int[],\n    :judgeLabel::text[]\n  ) AS input(round_id, person_judge_id, judge_index, judge_label)\n), inserted_results AS (\n  INSERT INTO federated.competition_round_result (\n    round_id, competitor_id, overall_ranking, overall_ranking_to, qualified_next, overall_score, dance_results\n  )\n  SELECT\n    round_id, competitor_id, overall_ranking, overall_ranking_to, qualified_next, overall_score, CASE\n      WHEN dance_results = '' THEN NULL\n      ELSE string_to_array(dance_results, ',')::real[]\n    END\n  FROM unnest(\n    :resultRoundId::bigint[],\n    :resultCompetitorId::text[],\n    :overallRanking::int[],\n    :overallRankingTo::int[],\n    :qualifiedNext::boolean[],\n    :overallScore::numeric(10,3)[],\n    :danceResults::text[]\n  ) AS input(\n    round_id, competitor_id, overall_ranking, overall_ranking_to, qualified_next, overall_score, dance_results\n  )\n)\nINSERT INTO federated.judge_score (\n  federation, event_date, event_id, competition_id, category_id, round_id, dance_order, dance_code,\n  judge_person_id, competitor_id, component, score, raw_score\n)\nSELECT\n  :scoreFederation::text, :scoreEventDate::date, :scoreEventId::bigint, :scoreCompetitionId::bigint,\n  :scoreCategoryId::bigint, round_id, dance_order, dance_code,\n  judge_person_id, competitor_id, component, score, raw_score\nFROM unnest(\n  :scoreRoundId::bigint[],\n  :scoreDanceOrder::int[],\n  :scoreDanceCode::text[],\n  :scoreJudgePersonId::bigint[],\n  :scoreCompetitorId::bigint[],\n  :scoreComponent::federated.score_component[],\n  :score::numeric(10,3)[],\n  :rawScore::text[]\n) AS input(\n  round_id, dance_order, dance_code, judge_person_id, competitor_id, component, score, raw_score\n)"};

/**
 * Query generated from SQL:
 * ```
 * WITH inserted_dances AS (
 *   INSERT INTO federated.round_dance (round_id, dance_program_id, dance_code, dance_order)
 *   SELECT input.round_id, cr.dance_program_id, input.dance_code, input.dance_order
 *   FROM unnest(
 *     :danceRoundId::bigint[],
 *     :danceCode::text[],
 *     :danceOrder::int[]
 *   ) AS input(round_id, dance_code, dance_order)
 *   JOIN federated.competition_round cr ON cr.id = input.round_id
 * ), inserted_judges AS (
 *   INSERT INTO federated.competition_round_judge (
 *     round_id, person_judge_id, judge_index, judge_label
 *   )
 *   SELECT round_id, person_judge_id, judge_index, nullif(judge_label, '')
 *   FROM unnest(
 *     :judgeRoundId::bigint[],
 *     :personJudgeId::text[],
 *     :judgeIndex::int[],
 *     :judgeLabel::text[]
 *   ) AS input(round_id, person_judge_id, judge_index, judge_label)
 * ), inserted_results AS (
 *   INSERT INTO federated.competition_round_result (
 *     round_id, competitor_id, overall_ranking, overall_ranking_to, qualified_next, overall_score, dance_results
 *   )
 *   SELECT
 *     round_id, competitor_id, overall_ranking, overall_ranking_to, qualified_next, overall_score, CASE
 *       WHEN dance_results = '' THEN NULL
 *       ELSE string_to_array(dance_results, ',')::real[]
 *     END
 *   FROM unnest(
 *     :resultRoundId::bigint[],
 *     :resultCompetitorId::text[],
 *     :overallRanking::int[],
 *     :overallRankingTo::int[],
 *     :qualifiedNext::boolean[],
 *     :overallScore::numeric(10,3)[],
 *     :danceResults::text[]
 *   ) AS input(
 *     round_id, competitor_id, overall_ranking, overall_ranking_to, qualified_next, overall_score, dance_results
 *   )
 * )
 * INSERT INTO federated.judge_score (
 *   federation, event_date, event_id, competition_id, category_id, round_id, dance_order, dance_code,
 *   judge_person_id, competitor_id, component, score, raw_score
 * )
 * SELECT
 *   :scoreFederation::text, :scoreEventDate::date, :scoreEventId::bigint, :scoreCompetitionId::bigint,
 *   :scoreCategoryId::bigint, round_id, dance_order, dance_code,
 *   judge_person_id, competitor_id, component, score, raw_score
 * FROM unnest(
 *   :scoreRoundId::bigint[],
 *   :scoreDanceOrder::int[],
 *   :scoreDanceCode::text[],
 *   :scoreJudgePersonId::bigint[],
 *   :scoreCompetitorId::bigint[],
 *   :scoreComponent::federated.score_component[],
 *   :score::numeric(10,3)[],
 *   :rawScore::text[]
 * ) AS input(
 *   round_id, dance_order, dance_code, judge_person_id, competitor_id, component, score, raw_score
 * )
 * ```
 */
export const insertRoundDetails = new PreparedQuery<IInsertRoundDetailsParams,IInsertRoundDetailsResult>(insertRoundDetailsIR);


