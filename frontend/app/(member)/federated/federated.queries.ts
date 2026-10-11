/** Types generated for queries found in "app/(member)/federated/federated.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type competition_type = 'championship' | 'cup' | 'g_cup' | 'league' | 'ranking' | 'super_league' | 'top_level' | 'unknown';

export type competitor_role = 'follow' | 'lead' | 'member' | 'substitute';

export type competitor_type = 'couple' | 'duo' | 'formation' | 'group' | 'solo' | 'team' | 'trio';

export type fetch_status = 'error' | 'gone' | 'ok' | 'pending' | 'transient';

export type process_status = 'error' | 'ok' | 'pending';

export type scoring_method = 'ajs-3.0' | 'skating_marks' | 'skating_places';

export type NumberOrString = number | string;

export type stringArray = (string)[];

/** 'BrowseCompetitions' parameters type */
export interface IBrowseCompetitionsParams {
  eventId: NumberOrString;
}

/** 'BrowseCompetitions' return type */
export interface IBrowseCompetitionsResult {
  category: string;
  check_in_end: string | null;
  /** Federation-provided competition type/grade, for example CSTS Cup, Ranking, League, Championship. */
  competition_type: competition_type | null;
  external_id: string;
  id: string;
  registrations: number | null;
  results: number | null;
  start_date: Date;
}

/** 'BrowseCompetitions' query type */
export interface IBrowseCompetitionsQuery {
  params: IBrowseCompetitionsParams;
  result: IBrowseCompetitionsResult;
}

const browseCompetitionsIR: any = {"usedParamSet":{"eventId":true},"params":[{"name":"eventId","required":true,"transform":{"type":"scalar"},"locs":[{"a":443,"b":451}]}],"statement":"SELECT c.id, c.external_id, c.start_date, c.check_in_end::text, c.competition_type, cat.name AS category,\n  (SELECT count(*)::int FROM federated.competition_entry ce WHERE ce.competition_id = c.id AND NOT ce.cancelled) AS registrations,\n  (SELECT count(*)::int FROM federated.competition_result cr WHERE cr.competition_id = c.id) AS results\nFROM federated.competition c JOIN federated.category cat ON cat.id = c.category_id\nWHERE c.event_id = :eventId!\nORDER BY c.start_date, c.check_in_end NULLS LAST, cat.name, c.id"};

/**
 * Query generated from SQL:
 * ```
 * SELECT c.id, c.external_id, c.start_date, c.check_in_end::text, c.competition_type, cat.name AS category,
 *   (SELECT count(*)::int FROM federated.competition_entry ce WHERE ce.competition_id = c.id AND NOT ce.cancelled) AS registrations,
 *   (SELECT count(*)::int FROM federated.competition_result cr WHERE cr.competition_id = c.id) AS results
 * FROM federated.competition c JOIN federated.category cat ON cat.id = c.category_id
 * WHERE c.event_id = :eventId!
 * ORDER BY c.start_date, c.check_in_end NULLS LAST, cat.name, c.id
 * ```
 */
export const browseCompetitions = new PreparedQuery<IBrowseCompetitionsParams,IBrowseCompetitionsResult>(browseCompetitionsIR);


/** 'BrowseParticipants' parameters type */
export interface IBrowseParticipantsParams {
  competitionId: NumberOrString;
}

/** 'BrowseParticipants' return type */
export interface IBrowseParticipantsResult {
  cancelled: boolean | null;
  competitor_external_id: string;
  competitor_id: string;
  completion_status: string | null;
  final_gain: string | null;
  name: string | null;
  point_gain: string | null;
  ranking: number | null;
  ranking_to: number | null;
  start_number: string | null;
}

/** 'BrowseParticipants' query type */
export interface IBrowseParticipantsQuery {
  params: IBrowseParticipantsParams;
  result: IBrowseParticipantsResult;
}

const browseParticipantsIR: any = {"usedParamSet":{"competitionId":true},"params":[{"name":"competitionId","required":true,"transform":{"type":"scalar"},"locs":[{"a":115,"b":129},{"a":215,"b":229}]}],"statement":"WITH participants AS (\n  SELECT competitor_id, cancelled\n  FROM federated.competition_entry WHERE competition_id = :competitionId!\n), results AS (\n  SELECT * FROM federated.competition_result WHERE competition_id = :competitionId!\n)\nSELECT c.id AS competitor_id, c.external_id AS competitor_external_id, c.name,\n  p.cancelled AS \"cancelled?\", r.ranking AS \"ranking?\", r.ranking_to, r.start_number, r.point_gain, r.final_gain, r.completion_status\nFROM participants p FULL JOIN results r USING (competitor_id)\nJOIN federated.competitor c ON c.id = coalesce(p.competitor_id, r.competitor_id)\nORDER BY r.ranking NULLS LAST, c.name, c.id"};

/**
 * Query generated from SQL:
 * ```
 * WITH participants AS (
 *   SELECT competitor_id, cancelled
 *   FROM federated.competition_entry WHERE competition_id = :competitionId!
 * ), results AS (
 *   SELECT * FROM federated.competition_result WHERE competition_id = :competitionId!
 * )
 * SELECT c.id AS competitor_id, c.external_id AS competitor_external_id, c.name,
 *   p.cancelled AS "cancelled?", r.ranking AS "ranking?", r.ranking_to, r.start_number, r.point_gain, r.final_gain, r.completion_status
 * FROM participants p FULL JOIN results r USING (competitor_id)
 * JOIN federated.competitor c ON c.id = coalesce(p.competitor_id, r.competitor_id)
 * ORDER BY r.ranking NULLS LAST, c.name, c.id
 * ```
 */
export const browseParticipants = new PreparedQuery<IBrowseParticipantsParams,IBrowseParticipantsResult>(browseParticipantsIR);


/** 'BrowseParticipantPeople' parameters type */
export interface IBrowseParticipantPeopleParams {
  competitionId: NumberOrString;
}

/** 'BrowseParticipantPeople' return type */
export interface IBrowseParticipantPeopleResult {
  canonical_name: string | null;
  competitor_id: string;
  couple_ids: stringArray | null;
  federated_person_id: string;
  person_external_id: string;
  person_id: string;
  person_name: string;
  tenants: stringArray | null;
}

/** 'BrowseParticipantPeople' query type */
export interface IBrowseParticipantPeopleQuery {
  params: IBrowseParticipantPeopleParams;
  result: IBrowseParticipantPeopleResult;
}

const browseParticipantPeopleIR: any = {"usedParamSet":{"competitionId":true},"params":[{"name":"competitionId","required":true,"transform":{"type":"scalar"},"locs":[{"a":102,"b":116},{"a":206,"b":220}]}],"statement":"WITH participants AS (\n  SELECT competitor_id FROM federated.competition_entry WHERE competition_id = :competitionId!\n  UNION\n  SELECT competitor_id FROM federated.competition_result WHERE competition_id = :competitionId!\n), mapped AS (\n  SELECT cc.competitor_id, fp.id AS federated_person_id, fp.external_id AS person_external_id, fp.canonical_name,\n    p.id AS person_id, p.name AS person_name,\n    ARRAY(SELECT t.name FROM public.tenant_membership tm\n      JOIN public.tenant t ON t.id = tm.tenant_id\n      WHERE tm.person_id = p.id AND tm.status = 'active'\n      ORDER BY t.name, t.id) AS tenants\n  FROM participants cp\n  JOIN federated.competitor_component cc ON cc.competitor_id = cp.competitor_id\n  JOIN federated.person fp ON fp.id = cc.person_id\n  LEFT JOIN public.person p ON fp.external_id <> 0 AND (\n    (fp.federation = 'csts' AND p.csts_id = fp.external_id) OR\n    (fp.federation = 'wdsf' AND p.wdsf_id = fp.external_id)\n  )\n)\nSELECT m.*, (\n  SELECT array_agg(c.id ORDER BY c.id)\n  FROM public.couple c\n  WHERE c.status = 'active' AND m.person_id IN (c.man_id, c.woman_id)\n    AND EXISTS (SELECT 1 FROM mapped partner WHERE partner.competitor_id = m.competitor_id\n      AND partner.person_id = CASE WHEN m.person_id = c.man_id THEN c.woman_id ELSE c.man_id END)\n) AS couple_ids\nFROM mapped m\nORDER BY competitor_id, federated_person_id, person_id"};

/**
 * Query generated from SQL:
 * ```
 * WITH participants AS (
 *   SELECT competitor_id FROM federated.competition_entry WHERE competition_id = :competitionId!
 *   UNION
 *   SELECT competitor_id FROM federated.competition_result WHERE competition_id = :competitionId!
 * ), mapped AS (
 *   SELECT cc.competitor_id, fp.id AS federated_person_id, fp.external_id AS person_external_id, fp.canonical_name,
 *     p.id AS person_id, p.name AS person_name,
 *     ARRAY(SELECT t.name FROM public.tenant_membership tm
 *       JOIN public.tenant t ON t.id = tm.tenant_id
 *       WHERE tm.person_id = p.id AND tm.status = 'active'
 *       ORDER BY t.name, t.id) AS tenants
 *   FROM participants cp
 *   JOIN federated.competitor_component cc ON cc.competitor_id = cp.competitor_id
 *   JOIN federated.person fp ON fp.id = cc.person_id
 *   LEFT JOIN public.person p ON fp.external_id <> 0 AND (
 *     (fp.federation = 'csts' AND p.csts_id = fp.external_id) OR
 *     (fp.federation = 'wdsf' AND p.wdsf_id = fp.external_id)
 *   )
 * )
 * SELECT m.*, (
 *   SELECT array_agg(c.id ORDER BY c.id)
 *   FROM public.couple c
 *   WHERE c.status = 'active' AND m.person_id IN (c.man_id, c.woman_id)
 *     AND EXISTS (SELECT 1 FROM mapped partner WHERE partner.competitor_id = m.competitor_id
 *       AND partner.person_id = CASE WHEN m.person_id = c.man_id THEN c.woman_id ELSE c.man_id END)
 * ) AS couple_ids
 * FROM mapped m
 * ORDER BY competitor_id, federated_person_id, person_id
 * ```
 */
export const browseParticipantPeople = new PreparedQuery<IBrowseParticipantPeopleParams,IBrowseParticipantPeopleResult>(browseParticipantPeopleIR);


/** 'BrowseRounds' parameters type */
export interface IBrowseRoundsParams {
  competitionId: NumberOrString;
}

/** 'BrowseRounds' return type */
export interface IBrowseRoundsResult {
  id: string;
  results: number | null;
  round_key: string;
  round_label: string | null;
  scoring_method: scoring_method;
}

/** 'BrowseRounds' query type */
export interface IBrowseRoundsQuery {
  params: IBrowseRoundsParams;
  result: IBrowseRoundsResult;
}

const browseRoundsIR: any = {"usedParamSet":{"competitionId":true},"params":[{"name":"competitionId","required":true,"transform":{"type":"scalar"},"locs":[{"a":221,"b":235}]}],"statement":"SELECT r.id, r.round_key, r.round_label, r.scoring_method,\n  (SELECT count(*)::int FROM federated.competition_round_result rr WHERE rr.round_id = r.id) AS results\nFROM federated.competition_round r WHERE competition_id = :competitionId!\nORDER BY round_index NULLS LAST, id"};

/**
 * Query generated from SQL:
 * ```
 * SELECT r.id, r.round_key, r.round_label, r.scoring_method,
 *   (SELECT count(*)::int FROM federated.competition_round_result rr WHERE rr.round_id = r.id) AS results
 * FROM federated.competition_round r WHERE competition_id = :competitionId!
 * ORDER BY round_index NULLS LAST, id
 * ```
 */
export const browseRounds = new PreparedQuery<IBrowseRoundsParams,IBrowseRoundsResult>(browseRoundsIR);


/** 'BrowseSourceFrontiers' parameters type */
export interface IBrowseSourceFrontiersParams {
  competitionKey?: string | null | void;
  eventKey: string;
  federation: string;
}

/** 'BrowseSourceFrontiers' return type */
export interface IBrowseSourceFrontiersResult {
  fetch_status: fetch_status;
  id: string;
  key: string;
  kind: string;
  process_status: process_status;
}

/** 'BrowseSourceFrontiers' query type */
export interface IBrowseSourceFrontiersQuery {
  params: IBrowseSourceFrontiersParams;
  result: IBrowseSourceFrontiersResult;
}

const browseSourceFrontiersIR: any = {"usedParamSet":{"federation":true,"eventKey":true,"competitionKey":true},"params":[{"name":"federation","required":true,"transform":{"type":"scalar"},"locs":[{"a":92,"b":103},{"a":114,"b":125},{"a":277,"b":288},{"a":340,"b":351}]},{"name":"eventKey","required":true,"transform":{"type":"scalar"},"locs":[{"a":188,"b":197}]},{"name":"competitionKey","required":false,"transform":{"type":"scalar"},"locs":[{"a":206,"b":220},{"a":250,"b":264}]}],"statement":"SELECT id, kind, key, fetch_status, process_status\nFROM crawler.frontier\nWHERE federation = :federation! AND (\n  (:federation! = 'csts' AND kind IN ('event', 'eventCompetitors') AND key = :eventKey!) OR\n  (:competitionKey::text IS NOT NULL AND key = :competitionKey AND (\n    (:federation! = 'csts' AND kind = 'competitionResults') OR\n    (:federation! = 'wdsf' AND kind IN ('competition', 'participantIndex', 'officialIndex'))\n  ))\n)\nORDER BY kind, key"};

/**
 * Query generated from SQL:
 * ```
 * SELECT id, kind, key, fetch_status, process_status
 * FROM crawler.frontier
 * WHERE federation = :federation! AND (
 *   (:federation! = 'csts' AND kind IN ('event', 'eventCompetitors') AND key = :eventKey!) OR
 *   (:competitionKey::text IS NOT NULL AND key = :competitionKey AND (
 *     (:federation! = 'csts' AND kind = 'competitionResults') OR
 *     (:federation! = 'wdsf' AND kind IN ('competition', 'participantIndex', 'officialIndex'))
 *   ))
 * )
 * ORDER BY kind, key
 * ```
 */
export const browseSourceFrontiers = new PreparedQuery<IBrowseSourceFrontiersParams,IBrowseSourceFrontiersResult>(browseSourceFrontiersIR);


/** 'BrowseCompetitorMembers' parameters type */
export interface IBrowseCompetitorMembersParams {
  id: string;
}

/** 'BrowseCompetitorMembers' return type */
export interface IBrowseCompetitorMembersResult {
  external_id: string;
  id: string;
  name: string | null;
  role: competitor_role;
}

/** 'BrowseCompetitorMembers' query type */
export interface IBrowseCompetitorMembersQuery {
  params: IBrowseCompetitorMembersParams;
  result: IBrowseCompetitorMembersResult;
}

const browseCompetitorMembersIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":true,"transform":{"type":"scalar"},"locs":[{"a":244,"b":247}]}],"statement":"SELECT p.id, p.external_id, coalesce(p.canonical_name, nullif(concat_ws(' ', p.first_name, p.last_name), ''), p.id) AS name, cc.role\nFROM federated.competitor_component cc JOIN federated.person p ON p.id = cc.person_id\nWHERE cc.competitor_id = :id!\nORDER BY cc.role, name, p.id"};

/**
 * Query generated from SQL:
 * ```
 * SELECT p.id, p.external_id, coalesce(p.canonical_name, nullif(concat_ws(' ', p.first_name, p.last_name), ''), p.id) AS name, cc.role
 * FROM federated.competitor_component cc JOIN federated.person p ON p.id = cc.person_id
 * WHERE cc.competitor_id = :id!
 * ORDER BY cc.role, name, p.id
 * ```
 */
export const browseCompetitorMembers = new PreparedQuery<IBrowseCompetitorMembersParams,IBrowseCompetitorMembersResult>(browseCompetitorMembersIR);


/** 'BrowseCompetitorHistoryCount' parameters type */
export interface IBrowseCompetitorHistoryCountParams {
  id: string;
}

/** 'BrowseCompetitorHistoryCount' return type */
export interface IBrowseCompetitorHistoryCountResult {
  total: number | null;
}

/** 'BrowseCompetitorHistoryCount' query type */
export interface IBrowseCompetitorHistoryCountQuery {
  params: IBrowseCompetitorHistoryCountParams;
  result: IBrowseCompetitorHistoryCountResult;
}

const browseCompetitorHistoryCountIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":true,"transform":{"type":"scalar"},"locs":[{"a":116,"b":119},{"a":209,"b":212}]}],"statement":"SELECT count(*)::int AS total FROM (\n  SELECT competition_id FROM federated.competition_entry WHERE competitor_id = :id!\n  UNION\n  SELECT competition_id FROM federated.competition_result WHERE competitor_id = :id!\n) history"};

/**
 * Query generated from SQL:
 * ```
 * SELECT count(*)::int AS total FROM (
 *   SELECT competition_id FROM federated.competition_entry WHERE competitor_id = :id!
 *   UNION
 *   SELECT competition_id FROM federated.competition_result WHERE competitor_id = :id!
 * ) history
 * ```
 */
export const browseCompetitorHistoryCount = new PreparedQuery<IBrowseCompetitorHistoryCountParams,IBrowseCompetitorHistoryCountResult>(browseCompetitorHistoryCountIR);


/** 'BrowseCompetitorHistory' parameters type */
export interface IBrowseCompetitorHistoryParams {
  id: string;
  offset: NumberOrString;
}

/** 'BrowseCompetitorHistory' return type */
export interface IBrowseCompetitorHistoryResult {
  cancelled: boolean | null;
  category: string;
  event_external_id: string;
  event_id: string;
  event_name: string | null;
  final_gain: string | null;
  id: string;
  point_gain: string | null;
  ranking: number | null;
  ranking_to: number | null;
  start_date: Date;
}

/** 'BrowseCompetitorHistory' query type */
export interface IBrowseCompetitorHistoryQuery {
  params: IBrowseCompetitorHistoryParams;
  result: IBrowseCompetitorHistoryResult;
}

const browseCompetitorHistoryIR: any = {"usedParamSet":{"id":true,"offset":true},"params":[{"name":"id","required":true,"transform":{"type":"scalar"},"locs":[{"a":108,"b":111},{"a":196,"b":199}]},{"name":"offset","required":true,"transform":{"type":"scalar"},"locs":[{"a":722,"b":729}]}],"statement":"WITH entries AS (\n  SELECT competition_id, cancelled FROM federated.competition_entry WHERE competitor_id = :id!\n), results AS (\n  SELECT * FROM federated.competition_result WHERE competitor_id = :id!\n)\nSELECT c.id, c.event_id, c.start_date, e.name AS event_name, e.external_id AS event_external_id,\n  cat.name AS category, ce.cancelled AS \"cancelled?\", cr.ranking AS \"ranking?\", cr.ranking_to, cr.point_gain, cr.final_gain\nFROM entries ce FULL JOIN results cr USING (competition_id)\nJOIN federated.competition c ON c.id = coalesce(ce.competition_id, cr.competition_id)\nJOIN federated.event e ON e.id = c.event_id\nJOIN federated.category cat ON cat.id = c.category_id\nORDER BY c.start_date DESC, c.id DESC\nLIMIT 50 OFFSET :offset!"};

/**
 * Query generated from SQL:
 * ```
 * WITH entries AS (
 *   SELECT competition_id, cancelled FROM federated.competition_entry WHERE competitor_id = :id!
 * ), results AS (
 *   SELECT * FROM federated.competition_result WHERE competitor_id = :id!
 * )
 * SELECT c.id, c.event_id, c.start_date, e.name AS event_name, e.external_id AS event_external_id,
 *   cat.name AS category, ce.cancelled AS "cancelled?", cr.ranking AS "ranking?", cr.ranking_to, cr.point_gain, cr.final_gain
 * FROM entries ce FULL JOIN results cr USING (competition_id)
 * JOIN federated.competition c ON c.id = coalesce(ce.competition_id, cr.competition_id)
 * JOIN federated.event e ON e.id = c.event_id
 * JOIN federated.category cat ON cat.id = c.category_id
 * ORDER BY c.start_date DESC, c.id DESC
 * LIMIT 50 OFFSET :offset!
 * ```
 */
export const browseCompetitorHistory = new PreparedQuery<IBrowseCompetitorHistoryParams,IBrowseCompetitorHistoryResult>(browseCompetitorHistoryIR);


/** 'BrowsePersonCompetitors' parameters type */
export interface IBrowsePersonCompetitorsParams {
  id: string;
}

/** 'BrowsePersonCompetitors' return type */
export interface IBrowsePersonCompetitorsResult {
  competitor_type: competitor_type;
  external_id: string;
  id: string;
  name: string | null;
  role: competitor_role;
}

/** 'BrowsePersonCompetitors' query type */
export interface IBrowsePersonCompetitorsQuery {
  params: IBrowsePersonCompetitorsParams;
  result: IBrowsePersonCompetitorsResult;
}

const browsePersonCompetitorsIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":true,"transform":{"type":"scalar"},"locs":[{"a":178,"b":181}]}],"statement":"SELECT c.id, c.external_id, c.name, c.competitor_type, cc.role\nFROM federated.competitor_component cc JOIN federated.competitor c ON c.id = cc.competitor_id\nWHERE cc.person_id = :id!\nORDER BY c.name, c.id"};

/**
 * Query generated from SQL:
 * ```
 * SELECT c.id, c.external_id, c.name, c.competitor_type, cc.role
 * FROM federated.competitor_component cc JOIN federated.competitor c ON c.id = cc.competitor_id
 * WHERE cc.person_id = :id!
 * ORDER BY c.name, c.id
 * ```
 */
export const browsePersonCompetitors = new PreparedQuery<IBrowsePersonCompetitorsParams,IBrowsePersonCompetitorsResult>(browsePersonCompetitorsIR);


/** 'BrowsePersonFrontiers' parameters type */
export interface IBrowsePersonFrontiersParams {
  federation: string;
  key: string;
}

/** 'BrowsePersonFrontiers' return type */
export interface IBrowsePersonFrontiersResult {
  fetch_status: fetch_status;
  id: string;
  key: string;
  kind: string;
  process_status: process_status;
}

/** 'BrowsePersonFrontiers' query type */
export interface IBrowsePersonFrontiersQuery {
  params: IBrowsePersonFrontiersParams;
  result: IBrowsePersonFrontiersResult;
}

const browsePersonFrontiersIR: any = {"usedParamSet":{"federation":true,"key":true},"params":[{"name":"federation","required":true,"transform":{"type":"scalar"},"locs":[{"a":92,"b":103}]},{"name":"key","required":true,"transform":{"type":"scalar"},"locs":[{"a":135,"b":139}]}],"statement":"SELECT id, kind, key, fetch_status, process_status\nFROM crawler.frontier\nWHERE federation = :federation! AND kind = 'member' AND key = :key!\nORDER BY id"};

/**
 * Query generated from SQL:
 * ```
 * SELECT id, kind, key, fetch_status, process_status
 * FROM crawler.frontier
 * WHERE federation = :federation! AND kind = 'member' AND key = :key!
 * ORDER BY id
 * ```
 */
export const browsePersonFrontiers = new PreparedQuery<IBrowsePersonFrontiersParams,IBrowsePersonFrontiersResult>(browsePersonFrontiersIR);


