/** Types generated for queries found in "app/(member)/admin/federated/federated.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type competition_type = 'championship' | 'cup' | 'g_cup' | 'league' | 'ranking' | 'super_league' | 'top_level' | 'unknown';

export type fetch_status = 'error' | 'gone' | 'ok' | 'pending' | 'transient';

export type process_status = 'error' | 'ok' | 'pending';

export type scoring_method = 'ajs-3.0' | 'skating_marks' | 'skating_places';

export type NumberOrString = number | string;

export type stringArray = (string)[];

/** 'BrowseFederations' parameters type */
export type IBrowseFederationsParams = void;

/** 'BrowseFederations' return type */
export interface IBrowseFederationsResult {
  code: string;
  competitions: number | null;
  events: number | null;
  name: string;
}

/** 'BrowseFederations' query type */
export interface IBrowseFederationsQuery {
  params: IBrowseFederationsParams;
  result: IBrowseFederationsResult;
}

const browseFederationsIR: any = {"usedParamSet":{},"params":[],"statement":"SELECT f.code, f.name,\n  (SELECT count(*)::int FROM federated.event e WHERE e.federation = f.code) AS events,\n  (SELECT count(*)::int FROM federated.competition c WHERE c.federation = f.code) AS competitions\nFROM federated.federation f ORDER BY f.code"};

/**
 * Query generated from SQL:
 * ```
 * SELECT f.code, f.name,
 *   (SELECT count(*)::int FROM federated.event e WHERE e.federation = f.code) AS events,
 *   (SELECT count(*)::int FROM federated.competition c WHERE c.federation = f.code) AS competitions
 * FROM federated.federation f ORDER BY f.code
 * ```
 */
export const browseFederations = new PreparedQuery<IBrowseFederationsParams,IBrowseFederationsResult>(browseFederationsIR);


/** 'BrowseFederation' parameters type */
export interface IBrowseFederationParams {
  federation: string;
}

/** 'BrowseFederation' return type */
export interface IBrowseFederationResult {
  code: string;
  name: string;
}

/** 'BrowseFederation' query type */
export interface IBrowseFederationQuery {
  params: IBrowseFederationParams;
  result: IBrowseFederationResult;
}

const browseFederationIR: any = {"usedParamSet":{"federation":true},"params":[{"name":"federation","required":true,"transform":{"type":"scalar"},"locs":[{"a":57,"b":68}]}],"statement":"SELECT code, name FROM federated.federation WHERE code = :federation!"};

/**
 * Query generated from SQL:
 * ```
 * SELECT code, name FROM federated.federation WHERE code = :federation!
 * ```
 */
export const browseFederation = new PreparedQuery<IBrowseFederationParams,IBrowseFederationResult>(browseFederationIR);


/** 'BrowseEventCount' parameters type */
export interface IBrowseEventCountParams {
  date: string;
  federation: string;
  search: string;
}

/** 'BrowseEventCount' return type */
export interface IBrowseEventCountResult {
  total: number | null;
}

/** 'BrowseEventCount' query type */
export interface IBrowseEventCountQuery {
  params: IBrowseEventCountParams;
  result: IBrowseEventCountResult;
}

const browseEventCountIR: any = {"usedParamSet":{"federation":true,"search":true,"date":true},"params":[{"name":"federation","required":true,"transform":{"type":"scalar"},"locs":[{"a":72,"b":83}]},{"name":"search","required":true,"transform":{"type":"scalar"},"locs":[{"a":92,"b":99},{"a":180,"b":187}]},{"name":"date","required":true,"transform":{"type":"scalar"},"locs":[{"a":203,"b":208},{"a":234,"b":239}]}],"statement":"SELECT count(*)::int AS total\nFROM federated.event e\nWHERE federation = :federation!\n  AND (:search! = '' OR strpos(lower(concat_ws(' ', name, location, city, external_id)), lower(:search!)) > 0)\n  AND (:date! = '' OR range @> nullif(:date!, '')::date)"};

/**
 * Query generated from SQL:
 * ```
 * SELECT count(*)::int AS total
 * FROM federated.event e
 * WHERE federation = :federation!
 *   AND (:search! = '' OR strpos(lower(concat_ws(' ', name, location, city, external_id)), lower(:search!)) > 0)
 *   AND (:date! = '' OR range @> nullif(:date!, '')::date)
 * ```
 */
export const browseEventCount = new PreparedQuery<IBrowseEventCountParams,IBrowseEventCountResult>(browseEventCountIR);


/** 'BrowseEvents' parameters type */
export interface IBrowseEventsParams {
  date: string;
  federation: string;
  offset: NumberOrString;
  search: string;
}

/** 'BrowseEvents' return type */
export interface IBrowseEventsResult {
  city: string | null;
  competitions: number | null;
  country: string | null;
  end_date: Date | null;
  external_id: string;
  id: string;
  location: string | null;
  name: string | null;
  start_date: Date;
}

/** 'BrowseEvents' query type */
export interface IBrowseEventsQuery {
  params: IBrowseEventsParams;
  result: IBrowseEventsResult;
}

const browseEventsIR: any = {"usedParamSet":{"federation":true,"search":true,"date":true,"offset":true},"params":[{"name":"federation","required":true,"transform":{"type":"scalar"},"locs":[{"a":229,"b":240}]},{"name":"search","required":true,"transform":{"type":"scalar"},"locs":[{"a":249,"b":256},{"a":337,"b":344}]},{"name":"date","required":true,"transform":{"type":"scalar"},"locs":[{"a":360,"b":365},{"a":391,"b":396}]},{"name":"offset","required":true,"transform":{"type":"scalar"},"locs":[{"a":460,"b":467}]}],"statement":"SELECT e.id, e.external_id, e.name, e.start_date, e.end_date, e.location, e.city, e.country,\n  (SELECT count(*)::int FROM federated.competition c WHERE c.event_id = e.id) AS competitions\nFROM federated.event e\nWHERE federation = :federation!\n  AND (:search! = '' OR strpos(lower(concat_ws(' ', name, location, city, external_id)), lower(:search!)) > 0)\n  AND (:date! = '' OR range @> nullif(:date!, '')::date)\nORDER BY start_date DESC, id DESC\nLIMIT 50 OFFSET :offset!"};

/**
 * Query generated from SQL:
 * ```
 * SELECT e.id, e.external_id, e.name, e.start_date, e.end_date, e.location, e.city, e.country,
 *   (SELECT count(*)::int FROM federated.competition c WHERE c.event_id = e.id) AS competitions
 * FROM federated.event e
 * WHERE federation = :federation!
 *   AND (:search! = '' OR strpos(lower(concat_ws(' ', name, location, city, external_id)), lower(:search!)) > 0)
 *   AND (:date! = '' OR range @> nullif(:date!, '')::date)
 * ORDER BY start_date DESC, id DESC
 * LIMIT 50 OFFSET :offset!
 * ```
 */
export const browseEvents = new PreparedQuery<IBrowseEventsParams,IBrowseEventsResult>(browseEventsIR);


/** 'BrowseEvent' parameters type */
export interface IBrowseEventParams {
  eventId: NumberOrString;
  federation: string;
}

/** 'BrowseEvent' return type */
export interface IBrowseEventResult {
  city: string | null;
  country: string | null;
  end_date: Date | null;
  external_id: string;
  federation: string;
  federation_name: string;
  id: string;
  location: string | null;
  name: string | null;
  postal_code: string | null;
  start_date: Date;
  street_address: string | null;
}

/** 'BrowseEvent' query type */
export interface IBrowseEventQuery {
  params: IBrowseEventParams;
  result: IBrowseEventResult;
}

const browseEventIR: any = {"usedParamSet":{"eventId":true,"federation":true},"params":[{"name":"eventId","required":true,"transform":{"type":"scalar"},"locs":[{"a":257,"b":265}]},{"name":"federation","required":true,"transform":{"type":"scalar"},"locs":[{"a":286,"b":297}]}],"statement":"SELECT e.id, e.federation, e.external_id, e.name, e.start_date, e.end_date,\n  e.location, e.street_address, e.city, e.postal_code, e.country, f.name AS federation_name\nFROM federated.event e JOIN federated.federation f ON f.code = e.federation\nWHERE e.id = :eventId! AND e.federation = :federation!"};

/**
 * Query generated from SQL:
 * ```
 * SELECT e.id, e.federation, e.external_id, e.name, e.start_date, e.end_date,
 *   e.location, e.street_address, e.city, e.postal_code, e.country, f.name AS federation_name
 * FROM federated.event e JOIN federated.federation f ON f.code = e.federation
 * WHERE e.id = :eventId! AND e.federation = :federation!
 * ```
 */
export const browseEvent = new PreparedQuery<IBrowseEventParams,IBrowseEventResult>(browseEventIR);


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


/** 'BrowseCompetition' parameters type */
export interface IBrowseCompetitionParams {
  competitionId: NumberOrString;
  eventId: NumberOrString;
}

/** 'BrowseCompetition' return type */
export interface IBrowseCompetitionResult {
  category: string;
  check_in_end: string | null;
  /** Federation-provided competition type/grade, for example CSTS Cup, Ranking, League, Championship. */
  competition_type: competition_type | null;
  completed_at: Date | null;
  external_id: string;
  id: string;
  participants_total: number | null;
  start_date: Date;
}

/** 'BrowseCompetition' query type */
export interface IBrowseCompetitionQuery {
  params: IBrowseCompetitionParams;
  result: IBrowseCompetitionResult;
}

const browseCompetitionIR: any = {"usedParamSet":{"competitionId":true,"eventId":true},"params":[{"name":"competitionId","required":true,"transform":{"type":"scalar"},"locs":[{"a":241,"b":255}]},{"name":"eventId","required":true,"transform":{"type":"scalar"},"locs":[{"a":274,"b":282}]}],"statement":"SELECT c.id, c.external_id, c.start_date, c.check_in_end::text, c.competition_type,\n  c.participants_total, c.completed_at, cat.name AS category\nFROM federated.competition c JOIN federated.category cat ON cat.id = c.category_id\nWHERE c.id = :competitionId! AND c.event_id = :eventId!"};

/**
 * Query generated from SQL:
 * ```
 * SELECT c.id, c.external_id, c.start_date, c.check_in_end::text, c.competition_type,
 *   c.participants_total, c.completed_at, cat.name AS category
 * FROM federated.competition c JOIN federated.category cat ON cat.id = c.category_id
 * WHERE c.id = :competitionId! AND c.event_id = :eventId!
 * ```
 */
export const browseCompetition = new PreparedQuery<IBrowseCompetitionParams,IBrowseCompetitionResult>(browseCompetitionIR);


/** 'BrowseParticipants' parameters type */
export interface IBrowseParticipantsParams {
  competitionId: NumberOrString;
}

/** 'BrowseParticipants' return type */
export interface IBrowseParticipantsResult {
  cancelled: boolean;
  competitor_id: string;
  completion_status: string | null;
  final_gain: string | null;
  name: string | null;
  point_gain: string | null;
  ranking: number;
  ranking_to: number | null;
  start_number: string | null;
}

/** 'BrowseParticipants' query type */
export interface IBrowseParticipantsQuery {
  params: IBrowseParticipantsParams;
  result: IBrowseParticipantsResult;
}

const browseParticipantsIR: any = {"usedParamSet":{"competitionId":true},"params":[{"name":"competitionId","required":true,"transform":{"type":"scalar"},"locs":[{"a":115,"b":129},{"a":215,"b":229}]}],"statement":"WITH participants AS (\n  SELECT competitor_id, cancelled\n  FROM federated.competition_entry WHERE competition_id = :competitionId!\n), results AS (\n  SELECT * FROM federated.competition_result WHERE competition_id = :competitionId!\n)\nSELECT c.id AS competitor_id, c.name,\n  p.cancelled, r.ranking, r.ranking_to, r.start_number, r.point_gain, r.final_gain, r.completion_status\nFROM participants p FULL JOIN results r USING (competitor_id)\nJOIN federated.competitor c ON c.id = coalesce(p.competitor_id, r.competitor_id)\nORDER BY r.ranking NULLS LAST, c.name, c.id"};

/**
 * Query generated from SQL:
 * ```
 * WITH participants AS (
 *   SELECT competitor_id, cancelled
 *   FROM federated.competition_entry WHERE competition_id = :competitionId!
 * ), results AS (
 *   SELECT * FROM federated.competition_result WHERE competition_id = :competitionId!
 * )
 * SELECT c.id AS competitor_id, c.name,
 *   p.cancelled, r.ranking, r.ranking_to, r.start_number, r.point_gain, r.final_gain, r.completion_status
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
  person_id: string;
  person_name: string;
  tenants: stringArray | null;
}

/** 'BrowseParticipantPeople' query type */
export interface IBrowseParticipantPeopleQuery {
  params: IBrowseParticipantPeopleParams;
  result: IBrowseParticipantPeopleResult;
}

const browseParticipantPeopleIR: any = {"usedParamSet":{"competitionId":true},"params":[{"name":"competitionId","required":true,"transform":{"type":"scalar"},"locs":[{"a":102,"b":116},{"a":206,"b":220}]}],"statement":"WITH participants AS (\n  SELECT competitor_id FROM federated.competition_entry WHERE competition_id = :competitionId!\n  UNION\n  SELECT competitor_id FROM federated.competition_result WHERE competition_id = :competitionId!\n), mapped AS (\n  SELECT cc.competitor_id, fp.id AS federated_person_id, fp.canonical_name,\n    p.id AS person_id, p.name AS person_name,\n    ARRAY(SELECT t.name FROM public.tenant_membership tm\n      JOIN public.tenant t ON t.id = tm.tenant_id\n      WHERE tm.person_id = p.id AND tm.status = 'active'\n      ORDER BY t.name, t.id) AS tenants\n  FROM participants cp\n  JOIN federated.competitor_component cc ON cc.competitor_id = cp.competitor_id\n  JOIN federated.person fp ON fp.id = cc.person_id\n  LEFT JOIN public.person p ON fp.external_id <> 0 AND (\n    (fp.federation = 'csts' AND p.csts_id = fp.external_id) OR\n    (fp.federation = 'wdsf' AND p.wdsf_id = fp.external_id)\n  )\n)\nSELECT m.*, (\n  SELECT array_agg(c.id ORDER BY c.id)\n  FROM public.couple c\n  WHERE c.status = 'active' AND m.person_id IN (c.man_id, c.woman_id)\n    AND EXISTS (SELECT 1 FROM mapped partner WHERE partner.competitor_id = m.competitor_id\n      AND partner.person_id = CASE WHEN m.person_id = c.man_id THEN c.woman_id ELSE c.man_id END)\n) AS couple_ids\nFROM mapped m\nORDER BY competitor_id, federated_person_id, person_id"};

/**
 * Query generated from SQL:
 * ```
 * WITH participants AS (
 *   SELECT competitor_id FROM federated.competition_entry WHERE competition_id = :competitionId!
 *   UNION
 *   SELECT competitor_id FROM federated.competition_result WHERE competition_id = :competitionId!
 * ), mapped AS (
 *   SELECT cc.competitor_id, fp.id AS federated_person_id, fp.canonical_name,
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


