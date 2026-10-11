/** Types generated for queries found in "app/(member)/federated/federated.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type fetch_status = 'error' | 'gone' | 'ok' | 'pending' | 'transient';

export type process_status = 'error' | 'ok' | 'pending';

export type NumberOrString = number | string;

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


