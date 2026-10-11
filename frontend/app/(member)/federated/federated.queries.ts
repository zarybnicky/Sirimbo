/** Types generated for queries found in "app/(member)/federated/federated.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type fetch_status = 'error' | 'gone' | 'ok' | 'pending' | 'transient';

export type process_status = 'error' | 'ok' | 'pending';

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


