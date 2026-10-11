/* @name BrowseSourceFrontiers */
SELECT id, kind, key, fetch_status, process_status
FROM crawler.frontier
WHERE federation = :federation! AND (
  (:federation! = 'csts' AND kind IN ('event', 'eventCompetitors') AND key = :eventKey!) OR
  (:competitionKey::text IS NOT NULL AND key = :competitionKey AND (
    (:federation! = 'csts' AND kind = 'competitionResults') OR
    (:federation! = 'wdsf' AND kind IN ('competition', 'participantIndex', 'officialIndex'))
  ))
)
ORDER BY kind, key;

/* @name BrowseCompetitorHistoryCount */
SELECT count(*)::int AS total FROM (
  SELECT competition_id FROM federated.competition_entry WHERE competitor_id = :id!
  UNION
  SELECT competition_id FROM federated.competition_result WHERE competitor_id = :id!
) history;

/* @name BrowseCompetitorHistory */
WITH entries AS (
  SELECT competition_id, cancelled FROM federated.competition_entry WHERE competitor_id = :id!
), results AS (
  SELECT * FROM federated.competition_result WHERE competitor_id = :id!
)
SELECT c.id, c.event_id, c.start_date, e.name AS event_name, e.external_id AS event_external_id,
  cat.name AS category, ce.cancelled AS "cancelled?", cr.ranking AS "ranking?", cr.ranking_to, cr.point_gain, cr.final_gain
FROM entries ce FULL JOIN results cr USING (competition_id)
JOIN federated.competition c ON c.id = coalesce(ce.competition_id, cr.competition_id)
JOIN federated.event e ON e.id = c.event_id
JOIN federated.category cat ON cat.id = c.category_id
ORDER BY c.start_date DESC, c.id DESC
LIMIT 50 OFFSET :offset!;
