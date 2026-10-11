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
