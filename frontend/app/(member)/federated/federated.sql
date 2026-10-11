/* @name BrowseParticipants */
WITH participants AS (
  SELECT competitor_id, cancelled
  FROM federated.competition_entry WHERE competition_id = :competitionId!
), results AS (
  SELECT * FROM federated.competition_result WHERE competition_id = :competitionId!
)
SELECT c.id AS competitor_id, c.external_id AS competitor_external_id, c.name,
  p.cancelled AS "cancelled?", r.ranking AS "ranking?", r.ranking_to, r.start_number, r.point_gain, r.final_gain, r.completion_status
FROM participants p FULL JOIN results r USING (competitor_id)
JOIN federated.competitor c ON c.id = coalesce(p.competitor_id, r.competitor_id)
ORDER BY r.ranking NULLS LAST, c.name, c.id;

/* @name BrowseParticipantPeople */
WITH participants AS (
  SELECT competitor_id FROM federated.competition_entry WHERE competition_id = :competitionId!
  UNION
  SELECT competitor_id FROM federated.competition_result WHERE competition_id = :competitionId!
), mapped AS (
  SELECT cc.competitor_id, fp.id AS federated_person_id, fp.external_id AS person_external_id, fp.canonical_name,
    p.id AS person_id, p.name AS person_name,
    ARRAY(SELECT t.name FROM public.tenant_membership tm
      JOIN public.tenant t ON t.id = tm.tenant_id
      WHERE tm.person_id = p.id AND tm.status = 'active'
      ORDER BY t.name, t.id) AS tenants
  FROM participants cp
  JOIN federated.competitor_component cc ON cc.competitor_id = cp.competitor_id
  JOIN federated.person fp ON fp.id = cc.person_id
  LEFT JOIN public.person p ON fp.external_id <> 0 AND (
    (fp.federation = 'csts' AND p.csts_id = fp.external_id) OR
    (fp.federation = 'wdsf' AND p.wdsf_id = fp.external_id)
  )
)
SELECT m.*, (
  SELECT array_agg(c.id ORDER BY c.id)
  FROM public.couple c
  WHERE c.status = 'active' AND m.person_id IN (c.man_id, c.woman_id)
    AND EXISTS (SELECT 1 FROM mapped partner WHERE partner.competitor_id = m.competitor_id
      AND partner.person_id = CASE WHEN m.person_id = c.man_id THEN c.woman_id ELSE c.man_id END)
) AS couple_ids
FROM mapped m
ORDER BY competitor_id, federated_person_id, person_id;

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
