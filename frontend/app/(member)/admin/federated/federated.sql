/* @name BrowseFederations */
SELECT f.code, f.name,
  (SELECT count(*)::int FROM federated.event e WHERE e.federation = f.code) AS events,
  (SELECT count(*)::int FROM federated.competition c WHERE c.federation = f.code) AS competitions
FROM federated.federation f ORDER BY f.code;

/* @name BrowseFederation */
SELECT code, name FROM federated.federation WHERE code = :federation!;

/* @name BrowseEventCount */
SELECT count(*)::int AS total
FROM federated.event e
WHERE federation = :federation!
  AND (:search! = '' OR strpos(lower(concat_ws(' ', name, location, city, external_id)), lower(:search!)) > 0)
  AND (:date! = '' OR range @> nullif(:date!, '')::date);

/* @name BrowseEvents */
SELECT e.id, e.external_id, e.name, e.start_date, e.end_date, e.location, e.city, e.country,
  (SELECT count(*)::int FROM federated.competition c WHERE c.event_id = e.id) AS competitions
FROM federated.event e
WHERE federation = :federation!
  AND (:search! = '' OR strpos(lower(concat_ws(' ', name, location, city, external_id)), lower(:search!)) > 0)
  AND (:date! = '' OR range @> nullif(:date!, '')::date)
ORDER BY start_date DESC, id DESC
LIMIT 50 OFFSET :offset!;

/* @name BrowseEvent */
SELECT e.id, e.federation, e.external_id, e.name, e.start_date, e.end_date,
  e.location, e.street_address, e.city, e.postal_code, e.country, f.name AS federation_name
FROM federated.event e JOIN federated.federation f ON f.code = e.federation
WHERE e.id = :eventId! AND e.federation = :federation!;

/* @name BrowseCompetitions */
SELECT c.id, c.external_id, c.start_date, c.check_in_end::text, c.competition_type, cat.name AS category,
  (SELECT count(*)::int FROM federated.competition_entry ce WHERE ce.competition_id = c.id AND NOT ce.cancelled) AS registrations,
  (SELECT count(*)::int FROM federated.competition_result cr WHERE cr.competition_id = c.id) AS results
FROM federated.competition c JOIN federated.category cat ON cat.id = c.category_id
WHERE c.event_id = :eventId!
ORDER BY c.start_date, c.check_in_end NULLS LAST, cat.name, c.id;

/* @name BrowseCompetition */
SELECT c.id, c.external_id, c.start_date, c.check_in_end::text, c.competition_type,
  c.participants_total, c.completed_at, cat.name AS category
FROM federated.competition c JOIN federated.category cat ON cat.id = c.category_id
WHERE c.id = :competitionId! AND c.event_id = :eventId!;

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

/* @name BrowseRounds */
SELECT r.id, r.round_key, r.round_label, r.scoring_method,
  (SELECT count(*)::int FROM federated.competition_round_result rr WHERE rr.round_id = r.id) AS results
FROM federated.competition_round r WHERE competition_id = :competitionId!
ORDER BY round_index NULLS LAST, id;

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

/* @name BrowseCompetitor */
SELECT c.id, c.name, c.competitor_type, c.age_group, f.name AS federation_name
FROM federated.competitor c JOIN federated.federation f ON f.code = c.federation
WHERE c.id = :id! AND c.federation = :federation!;

/* @name BrowseCompetitorMembers */
SELECT p.id, p.external_id, coalesce(p.canonical_name, nullif(concat_ws(' ', p.first_name, p.last_name), ''), p.id) AS name, cc.role
FROM federated.competitor_component cc JOIN federated.person p ON p.id = cc.person_id
WHERE cc.competitor_id = :id!
ORDER BY cc.role, name, p.id;

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

/* @name BrowseFederatedPerson */
SELECT p.id, p.external_id, coalesce(p.canonical_name, nullif(concat_ws(' ', p.first_name, p.last_name), ''), p.id) AS name,
  p.gender, p.dob, p.nationality, p.age_group, f.name AS federation_name
FROM federated.person p JOIN federated.federation f ON f.code = p.federation
WHERE p.id = :id! AND p.federation = :federation!;

/* @name BrowsePersonCompetitors */
SELECT c.id, c.external_id, c.name, c.competitor_type, cc.role
FROM federated.competitor_component cc JOIN federated.competitor c ON c.id = cc.competitor_id
WHERE cc.person_id = :id!
ORDER BY c.name, c.id;

/* @name BrowsePersonLocalMatches */
SELECT p.id, p.name, ARRAY(
  SELECT t.name FROM public.tenant_membership tm JOIN public.tenant t ON t.id = tm.tenant_id
  WHERE tm.person_id = p.id AND tm.status = 'active' ORDER BY t.name, t.id
) AS tenants
FROM public.person p JOIN federated.person fp ON fp.external_id <> 0 AND (
  (fp.federation = 'csts' AND p.csts_id = fp.external_id) OR
  (fp.federation = 'wdsf' AND p.wdsf_id = fp.external_id)
)
WHERE fp.id = :id!
ORDER BY p.name, p.id;

/* @name BrowsePersonFrontiers */
SELECT id, kind, key, fetch_status, process_status
FROM crawler.frontier
WHERE federation = :federation! AND kind = 'member' AND key = :key!
ORDER BY id;
