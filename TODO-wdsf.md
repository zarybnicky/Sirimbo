# Federation browser, crawler, and WDSF backlog

## Competition results and round marks

- [x] Store and display normalized event/competition statuses: planned, in progress, completed, postponed, cancelled, unknown.
- [x] Link competitions to round detail pages.
- [x] Sort overall results by placing; retain registration browsing.
- [x] Show stored round rankings, qualification, per-dance results, and judges' marks.
- [x] Render marks, skating places, and component scores according to the scoring method.
- [ ] Store official WDSF final dance results and AJS rankings/scores; current round aggregates sum judges' marks.
- [ ] Add competitor/judge relations for the numeric IDs on `JudgeScore`.
- [x] Handle ties, withdrawals/disqualifications, and results without registrations.

## Officials and judging history

- [x] Show local matches for judges and other officials through federation person relations.
- [x] Event page: show officials with person, role, discipline, and grade.
- [x] Competition page: show officials with person and role.
- [x] Round page: show judges with labels/letters and panel order, matched to marks columns.
- [x] Link official names to federation person pages.
- [x] Person page: show official assignments with roles and links to events/competitions.
- [x] Preserve WDSF adjudicator labels in `wdsfOfficialIndex` (see official metadata notes).

## Licenses

- [x] Person page: show license kind, discipline, grade, source status, and expiry date.
- [x] Include athlete, trainer, adjudicator, and other official qualifications.
- [ ] Store WDSF license blocking dates (see member metadata notes).

## Category progress and ranklists

- [x] Competitor page: show category/class, points, domestic finals, and foreign finals.
- [x] Link progress from the person's competitor memberships.
- [ ] Add federation → ranklist → dated snapshot navigation.
- [ ] Show snapshot date/kind and entries ordered by rank, with links to competitors.
- [ ] Link competitors back to their ranklist entries.
- [ ] Show ranklist placement alongside class and age group in the app, linked to the dated snapshot.

## Remaining app consumers

- [ ] [Competitions.tsx](frontend/ui/Competitions.tsx): link dashboard, calendar, and timeline cards to internal event/competition pages; support category labels across federations.
- [ ] [ActivityTimeline.tsx](frontend/ui/ActivityTimeline.tsx): replace ČSTS-only judging links on person/couple/group pages; link event-level official activity too.
- [ ] [competition_reports.sql](migrations/fixtures/functions/competition_reports.sql): use `person_federation_link` in `competition_brief` and `competition_report` instead of repeated ID matching.
- [ ] [activity_timeline.sql](migrations/fixtures/functions/activity_timeline.sql): use the same relation in the judging branch.
- [ ] Review `current_tenant_membership` scoping in reports: explicitly selected, RLS-visible people must not disappear solely for lacking tenant membership.
- [ ] [CstsIdBackfillWidget.tsx](frontend/ui/CstsIdBackfillWidget.tsx) / `person_csts_candidates`: generalize federation selection and candidate matching; retain fuzzy suggestions and explicit selection.

## Clubs and affiliations

- [ ] Add club listing/detail pages.
- [ ] Populate `person_club_membership` and `competitor_club_affiliation` (both empty locally on 2026-10-12).
- [ ] Show dated memberships/affiliations on person and competitor pages, linked to clubs.
- [ ] Show members and competitors on club pages.

## Crawler

- [ ] Add system-admin refetch/reprocess actions using worker scheduling, with resulting status and errors.
- [ ] Rename physical `json_response` / `json_response_cache` tables (low priority).

## Deferred

- [ ] Replace `csts_id`, `wdsf_id`, and ad-hoc external identifiers with persisted federation links.
- [ ] Add provenance and as-of attribution.
- [ ] Add cross-federation matching.
- [ ] Add analytics and roll-ups for actual workloads.
- [ ] Link app categories/dances to federation metadata.

## WDSF ingestion notes

### Couple and team composition

The old scraper fetched `/api/1/couple/rls-<id>` from participant detail links and
stored male/female member IDs. The current `wdsfParticipant` loader creates a
couple competitor but does not attach lead/follow components, so public reports
that join through `federated.competitor_component` cannot associate WDSF couple
results with local `person.wdsf_id` values.

A `wdsfCoupleIndex` is likely a better first target than a per-couple loader if
`/api/1/couple` exposes enough current and historical composition data. The loader
should seed or upsert:

- `federated.competitor` rows for WDSF couples
- `federated.person` rows for the linked people when only IDs/names are present
- `federated.competitor_component` rows with `lead` / `follow` roles
- any country/national-reference metadata that belongs to the couple or entry

The same pattern is missing for teams, formations, groups, duos, and trios.
Add a `wdsfTeamIndex` or equivalent team-family loaders before claiming full WDSF
participant coverage.

### Member metadata

`wdsfMember` currently parses fields that are not stored:

- `nickname`
- `yearOfBirth`
- `nationalReference`
- license blocking dates: `wrlBLockedUntil` and `cupOrChampionshipBlockedUntil`

The schema has `federated.person.dob`, but WDSF supplies only `yearOfBirth`, so
storing a synthetic date would be misleading. Add an honest birth-year or approximate
year-range field if age-based reports remain useful.

### Participant metadata

`wdsfParticipant` parses but drops or only partly uses:

- `basepoints`
- source `coupleId`, `personId`, and `teamId`
- `TeamCountryXmlName`
- participant `country`
- `nationalreference`
- round `maxDeviation`, `recalls`, and `isGroupDance`

Some of this should probably become entry-level metadata. Some should feed the
couple/team loaders. None should be silently discarded.

### Score families

The participant schema accepts several WDSF score families that `scoreRows` currently
turns into no rows:

- `onScaleIdo`
- `onScaleDisco3`
- `onScaleMtc`
- `onScaleTcps`
- `breakingseed`
- `breakingseedbyscore`
- `wdsfbreaking`
- `trivium`

That is acceptable only as a temporary parser-unblocking step. The next iteration
should add score components and materialization rules for any parsed score family
we want to keep accepting.

### Competition metadata

`wdsfCompetition` parses but does not persist or model:

- `coefficient`
- `eventId`
- `lastmodifiedDate`
- `link` relations

### Official metadata

`wdsfOfficialIndex` stores adjudicator labels and propagates them to round judges.
Official country is parsed but not stored.

### Age rules from `/api/1/age`

The branch had a `wdsfAgeGroups` loader and a `federated.federation_age_group_rule`
table. The useful idea is storing WDSF age rules as dated federation metadata:

- `federation`
- `as_of_year`
- `name`
- `from_age`
- `to_age`
- `min_birthdate`
- `max_birthdate`
- `allowed_to_dance_in`
- `divisions`
- parsed applicability flags such as person/couple/team/competition

The branch parsed `isPerson`, `isCouple`, `isTeam`, and `isCompetition` but did not
store them. If this loader is reintroduced, either store those flags or remove them
from the schema.

The helper concept worth keeping is `yob_range_for_competed_age_group`: given a
federation, competed age group, and competition date, infer an approximate year of
birth range. This is useful for candidate matching and data-quality checks, but it
must not replace WDSF member `yearOfBirth` when that explicit field exists.

### Approximate birth years

The branch modeled `person.yob_approx int4range` and narrowed it from multiple
signals. The useful behavior is:

- exact date of birth narrows the range to one year
- explicit WDSF `yearOfBirth` narrows the range to one year
- age-group rules narrow the range only when no more precise signal exists
- conflicting signals should be detectable instead of silently overwriting each other

Current master already has `federated.person.dob`, but WDSF supplies only a year.
The target can be either `birth_year integer` or an approximate year range, but it
must preserve precision honestly. Do not store `YYYY-01-01` as a fake date of birth.

### Participant-driven follow-up frontiers

The branch participant loader queued `couple`, `team`, or `person` frontiers from
participant detail links. Keep that mechanism, but prefer index-driven loaders when
the endpoint supports them:

- `wdsfCoupleIndex` from `/api/1/couple` should probably seed couple composition
  before relying on per-participant couple details.
- `wdsfTeamIndex` or a team-family index should seed team, formation, group, duo,
  and trio composition.
- Participant detail can still queue missing detail frontiers as a repair path.
- Solo participant links can queue `wdsf:member` when person detail is missing.

### Reports from `wdsf/`

```sql
SELECT
  cr.ranking AS rank,
  COUNT(*)::int AS count,
  ROUND(AVG(EXTRACT(YEAR FROM comp.start_date)::int - fp.birth_year), 1)::float AS average_age,
  MIN(EXTRACT(YEAR FROM comp.start_date)::int - fp.birth_year)::int AS min_age,
  MAX(EXTRACT(YEAR FROM comp.start_date)::int - fp.birth_year)::int AS max_age
FROM federated.competition_result cr
JOIN federated.competition comp ON comp.id = cr.competition_id
JOIN federated.competitor c ON c.id = cr.competitor_id
JOIN federated.competitor_component cc ON cc.competitor_id = c.id
JOIN federated.person fp ON fp.id = cc.person_id
WHERE comp.federation = 'wdsf'
  AND c.competitor_type = 'couple'
  AND cr.ranking IS NOT NULL
  AND fp.birth_year IS NOT NULL
GROUP BY cr.ranking
ORDER BY cr.ranking;
```

```sql
WITH cleaned AS (
  SELECT
    COALESCE(NULLIF(BTRIM(ce.country), ''), NULLIF(BTRIM(c.country), ''), 'Unknown') AS country,
    c.id AS competitor_id
  FROM federated.competition_result cr
  JOIN federated.competition comp ON comp.id = cr.competition_id
  JOIN federated.competition_entry ce
    ON ce.competition_id = cr.competition_id
   AND ce.competitor_id = cr.competitor_id
  JOIN federated.competitor c ON c.id = cr.competitor_id
  WHERE comp.federation = 'wdsf'
    AND c.competitor_type = 'couple'
    AND cr.ranking = 1
)
SELECT
  country,
  COUNT(*)::int AS wins,
  COUNT(DISTINCT competitor_id)::int AS couples
FROM cleaned
GROUP BY country
ORDER BY wins DESC, country
LIMIT 10;
```
