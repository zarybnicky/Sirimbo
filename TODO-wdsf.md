# Federation browser, crawler, and WDSF backlog

## Browser and shared UI

- [ ] Redesign reusable faceted search for competitors, then app people/couples; retain inline class, points/finals, and ranklist placement where relevant.
- [ ] Derive competitor age categories from their members.
- [ ] Make `/federated` suitable for ordinary users before linking it throughout the main app.
- [x] Retain local person/couple links in competition results. Results and registrations share the query fragment and link rendering.
- [x] Derive ČSTS event status from competition completion, while preserving explicit cancellation.

ČSTS and WDSF share event status aggregation. ČSTS index refreshes preserve completed
events, and event detail loads preserve explicit cancellation. Existing events update
on their next load.

## Competition results and round marks

- [ ] Store official WDSF final dance results and AJS rankings/scores; current round aggregates sum judges' marks.
- [ ] Add competitor/judge relations for the numeric IDs on `JudgeScore`.

## Ranklists

- [ ] Check the points formula in [cstsRanklist.ts](worker/crawler/cstsRanklist.ts): it adds `pointsWdsf` twice and omits `pointsChampionship`. Confirm the intended calculation, fix it, and replay cached responses locally.
- [ ] Add federation → ranklist → dated snapshot navigation.
- [ ] Show snapshot date/kind and entries ordered by rank, with links to competitors.
- [ ] Link competitors back to their ranklist entries.
- [ ] Link in-app ranklist placements to the dated snapshot; retain the compact current-category summary without nomination rankings.
- [ ] Add WDSF ranklist ingestion; stored snapshots currently cover ČSTS only.

## Remaining app consumers

- [ ] [Competitions.tsx](frontend/ui/Competitions.tsx): link dashboard, calendar, and timeline cards to internal event/competition pages once the browser is ready for users; support category labels across federations.
- [ ] [ActivityTimeline.tsx](frontend/ui/ActivityTimeline.tsx): replace ČSTS-only judging links on person/couple/group pages; link event-level official activity too.
- [ ] [CstsIdBackfillWidget.tsx](frontend/ui/CstsIdBackfillWidget.tsx) / `person_csts_candidates`: generalize federation selection and candidate matching; retain fuzzy suggestions and explicit selection.

## Clubs and affiliations

- [ ] Add club listing/detail pages.
- [ ] Populate `person_club_membership` and `competitor_club_affiliation` (both empty locally on 2026-10-12).
- [ ] Show dated memberships/affiliations on person and competitor pages, linked to clubs.
- [ ] Show members and competitors on club pages.

## Crawler

- [ ] Link failure groups to the affected filtered frontier list.
- [ ] Add system-admin refetch/reprocess actions using worker scheduling, with resulting status and errors.
- [ ] Rename physical `json_response` / `json_response_cache` tables (low priority).
- [ ] Resume the WDSF completeness/recovery/replay investigation in a separate session; explicitly shelved, with no production access.

## Deferred

- [ ] Audit missing `@ref` / `@refVia` relations and existing refs that consumers bypass with nested traversals. Prefer direct relations where intermediate rows carry no information needed by the consumer.
- [ ] Replace `csts_id`, `wdsf_id`, and ad-hoc external identifiers with persisted federation links.
- [ ] Remove surrogate IDs for federation events/competitions; external IDs in browser URLs did not replace the database keys.
- [ ] Add provenance and as-of attribution.
- [ ] Add cross-federation matching.
- [ ] Link app categories/dances to federation metadata.

## WDSF ingestion notes

### Couple and team composition

- [ ] Implement couple and team composition loaders so person-based reports can associate WDSF results with their members.

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

- [ ] Store explicit birth years, license blocking dates, and the remaining member metadata below.

`wdsfMember` currently parses fields that are not stored:

- `nickname`
- `yearOfBirth`
- `nationalReference`
- license blocking dates: `wrlBLockedUntil` and `cupOrChampionshipBlockedUntil`

The schema has `federated.person.dob`, but WDSF supplies only `yearOfBirth`, so
storing a synthetic date would be misleading. Add an honest birth-year or approximate
year-range field if age-based reports remain useful.

### Participant metadata

- [ ] Store or use the participant and round metadata below for entries, composition, and discovery.

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

- [ ] Materialize the accepted score families below that currently produce no score rows.

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

- [ ] Store or use the competition metadata below.

`wdsfCompetition` parses but does not persist or model:

- `coefficient`
- `eventId`
- `lastmodifiedDate`
- `link` relations

### Official metadata

- [ ] Store official country.

`wdsfOfficialIndex` stores adjudicator labels and propagates them to round judges.
Official country is parsed but not stored.

### Age rules from `/api/1/age`

- [ ] Restore dated federation age rules and their applicability flags.

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

- [ ] Implement inferred birth-year ranges, precedence of explicit birth data, and conflict detection.

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

- [ ] Restore follow-up discovery for missing couple, team, and person details.

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

- [ ] Implement the age-by-placement report below once composition and birth-year data is available.
- [ ] Implement the wins-by-country report below once composition and country data is available.

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
