# WDSF removal scratch notes

## Couple and team composition

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

## Member metadata

`wdsfMember` currently parses fields that are not stored:

- `nickname`
- `yearOfBirth`
- `nationalReference`
- license blocking dates: `wrlBLockedUntil` and `cupOrChampionshipBlockedUntil`

The schema has `federated.person.dob`, but WDSF supplies only `yearOfBirth`, so
storing a synthetic date would be misleading. Add an honest birth-year or approximate
year-range field if age-based reports remain useful.

## Participant metadata

`wdsfParticipant` parses but drops or only partly uses:

- `basepoints`
- source `coupleId`, `personId`, and `teamId`
- `TeamCountryXmlName`
- participant `country`
- `nationalreference`
- round `maxDeviation`, `recalls`, and `isGroupDance`

Some of this should probably become entry-level metadata. Some should feed the
couple/team loaders. None should be silently discarded.

## Score families

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

## Competition metadata

`wdsfCompetition` parses but does not persist or model:

- WDSF `status`
- `coefficient`
- `eventId`
- `lastmodifiedDate`
- `link` relations

## Official metadata

`wdsfOfficialIndex` parses official country and adjudicator label, but the current
load path stores neither. The adjudicator label is important if WDSF score payloads
or reports need judge letters rather than just official external IDs.

## Age rules from `/api/1/age`

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

## Approximate birth years

The branch modeled `person.yob_approx int4range` and narrowed it from multiple
signals. The useful behavior is:

- exact date of birth narrows the range to one year
- explicit WDSF `yearOfBirth` narrows the range to one year
- age-group rules narrow the range only when no more precise signal exists
- conflicting signals should be detectable instead of silently overwriting each other

Current master already has `federated.person.dob`, but WDSF supplies only a year.
The target can be either `birth_year integer` or an approximate year range, but it
must preserve precision honestly. Do not store `YYYY-01-01` as a fake date of birth.

## Participant-driven follow-up frontiers

The branch participant loader queued `couple`, `team`, or `person` frontiers from
participant detail links. Keep that mechanism, but prefer index-driven loaders when
the endpoint supports them:

- `wdsfCoupleIndex` from `/api/1/couple` should probably seed couple composition
  before relying on per-participant couple details.
- `wdsfTeamIndex` or a team-family index should seed team, formation, group, duo,
  and trio composition.
- Participant detail can still queue missing detail frontiers as a repair path.
- Solo participant links can queue `wdsf:member` when person detail is missing.

## Reports from `wdsf/`

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
