create or replace function public.csts_athlete(idt int) returns text as $$
  select canonical_name from federated.person where federation = 'csts' and external_id = idt::text;
$$ language sql stable;

create or replace function public.wdsf_athlete(min int) returns text as $$
  select canonical_name from federated.person where federation = 'wdsf' and external_id = min::text;
$$ language sql stable;

grant all on function public.csts_athlete to anonymous;
grant all on function public.wdsf_athlete to anonymous;

drop function if exists public.person_csts_progress;
create or replace function public.person_csts_progress(in_person public.person) returns table (
  competitor_name text,
  category federated.category,
  points numeric(10, 3),
  finals integer
) as $$
select
  competitor.name as competitor_name,
  row(category.*) as category,
  ccp.points,
  ccp.domestic_finals + ccp.foreign_finals as finals
from federated.person p
       join federated.competitor_component cp on cp.person_id = p.id
       join federated.competitor on competitor.id = cp.competitor_id
       join federated.competitor_category_progress ccp on competitor.id = ccp.competitor_id
       join federated.category on ccp.category_id = category.id
where p.federation = 'csts' and p.external_id = in_person.csts_id;
$$ language sql stable;

comment on function public.person_csts_progress is '@simpleCollections only';
grant all on function public.person_csts_progress to anonymous;

drop function if exists competition_brief;
CREATE OR REPLACE FUNCTION public.competition_brief(
  p_person_ids text[] DEFAULT null,
  p_since date DEFAULT null,
  p_until date DEFAULT null
)
RETURNS TABLE (
  person_id         text,
  person_name       text,
  competitor_id     text,
  competitor_name   text,
  event_id          bigint,
  event_name        text,
  event_location    text,
  competition_id    bigint,
  competition_date  date,
  check_in_end      time,
  category_name     text,
  discipline        text,
  age_group         text,
  class             text,
  dances            text[]
)
LANGUAGE sql STABLE AS $$
  WITH params AS (
    SELECT
      coalesce(p_since, date_trunc('week', now())::date + 5) AS since,
      coalesce(p_until, date_trunc('week', now())::date + 6) AS until,
      coalesce(p_person_ids, (
        select array_agg('csts:' || regexp_replace(person.csts_id, '\D', '', 'g'))
        from public.current_tenant_membership join public.person on person_id = person.id
        where csts_id is not null
      )) as person_ids
  )
  SELECT
    p.id                                   AS person_id,
    coalesce(p.canonical_name, concat_ws(' ', p.first_name, p.last_name)) AS person_name,
    c.id                                   AS competitor_id,
    c.name                                 AS competitor_name,
    e.id                                   AS event_id,
    e.name                                 AS event_name,
    coalesce(e.location, e.city)           AS event_location,
    comp.id                                AS competition_id,
    comp.start_date                        AS competition_date,
    comp.check_in_end,
    cat.name                               AS category_name,
    cat.discipline,
    cat.age_group,
    cat.class,
    array_agg(DISTINCT d.name ORDER BY d.name) AS dances
  FROM params
  CROSS JOIN federated.competitor_component cc
  JOIN federated.person p ON p.id = cc.person_id
  JOIN federated.competitor c ON c.id = cc.competitor_id
  JOIN federated.competition_entry ce
    ON ce.competitor_id = c.id
   AND NOT ce.cancelled
  JOIN federated.competition comp ON comp.id = ce.competition_id
  JOIN federated.event e ON e.id = comp.event_id
  JOIN federated.category cat ON cat.id = comp.category_id
  LEFT JOIN federated.dance_program_dance dpd
    ON dpd.program_id = cat.base_dance_program_id
  LEFT JOIN federated.dance d ON d.code = dpd.dance_code
  WHERE cc.person_id = ANY(params.person_ids)
    AND comp.start_date BETWEEN params.since AND params.until
  GROUP BY
    p.id, p.canonical_name, p.first_name, p.last_name,
    c.id, c.name,
    e.id, e.name, e.location, e.city,
    comp.id, comp.start_date, comp.check_in_end,
    cat.name, cat.discipline, cat.age_group, cat.class
  ORDER BY
    comp.start_date,
    comp.check_in_end NULLS LAST,
    person_name,
    cat.discipline,
    cat.class;
$$;
comment on function public.competition_brief is '@simpleCollections only';
grant all on function public.competition_brief to anonymous;

drop function if exists competition_report;
CREATE OR REPLACE FUNCTION public.competition_report(
  p_person_ids text[] DEFAULT null,
  p_since date DEFAULT null,
  p_until date DEFAULT null
)
RETURNS TABLE (
  person_id         text,
  person_name       text,
  competitor_id     text,
  competitor_name   text,
  event_name        text,
  event_location    text,
  event_id          bigint,
  competition_id    bigint,
  competition_date  date,
  category_name     text,
  series            text,
  discipline        text,
  age_group         text,
  class             text,
  participants      integer,
  ranking           integer,
  ranking_to        integer,
  point_gain        numeric(10,3),
  is_final          boolean
)
LANGUAGE sql STABLE AS $$
  WITH params AS (
    SELECT
      coalesce(p_since, date_trunc('week', now())::date - 7 + 5) AS since,
      coalesce(p_until, date_trunc('week', now())::date - 7 + 6) AS until,
      coalesce(p_person_ids, (
        select array_agg('csts:' || regexp_replace(person.csts_id, '\D', '', 'g'))
        from public.current_tenant_membership join public.person on person_id = person.id
        where csts_id is not null
      )) as person_ids
  )
  SELECT
    p.id                                   AS person_id,
    coalesce(p.canonical_name, concat_ws(' ', p.first_name, p.last_name)) AS person_name,
    c.id                                   AS competitor_id,
    c.name                                 AS competitor_name,
    e.name                                 AS event_name,
    coalesce(e.location, e.city)           AS event_location,
    e.id                                   AS event_id,
    comp.id                                AS competition_id,
    comp.start_date                        AS competition_date,
    cat.name                               AS category_name,
    cat.series,
    cat.discipline,
    cat.age_group,
    cat.class,
    comp.participants_total                AS participants,
    cr.ranking,
    cr.ranking_to,
    cr.point_gain,
    cr.is_final
  FROM params
  CROSS JOIN federated.competitor_component cc
  JOIN federated.person p ON p.id = cc.person_id
  JOIN federated.competitor c ON c.id = cc.competitor_id
  JOIN federated.competition_result cr ON cr.competitor_id = c.id
  JOIN federated.competition comp ON comp.id = cr.competition_id
  JOIN federated.event e ON e.id = comp.event_id
  JOIN federated.category cat ON cat.id = comp.category_id
  WHERE cc.person_id = ANY(params.person_ids)
    AND comp.start_date BETWEEN params.since AND params.until
  ORDER BY
    comp.start_date,
    person_name,
    cat.discipline,
    cat.class,
    cr.ranking;
$$;
comment on function public.competition_report is '@simpleCollections only';
grant all on function public.competition_report to anonymous;
