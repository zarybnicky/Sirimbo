
CREATE or replace FUNCTION public.person_csts_candidates(in_person public.person, "limit" integer DEFAULT 10, threshold real DEFAULT 0.4) RETURNS TABLE(id integer, name text, age_group text, similarity real)
    LANGUAGE sql STABLE
    AS $_$
  with params as (
    select
      greatest(0.3::real, least(coalesce(threshold, 0.4), 1::real)) as effective_threshold,
      greatest(0, least(coalesce($2, 10), 50)) as effective_limit
  ),
  scored_candidates as (
    select
      fp.external_id::integer as id,
      fp.canonical_name as name,
      fp.age_group,
      score.name_score,
      case
        when in_person.birth_date is not null and fp.dob is not null and fp.dob = in_person.birth_date then 1
        else 0
      end as dob_score
      -- Future year-of-birth range scoring belongs here once federated.person.yob_range exists.
    from params
    join federated.person fp on true
    cross join lateral (
      select public.similarity(fp.search_name, in_person.search_name) as name_score
    ) score
    where in_person.search_name is not null
      and fp.federation = 'csts'
      and fp.external_id between 0 and 2147483647
      and fp.search_name % in_person.search_name
      and score.name_score >= params.effective_threshold
  )
  select scored_candidates.id, scored_candidates.name, scored_candidates.age_group, scored_candidates.name_score
  from scored_candidates
  order by
    scored_candidates.dob_score desc,
    scored_candidates.name_score desc,
    scored_candidates.name,
    scored_candidates.id
  limit (select effective_limit from params);
$_$;

COMMENT ON FUNCTION public.person_csts_candidates(in_person public.person, "limit" integer, threshold real) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.person_csts_candidates(in_person public.person, "limit" integer, threshold real) TO anonymous;

alter table federated.event
  add column if not exists venue_lat double precision,
  add column if not exists venue_lng double precision,
  add column if not exists venue_location_source text,
  add column if not exists venue_location_ref text;

create or replace function federated.class_rank(class text) returns int language sql immutable as $$
  select case class
    when 'M' then 16
    when 'S' then 16
    when 'A' then 15
    when 'B' then 14
    when 'C' then 13
    when 'D' then 12
    when 'E' then 11
    when 'Gold' then 4
    when 'Silver' then 3
    when 'Bronze' then 2
    when 'Novice' then 1
    else 0
  end;
$$;

DROP FUNCTION if exists federated.competition_sankey_links;
create or replace function federated.competition_sankey_links(
    p_first_year integer,
    p_last_year integer,
    p_classes text[],
    p_disciplines text[],
    p_by_school_year boolean default false
) returns table (
    from_year integer,
    from_state text,       -- 'active' | 'inactive'
    from_class text,
    to_year integer,
    to_state text,         -- 'active' | 'inactive'
    to_class text,
    discipline text,
    kind text,             -- 'flow' | 'dropout' | 'inactive' | 'return' | 'entry'
    value integer
) language sql stable as $$
with base as (
    select
        cm.person_id,
        (extract(year from comp.start_date) - case when p_by_school_year and extract(month from comp.start_date) < 9 then 1 else 0 end) as period_year,
        c.discipline as discipline,
        c.class as class,
        federated.class_rank(c.class) as class_rank,
        comp.start_date date,
        comp.category_id,
        cr.competitor_id
    from federated.competition_result cr
    join federated.competition comp on cr.competition_id = comp.id
    join federated.competitor_component cm on cm.competitor_id = cr.competitor_id
    join federated.category c on c.id = comp.category_id
    where (p_disciplines is null or c.discipline = any (p_disciplines))
      and (p_classes is null or c.class = any(p_classes))
),
bounds as (
    select
        coalesce(p_first_year, min(period_year) - 1) as first_year,
        coalesce(p_last_year, max(period_year)) as last_year
    from base
),
observed as (
    select r.person_id, r.period_year, r.discipline, r.class
    from (
        select
            b.*,
            row_number() over (
                partition by b.person_id, b.period_year, b.discipline
                order by b.class_rank desc nulls last, b.date desc nulls last, b.category_id , b.competitor_id
              ) as rn
        from base b, bounds x
        where x.last_year is not null and b.period_year <= x.last_year
    ) r
    where r.rn = 1
),
windowed as (
    select
        o.*,
        row_number() over person_timeline as observed_n,
        lead(o.period_year) over person_timeline as next_year,
        lead(o.class) over person_timeline as next_class
    from observed o
    window person_timeline as (partition by o.person_id, o.discipline order by o.period_year)
),
person_links(from_year, from_state, from_class, to_year, to_state, to_class, discipline, kind) as (
    select
        w.period_year - 1,
        'inactive',
        w.class,
        w.period_year,
        'active',
        w.class,
        w.discipline,
        'entry'
    from windowed w
    where w.observed_n = 1

    union all

    select
        y.from_year,
        case when y.from_year = w.period_year then 'active' else 'inactive' end,
        w.class,
        y.from_year + 1,
        case when w.next_year = y.from_year + 1 then 'active' else 'inactive' end,
        case when w.next_year = y.from_year + 1 then w.next_class else w.class end,
        w.discipline,
        case
            when w.next_year = w.period_year + 1 then 'flow'
            when y.from_year = w.period_year then 'dropout'
            when w.next_year = y.from_year + 1 then 'return'
            else 'inactive'
        end
    from windowed w
    cross join bounds x
    cross join lateral generate_series(w.period_year, coalesce(w.next_year, case when w.period_year < x.last_year then x.last_year else w.period_year end) - 1) as y(from_year)
    where w.next_year is not null or w.period_year < x.last_year
)
select pl.from_year, pl.from_state, pl.from_class, pl.to_year, pl.to_state, pl.to_class, pl.discipline, pl.kind, count(*)::integer as value
from person_links pl, bounds x
where pl.from_year >= x.first_year and pl.to_year <= x.last_year
group by pl.from_year, pl.from_state, pl.from_class, pl.to_year, pl.to_state, pl.to_class, pl.discipline, pl.kind
order by
    pl.from_year,
    pl.discipline,
    coalesce(array_position(array['entry','flow','dropout','inactive','return']::text[], pl.kind), 99),
    pl.from_state,
    pl.from_class,
    pl.to_state,
    pl.to_class;
$$;
