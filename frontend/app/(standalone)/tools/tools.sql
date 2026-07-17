/* @name CompetitionEventLocations */
select
    e.name as "name",
    e.location as "location",
    e.start_date::text as "startDate!",
    e.venue_lat as "venueLat!",
    e.venue_lng as "venueLng!"
from federated.event e
where e.federation = :federation!
  and e.start_date between :fromDate!::date and :toDate!::date
  and e.venue_lat is not null
  and e.venue_lng is not null
order by e.start_date;

/* @name CompetitionSankeyLinks */
with base as (
    select
        cm.person_id,
        extract(year from comp.start_date)::int as period_year,
        c.discipline as discipline,
        c.class as class,
        federated.class_rank(c.class) as class_rank,
        comp.start_date as date,
        comp.category_id,
        cr.competitor_id
    from federated.competition_result cr
    join federated.competition comp on cr.competition_id = comp.id
    join federated.competitor_component cm on cm.competitor_id = cr.competitor_id
    join federated.category c on c.id = comp.category_id
    where c.discipline = any (:disciplines!::text[])
      and c.class = any (:classes!::text[])
),
observed as (
    select r.person_id, r.period_year, r.discipline, r.class
    from (
        select b.*, row_number() over (
          partition by b.person_id, b.period_year, b.discipline
          order by b.class_rank desc nulls last, b.date desc nulls last, b.category_id, b.competitor_id
        ) as rn
        from base b
        where b.period_year <= :lastYear!::int
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
    cross join lateral generate_series(w.period_year, coalesce(w.next_year, greatest(w.period_year, :lastYear!::int)) - 1) as y(from_year)
    where w.next_year is not null or w.period_year < :lastYear!::int
),
-- with mergeInactive, inactive endpoints lose their class and collapse into one node per year
merged as (
    select
        pl.from_year,
        pl.from_state,
        case when :mergeInactive!::boolean and pl.from_state = 'inactive' then null else pl.from_class end as from_class,
        pl.to_year,
        pl.to_state,
        case when :mergeInactive!::boolean and pl.to_state = 'inactive' then null else pl.to_class end as to_class,
        pl.discipline,
        pl.kind
    from person_links pl
    where pl.from_year >= :firstYear!::int and pl.to_year <= :lastYear!::int
)
select
    from_year as "fromYear!",
    from_state as "fromState!",
    from_class as "fromClass",
    to_year as "toYear!",
    to_state as "toState!",
    to_class as "toClass",
    discipline as "discipline!",
    kind as "kind!",
    count(*)::int as "value!"
from merged
group by from_year, from_state, from_class, to_year, to_state, to_class, discipline, kind
order by
    from_year,
    coalesce(array_position(:classes!::text[], from_class), 9999),
    array_position(array['entry','flow','dropout','inactive','return']::text[], kind),
    discipline,
    to_year,
    coalesce(array_position(:classes!::text[], to_class), 9999);
