CREATE FUNCTION federated.competition_sankey_links(p_first_year integer, p_last_year integer, p_classes text[], p_disciplines text[], p_by_school_year boolean DEFAULT false) RETURNS TABLE(from_year integer, from_state text, from_class text, to_year integer, to_state text, to_class text, discipline text, kind text, value integer)
    LANGUAGE sql STABLE
    AS $$
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
