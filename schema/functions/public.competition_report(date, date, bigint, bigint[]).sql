CREATE FUNCTION public.competition_report(p_since date DEFAULT ((date_trunc('week'::text, now()))::date - 2), p_until date DEFAULT (date_trunc('week'::text, now()))::date, p_cohort_id bigint DEFAULT NULL::bigint, p_person_ids bigint[] DEFAULT NULL::bigint[]) RETURNS SETOF public.competition_participation_record
    LANGUAGE sql STABLE
    AS $$
  with scoped_competitions as (
    select *
    from federated.competition
    where start_date >= p_since
      and start_date < p_until
  ),
  latest_rounds as (
    select distinct on (r.competition_id)
      r.id,
      r.competition_id
    from federated.competition_round r
    join scoped_competitions comp on comp.id = r.competition_id
    order by r.competition_id, r.round_index desc, (r.round_key = 'F') desc, r.id desc
  ),
  competition_dances as (
    select
      r.competition_id,
      array_agg(d.name order by rd.dance_order) as dances
    from latest_rounds r
    join federated.round_dance rd on rd.round_id = r.id
    join federated.dance d on d.code = rd.dance_code
    group by r.competition_id
  ),
  scoped_people as (
    select distinct p.id, p.name, p.csts_id, p.wdsf_id
    from current_tenant_membership tm
    join person p on p.id = tm.person_id
    where (p_person_ids is null or p.id = any(p_person_ids))
      and (p_cohort_id is null
       or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
  ),
  federated_people as (
    select
      sp.id as person_id,
      sp.name as person_name,
      fp.id as federated_person_id,
      fp.federation
    from scoped_people sp
    cross join lateral (
      values
        ('csts'::text, sp.csts_id::bigint),
        ('wdsf'::text, sp.wdsf_id::bigint)
    ) ids(federation, external_id)
    join federated.person fp
      on fp.federation = ids.federation
     and fp.external_id = ids.external_id
     and ids.external_id <> 0
  )
  select
    fp.person_id,
    fp.person_name,
    fp.federation,
    fp.federated_person_id,
    c.id as competitor_id,
    c.name as competitor_name,
    c.competitor_type,
    e.id as event_id,
    e.name as event_name,
    coalesce(e.location, e.city) as event_location,
    comp.id as competition_id,
    comp.start_date as competition_date,
    comp.check_in_end,
    cat as category,
    coalesce(dances.dances, '{}'::text[]) as dances,
    comp.participants_total as participants,
    cr.ranking,
    cr.ranking_to,
    cr.point_gain,
    cr.is_final,
    true as has_result,
    comp.competition_type,
    e.external_id as event_external_id,
    comp.external_id as competition_external_id
  from scoped_competitions comp
  join federated.event e on e.id = comp.event_id
  join federated.category cat on cat.id = comp.category_id
  join federated.competition_result cr on cr.competition_id = comp.id
  join federated.competitor c on c.id = cr.competitor_id
  join federated.competitor_component cc on cc.competitor_id = c.id
  join federated_people fp on fp.federated_person_id = cc.person_id
  left join competition_dances dances on dances.competition_id = comp.id
  order by
    comp.start_date,
    fp.person_name,
    cat.discipline,
    cat.class,
    cr.ranking,
    c.name;
$$;

COMMENT ON FUNCTION public.competition_report(p_since date, p_until date, p_cohort_id bigint, p_person_ids bigint[]) IS '@omit';

GRANT ALL ON FUNCTION public.competition_report(p_since date, p_until date, p_cohort_id bigint, p_person_ids bigint[]) TO anonymous;
