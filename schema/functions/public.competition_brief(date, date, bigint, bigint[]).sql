CREATE FUNCTION public.competition_brief(p_since date DEFAULT NULL::date, p_until date DEFAULT NULL::date, p_cohort_id bigint DEFAULT NULL::bigint, p_person_ids bigint[] DEFAULT NULL::bigint[]) RETURNS SETOF public.competition_participation_record
    LANGUAGE sql STABLE
    AS $$
  with params as (
    select
      coalesce(p_since, date_trunc('week', now())::date + 5) as since,
      coalesce(p_until, date_trunc('week', now())::date + 7) as until
  ),
  scoped_people as (
    select distinct p.id, p.name, p.csts_id, p.wdsf_id
    from public.current_tenant_membership tm
    join public.person p on p.id = tm.person_id
    where (p_person_ids is null or p.id = any(p_person_ids))
      and (
        p_cohort_id is null
        or exists (
          select 1
          from public.current_cohort_membership cm
          where cm.person_id = p.id
            and cm.cohort_id = p_cohort_id
        )
      )
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
        ('csts'::text, nullif(regexp_replace(sp.csts_id, '\D', '', 'g'), '')::bigint),
        ('wdsf'::text, nullif(regexp_replace(sp.wdsf_id, '\D', '', 'g'), '')::bigint)
    ) ids(federation, external_id)
    join federated.person fp
      on fp.federation = ids.federation
     and fp.external_id = ids.external_id
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
    dances.dances,
    comp.participants_total as participants,
    null::integer as ranking,
    null::integer as ranking_to,
    null::numeric(10,3) as point_gain,
    null::boolean as is_final,
    false as has_result
  from params
  join federated.competition comp
    on comp.start_date >= params.since
   and comp.start_date < params.until
  join federated.event e on e.id = comp.event_id
  join federated.category cat on cat.id = comp.category_id
  join federated.competition_entry ce
    on ce.competition_id = comp.id
   and not ce.cancelled
  join federated.competitor c on c.id = ce.competitor_id
  join federated.competitor_component cc on cc.competitor_id = c.id
  join federated_people fp on fp.federated_person_id = cc.person_id
  left join lateral (
    select coalesce(array_agg(d.name order by dpd.dance_order), '{}'::text[]) as dances
    from federated.dance_program_dance dpd
    join federated.dance d on d.code = dpd.dance_code
    where dpd.program_id = cat.base_dance_program_id
  ) dances on true
  order by
    comp.start_date,
    comp.check_in_end nulls last,
    fp.person_name,
    cat.discipline,
    cat.class,
    c.name;
$$;

COMMENT ON FUNCTION public.competition_brief(p_since date, p_until date, p_cohort_id bigint, p_person_ids bigint[]) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.competition_brief(p_since date, p_until date, p_cohort_id bigint, p_person_ids bigint[]) TO anonymous;
