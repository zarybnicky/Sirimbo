CREATE FUNCTION public.competition_brief(p_since date DEFAULT ((date_trunc('week'::text, now()))::date + 4), p_until date DEFAULT ((date_trunc('week'::text, now()))::date + 7), p_cohort_id bigint DEFAULT NULL::bigint, p_person_ids bigint[] DEFAULT NULL::bigint[]) RETURNS SETOF public.competition_participation_record
    LANGUAGE sql STABLE
    AS $$
  with scoped_people as materialized (
    select p.id, p.name
    from person p
    where (p.id = any(p_person_ids)
       or (p_person_ids is null and p.id in (
         select person_id from current_tenant_membership
         union all
         select person_id from current_tenant_trainer
         union all
         select person_id from current_tenant_administrator
       )))
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
    join person_federation_link link on link.person_id = sp.id
    join federated.person fp
      on (fp.federation, fp.external_id) = (link.federation, link.external_id)
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
    false as has_result,
    comp.competition_type,
    e.external_id as event_external_id,
    comp.external_id as competition_external_id,
    null::numeric(10,3) as final_gain
  from federated_people fp
  join federated.competitor_component cc on cc.person_id = fp.federated_person_id
  join federated.competitor c on c.id = cc.competitor_id
  join federated.competition_entry ce on ce.competitor_id = c.id and not ce.cancelled
  join federated.competition comp on comp.id = ce.competition_id
  join federated.event e on e.id = comp.event_id
  join federated.category cat on cat.id = comp.category_id
  left join lateral (
    select coalesce(array_agg(d.name order by dpd.dance_order), '{}'::text[]) as dances
    from federated.dance_program_dance dpd
    join federated.dance d on d.code = dpd.dance_code
    where dpd.program_id = cat.base_dance_program_id
  ) dances on true
  where comp.start_date >= p_since
    and comp.start_date < p_until
  order by
    comp.start_date,
    comp.check_in_end nulls last,
    fp.person_name,
    cat.discipline,
    cat.class,
    c.name;
$$;

COMMENT ON FUNCTION public.competition_brief(p_since date, p_until date, p_cohort_id bigint, p_person_ids bigint[]) IS '@behavior -*';

GRANT ALL ON FUNCTION public.competition_brief(p_since date, p_until date, p_cohort_id bigint, p_person_ids bigint[]) TO anonymous;
