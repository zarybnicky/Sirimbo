CREATE FUNCTION public.competition_report(p_since date DEFAULT ((date_trunc('week'::text, now()))::date - 2), p_until date DEFAULT (date_trunc('week'::text, now()))::date, p_cohort_id bigint DEFAULT NULL::bigint, p_person_ids bigint[] DEFAULT NULL::bigint[]) RETURNS SETOF public.competition_participation_record
    LANGUAGE sql STABLE
    AS $$
  with scoped_people as materialized (
    select p.id, p.name
    from person p
    where (p.id = any(p_person_ids)
       or (p_person_ids is null and p.id in (
         select person_id from current_tenant_membership
         union all select person_id from current_tenant_trainer
         union all select person_id from current_tenant_administrator
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
    coalesce(dances.dances, '{}'::text[]) as dances,
    comp.participants_total as participants,
    cr.ranking,
    cr.ranking_to,
    cr.point_gain,
    cr.is_final,
    true as has_result,
    comp.competition_type,
    e.external_id as event_external_id,
    comp.external_id as competition_external_id,
    cr.final_gain
  from federated_people fp
  join federated.competitor_component cc on cc.person_id = fp.federated_person_id
  join federated.competitor c on c.id = cc.competitor_id
  join federated.competition_result cr on cr.competitor_id = c.id
  join federated.competition comp on comp.id = cr.competition_id
  join federated.event e on e.id = comp.event_id
  join federated.category cat on cat.id = comp.category_id
  left join lateral (
    select array_agg(d.name order by rd.dance_order) as dances
    from (
      select r.id
      from federated.competition_round r
      where r.competition_id = comp.id
      order by r.round_index desc, (r.round_key = 'F') desc, r.id desc
      limit 1
    ) r
    join federated.round_dance rd on rd.round_id = r.id
    join federated.dance d on d.code = rd.dance_code
  ) dances on true
  where comp.start_date >= p_since
    and comp.start_date < p_until
  order by
    comp.start_date,
    fp.person_name,
    cat.discipline,
    cat.class,
    cr.ranking,
    c.name;
$$;

COMMENT ON FUNCTION public.competition_report(p_since date, p_until date, p_cohort_id bigint, p_person_ids bigint[]) IS '@behavior -*';

GRANT ALL ON FUNCTION public.competition_report(p_since date, p_until date, p_cohort_id bigint, p_person_ids bigint[]) TO anonymous;
