CREATE or replace FUNCTION public.former_filtered_people(
  is_trainer boolean,
  is_admin boolean,
  in_cohorts bigint[] default null
) RETURNS SETOF person LANGUAGE sql STABLE AS $$
  with details as (
    select
      person.id as person_id,
      array_remove(array_agg(distinct case when cm.tenant_id = current_tenant_id() then cm.cohort_id end), null) as cohort_memberships,
      array_remove(array_agg(distinct case when tt.tenant_id = current_tenant_id() then tt.tenant_id end), null) as tenant_trainers,
      array_remove(array_agg(distinct case when ta.tenant_id = current_tenant_id() then ta.tenant_id end), null) as tenant_administrators
    from person
    join tenant_membership tm on tm.person_id = person.id and tm.tenant_id = current_tenant_id()
    left join cohort_membership cm on cm.person_id = person.id and cm.tenant_id = current_tenant_id() and cm.status = 'expired'
    left join tenant_trainer tt on tt.person_id = person.id and tt.tenant_id = current_tenant_id() and tt.status = 'expired'
    left join tenant_administrator ta on ta.person_id = person.id and ta.tenant_id = current_tenant_id() and ta.status = 'expired'
    where
      tm.status = 'expired'
      and not exists (
        select 1 from tenant_membership active_tm
        where active_tm.person_id = person.id
          and active_tm.tenant_id = current_tenant_id()
          and active_tm.status = 'active'
      )
    group by person.id
  )
  select p.*
  from person p
  join details d on d.person_id = p.id
  where
    case
      when in_cohorts is null then true
      else coalesce(in_cohorts = d.cohort_memberships, false)
        or coalesce(in_cohorts && d.cohort_memberships, false)
    end
    and case
      when is_trainer is null then true
      else is_trainer = (
        current_tenant_id() = any (coalesce(d.tenant_trainers, array[]::bigint[]))
      )
    end
    and case
      when is_admin is null then true
      else is_admin = (
        current_tenant_id() = any (coalesce(d.tenant_administrators, array[]::bigint[]))
      )
    end
  order by p.last_name, p.first_name
$$;
COMMENT ON FUNCTION public.former_filtered_people IS '@omit';
GRANT ALL ON FUNCTION public.former_filtered_people TO anonymous;
