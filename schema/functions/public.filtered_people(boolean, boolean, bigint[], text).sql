CREATE FUNCTION public.filtered_people(is_trainer boolean, is_admin boolean, in_cohorts bigint[] DEFAULT NULL::bigint[], membership_state text DEFAULT 'current'::text) RETURNS SETOF public.person
    LANGUAGE plpgsql STABLE
    AS $$
begin
  if lower(coalesce(membership_state, 'current')) = 'former' then
    return query
      select *
      from public.former_filtered_people(is_trainer, is_admin, in_cohorts);
  end if;

  return query
    select person.*
    from person
    join auth_details on auth_details.person_id = person.id
    where
      current_tenant_id() = any (auth_details.allowed_tenants)
      and case
        when in_cohorts is null then true
        else in_cohorts = auth_details.cohort_memberships
          or in_cohorts && auth_details.cohort_memberships
      end
      and case
        when is_trainer is null then true
        else is_trainer = (current_tenant_id() = any (auth_details.tenant_trainers))
      end
      and case
        when is_admin is null then true
        else is_admin = (current_tenant_id() = any (auth_details.tenant_administrators))
      end
    order by last_name, first_name;
end;
$$;

COMMENT ON FUNCTION public.filtered_people(is_trainer boolean, is_admin boolean, in_cohorts bigint[], membership_state text) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.filtered_people(is_trainer boolean, is_admin boolean, in_cohorts bigint[], membership_state text) TO anonymous;
