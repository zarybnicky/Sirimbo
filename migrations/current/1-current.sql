CREATE or replace FUNCTION public.filtered_people(in_tenants bigint[], in_cohort bigint, is_trainer boolean, is_admin boolean) RETURNS SETOF public.person
    LANGUAGE sql STABLE
    AS $$
 select person.* from person
  where
    exists (select 1 from tenant_membership where tenant_id = any (in_tenants) and person_id=person.id and active=true)
  and
    case when in_cohort is null then true
    else exists (select 1 from cohort_membership where cohort_id=in_cohort and person_id=person.id and active=true) end
  and
    case when is_trainer is null then true
    else is_trainer = exists (select 1 from tenant_trainer where tenant_id = any (in_tenants) and person_id=person.id) end
  and
    case when is_admin is null then true
    else is_admin = exists (select 1 from tenant_administrator where tenant_id = any (in_tenants) and person_id=person.id) end
$$;

CREATE or replace FUNCTION public.person_couples(p person) RETURNS SETOF public.couple LANGUAGE sql STABLE AS $$
  select couple.* from couple where (man_id = p.id or woman_id = p.id);
$$;
CREATE or replace FUNCTION public.person_active_couples(p person) RETURNS SETOF public.couple LANGUAGE sql STABLE AS $$
  select couple.* from couple where (man_id = p.id or woman_id = p.id) and active = true;
$$;
GRANT ALL ON FUNCTION public.person_active_couples(person) TO anonymous;
comment on function person_active_couples is E'@simpleCollections only';

CREATE or replace FUNCTION public.couple_attendances(p couple) RETURNS SETOF public.event_attendance LANGUAGE sql STABLE AS $$
  select event_attendance.* from event_attendance
  where person_id = p.man_id or person_id = p.woman_id;
$$;
GRANT ALL ON FUNCTION public.couple_attendances(couple) TO anonymous;
comment on function couple_attendances is E'@simpleCollections only
@filterable
@sortable';
