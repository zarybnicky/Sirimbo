CREATE FUNCTION public.tenant_couples(t public.tenant) RETURNS SETOF public.couple
    LANGUAGE sql STABLE
    AS $$
  select distinct couple.*
  from couple
  join tenant_membership on man_id=person_id or woman_id=person_id
  where now() <@ couple.active_range and now() <@ tenant_membership.active_range and tenant_id=t.id
  order by couple.active_range asc;
$$;

COMMENT ON FUNCTION public.tenant_couples(t public.tenant) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.tenant_couples(t public.tenant) TO anonymous;


