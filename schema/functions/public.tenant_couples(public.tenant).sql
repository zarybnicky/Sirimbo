CREATE FUNCTION public.tenant_couples(t public.tenant) RETURNS SETOF public.couple
    LANGUAGE sql STABLE
    AS $$
  select couple.*
  from couple
  where couple.status = 'active'
    and (exists (select 1 from current_tenant_membership where person_id = man_id)
      or exists (select 1 from current_tenant_membership where person_id = woman_id));
$$;

COMMENT ON FUNCTION public.tenant_couples(t public.tenant) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.tenant_couples(t public.tenant) TO anonymous;
