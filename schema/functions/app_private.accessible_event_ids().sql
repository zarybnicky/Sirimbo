CREATE FUNCTION app_private.accessible_event_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  with recursive attended(id) as (
    select event_id from public.event_registration
    where tenant_id = (select public.current_tenant_id())
      and person_id = any((select public.current_person_ids())::bigint[])
      and registration_status = 'active'
    union
    select child.id from public.event child
    join attended parent on child.parent_id = parent.id
    where child.parent_id is not null
      and child.tenant_id = (select public.current_tenant_id())
  )
  select id from attended
  union
  select id from public.event
  where tenant_id = (select public.current_tenant_id())
    and id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[]);
$$;

GRANT ALL ON FUNCTION app_private.accessible_event_ids() TO anonymous;
