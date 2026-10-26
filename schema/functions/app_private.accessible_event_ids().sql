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
  -- Scan events once; their primary key already makes the result unique.
  select id from public.event
  where tenant_id = (select public.current_tenant_id())
    and (
      is_public
      or (is_visible and (select pg_has_role(current_setting('role'), 'member', 'member')))
      or (manager_person_ids && (select current_person_ids()))
      or id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[])
      or id in (select id from attended)
    );
$$;

GRANT ALL ON FUNCTION app_private.accessible_event_ids() TO anonymous;
