create or replace function app_private.accessible_event_ids() returns setof bigint
language sql stable security definer
set search_path = pg_catalog, public, pg_temp
as $$
  with recursive attended(id) as (
    select instance_id from public.event_instance_registration
    where tenant_id = (select public.current_tenant_id())
      and person_id = any((select public.current_person_ids())::bigint[])
      and registration_status = 'active'
    union
    select child.id from public.event_instance child
    join attended parent on child.parent_id = parent.id
    where child.parent_id is not null
      and child.tenant_id = (select public.current_tenant_id())
  )
  select id from attended
  union
  select id from public.event_instance
  where tenant_id = (select public.current_tenant_id())
    and id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[]);
$$;

grant execute on function app_private.accessible_event_ids to anonymous;
