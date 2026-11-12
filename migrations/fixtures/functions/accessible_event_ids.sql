create or replace function app_private.accessible_event_ids(title_query tsquery default null) returns setof bigint
language sql stable security definer
set search_path = pg_catalog, public, pg_temp
as $$
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
    and (title_query is null or search_vector @@ title_query)
    and (
      (select pg_has_role(current_setting('role'), 'administrator', 'member'))
      or is_public
      or (is_visible and (select pg_has_role(current_setting('role'), 'member', 'member')))
      or ((select pg_has_role(current_setting('role'), 'trainer', 'member')) and cardinality(manager_person_ids) = 0)
      or (manager_person_ids && (select current_person_ids()))
      or id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[])
      or id in (select id from attended)
    );
$$;

grant execute on function app_private.accessible_event_ids to anonymous;
