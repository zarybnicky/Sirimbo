create or replace function app_private.visible_person_ids(include_partners boolean default false)
returns setof bigint
language sql stable leakproof parallel safe security definer
set search_path = pg_catalog, public, pg_temp
as $$
  with visible(person_id) as materialized (
    select p.id from person p
    where p.id = any ((select current_person_ids())::bigint[])
      or exists (
        select from app_private.tenant_relation r
        where r.person_id = p.id and r.has_roles and (
          r.tenant_id = any ((select current_admin_tenant_ids())::bigint[])
          or (r.tenant_id = (select current_tenant_id()) and r.is_current and (
            (select current_tenant_id() = any(my_tenants_array()))
            or r.current_roles && array['trainer', 'administrator']::public.tenant_role[]
          ))
        )
      )
      or p.id in (
        select visible_person_id from app_private.event_people
        where tenant_id = (select current_tenant_id())
          and viewer_person_id = any ((select current_person_ids())::bigint[])
      )
      or ((select current_admin_tenant_ids() <> '{}'::bigint[]) and exists (
        select 1 from event_registration r
        where r.person_id = p.id and r.registration_status = 'active'
          and r.tenant_id = any ((select current_admin_tenant_ids())::bigint[])
        union all
        select 1 from event_trainer t
        where t.person_id = p.id
          and t.tenant_id = any ((select current_admin_tenant_ids())::bigint[])
      ))
  )
  select person_id from visible
  union all
  select unnest(array[c.man_id, c.woman_id]) from couple c
  where include_partners and c.status = 'active'
    and (c.man_id in (select person_id from visible)
      or c.woman_id in (select person_id from visible)
      or c.id in (
        select couple_id from event_registration
        where tenant_id = (select current_tenant_id())
          and registration_status = 'active' and parent_registration_id is null and couple_id is not null
          and event_id = any ((select app_private.current_shared_event_ids())::bigint[])
      ));
$$;

grant all on function app_private.visible_person_ids(boolean) to anonymous;
