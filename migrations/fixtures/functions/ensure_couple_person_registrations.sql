create or replace function app_private.ensure_couple_person_registrations(p_registration_id bigint)
returns void language plpgsql
set search_path = pg_catalog, public, pg_temp
as $$
declare
  parent event_registration;
  child event_registration;
  partner_id bigint;
begin
  perform e.id from event e
  join event_registration r on r.event_id = e.id
  where r.id = p_registration_id for update of e;

  select * into parent from event_registration
  where id = p_registration_id and couple_id is not null and registration_status = 'active';
  if not found then return; end if;

  for partner_id in
    select unnest(array[man_id, woman_id]) from couple where id = parent.couple_id
  loop
    select * into child from event_registration
    where event_id = parent.event_id and person_id = partner_id
    order by (registration_status = 'active') desc,
      (parent_registration_id = parent.id) desc nulls last,
      (parent_registration_id is null) desc, id desc
    limit 1 for update;

    if found then
      if child.registration_status = 'active' and child.parent_registration_id is not null
        and child.parent_registration_id <> parent.id then
        raise exception 'PERSON_ALREADY_REGISTERED' using errcode = '22023';
      end if;

      -- Keep attendance, notes and personal lesson requests on the same row.
      update event_registration
      set parent_registration_id = parent.id, registration_status = 'active',
          source = parent.source, target_cohort_id = null
      where id = child.id and (parent_registration_id, registration_status, source, target_cohort_id)
        is distinct from (parent.id, 'active'::event_registration_status, parent.source, null);
    else
      insert into event_registration (event_id, tenant_id, parent_registration_id, person_id, status, source)
      values (parent.event_id, parent.tenant_id, parent.id, partner_id, 'unknown', parent.source);
    end if;
  end loop;
end;
$$;

revoke all on function app_private.ensure_couple_person_registrations from public;
grant execute on function app_private.ensure_couple_person_registrations to trainer, administrator;
