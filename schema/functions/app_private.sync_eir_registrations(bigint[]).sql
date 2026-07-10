CREATE FUNCTION app_private.sync_eir_registrations(reg_ids bigint[]) RETURNS void
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  -- couple unit rows: one per couple registration x each instance of its event
  insert into event_instance_registration
    (legacy_registration_id, tenant_id, instance_id, event_id, couple_id, target_cohort_id, note)
  select er.id, er.tenant_id, ei.id, er.event_id, er.couple_id, er.target_cohort_id, er.note
  from event_registration er
  join event_instance ei on ei.event_id = er.event_id
  where er.couple_id is not null and er.id = any (reg_ids)
  on conflict (legacy_registration_id, instance_id, (coalesce(person_id, -1))) do update
    set event_id = excluded.event_id,
        couple_id = excluded.couple_id,
        target_cohort_id = excluded.target_cohort_id,
        note = excluded.note;

  -- couple person rows: registration's people x instances, linked to the unit.
  -- New rows start 'unknown'; on conflict status/note are left as-is (native).
  insert into event_instance_registration
    (legacy_registration_id, parent_registration_id, tenant_id, instance_id, event_id, person_id, status)
  select er.id, u.id, er.tenant_id, ei.id, er.event_id, pid, 'unknown'
  from event_registration er
  join event_instance ei on ei.event_id = er.event_id
  join event_instance_registration u
    on u.legacy_registration_id = er.id and u.instance_id = ei.id and u.person_id is null
  cross join lateral app_private.event_registration_person_ids(er) as pid
  where er.couple_id is not null and er.id = any (reg_ids)
  on conflict (legacy_registration_id, instance_id, (coalesce(person_id, -1))) do update
    set parent_registration_id = excluded.parent_registration_id,
        event_id = excluded.event_id;

  -- solo rows: combined unit/person per instance
  insert into event_instance_registration
    (legacy_registration_id, tenant_id, instance_id, event_id, person_id, status, note, target_cohort_id)
  select er.id, er.tenant_id, ei.id, er.event_id, er.person_id, 'unknown', er.note, er.target_cohort_id
  from event_registration er
  join event_instance ei on ei.event_id = er.event_id
  where er.couple_id is null and er.id = any (reg_ids)
  on conflict (legacy_registration_id, instance_id, (coalesce(person_id, -1))) do update
    set event_id = excluded.event_id,
        note = excluded.note,
        target_cohort_id = excluded.target_cohort_id,
        parent_registration_id = excluded.parent_registration_id,
        couple_id = excluded.couple_id;

  -- orphan sweep: rows no longer in the registration x instances product
  -- (registration cancelled, instance removed, couple recomposed).
  delete from event_instance_registration e
  where e.legacy_registration_id = any (reg_ids)
    and not exists (
      select 1 from event_registration er
      join event_instance ei on ei.event_id = er.event_id
      where er.id = e.legacy_registration_id and ei.id = e.instance_id
        and ((e.person_id is null and er.couple_id is not null)
          or (e.person_id is not null and e.person_id in (select app_private.event_registration_person_ids(er))))
    );
end;
$$;

GRANT ALL ON FUNCTION app_private.sync_eir_registrations(reg_ids bigint[]) TO anonymous;
