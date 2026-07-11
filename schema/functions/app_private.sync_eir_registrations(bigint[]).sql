CREATE FUNCTION app_private.sync_eir_registrations(reg_ids bigint[]) RETURNS void
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  -- Legacy targetless registrations do not record who created them, so their
  -- source stays null; cohort-backed roots can be classified exactly.
  insert into event_instance_registration
    (legacy_registration_id, tenant_id, instance_id, event_id, couple_id,
      target_cohort_id, source, note)
  select er.id, er.tenant_id, ei.id, er.event_id, er.couple_id,
    target.cohort_id,
    case when target.cohort_id is not null then 'cohort'::event_registration_source end,
    er.note
  from event_registration er
  join event_instance ei on ei.event_id = er.event_id
  left join event_target_cohort target on target.id = er.target_cohort_id
  where er.couple_id is not null and er.id = any (reg_ids)
  on conflict (legacy_registration_id, instance_id, (coalesce(person_id, -1))) do update
    set event_id = excluded.event_id,
        couple_id = excluded.couple_id,
        note = excluded.note;

  insert into event_instance_registration
    (legacy_registration_id, parent_registration_id, tenant_id, instance_id, event_id, person_id, status)
  select er.id, unit.id, er.tenant_id, ei.id, er.event_id, people.person_id, 'unknown'
  from event_registration er
  join event_instance ei on ei.event_id = er.event_id
  join event_instance_registration unit
    on unit.legacy_registration_id = er.id and unit.instance_id = ei.id and unit.person_id is null
  cross join lateral app_private.event_registration_person_ids(er) as people(person_id)
  where er.couple_id is not null and er.id = any (reg_ids)
  on conflict (legacy_registration_id, instance_id, (coalesce(person_id, -1))) do update
    set parent_registration_id = excluded.parent_registration_id,
        event_id = excluded.event_id;

  insert into event_instance_registration
    (legacy_registration_id, tenant_id, instance_id, event_id, person_id, status,
      note, target_cohort_id, source)
  select er.id, er.tenant_id, ei.id, er.event_id, er.person_id, 'unknown', er.note,
    target.cohort_id,
    case when target.cohort_id is not null then 'cohort'::event_registration_source end
  from event_registration er
  join event_instance ei on ei.event_id = er.event_id
  left join event_target_cohort target on target.id = er.target_cohort_id
  where er.couple_id is null and er.id = any (reg_ids)
  on conflict (legacy_registration_id, instance_id, (coalesce(person_id, -1))) do update
    set event_id = excluded.event_id,
        note = excluded.note,
        parent_registration_id = excluded.parent_registration_id,
        couple_id = excluded.couple_id;

  delete from event_instance_registration registration
  where registration.legacy_registration_id = any (reg_ids)
    and not exists (
      select 1
      from event_registration er
      join event_instance ei on ei.event_id = er.event_id
      where er.id = registration.legacy_registration_id
        and ei.id = registration.instance_id
        and (
          (registration.person_id is null and er.couple_id is not null)
          or registration.person_id in (select app_private.event_registration_person_ids(er))
        )
    );
end;
$$;

GRANT ALL ON FUNCTION app_private.sync_eir_registrations(reg_ids bigint[]) TO anonymous;
