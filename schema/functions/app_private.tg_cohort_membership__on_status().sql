CREATE FUNCTION app_private.tg_cohort_membership__on_status() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  if NEW.status = 'expired' and (TG_OP = 'INSERT' or OLD.status <> NEW.status) then
    -- with affected as (
    --   select cohort_membership.until, event_registration.person_id, event_registration.id as registration_id
    --   from event_target_cohort
    --   join event_registration on event_target_cohort.event_id=event_registration.event_id
    --   join cohort_membership on cohort_membership.person_id=event_registration.person_id and cohort_membership.cohort_id=event_target_cohort.cohort_id
    --   where cohort_membership.until is not null
    --     and cohort_membership.id = OLD.id
    -- )
    -- update event_attendance set status = 'cancelled'
    -- where id in (
    --   select event_attendance.id
    --   from event_attendance
    --   join event_instance on event_instance.id = event_attendance.instance_id
    --   join affected on event_attendance.registration_id = affected.registration_id
    --    and affected.until < event_instance.since
    --    and status in ('unknown', 'not-excused', 'excused')
    -- );

    -- with affected as (
    --   select cohort_membership.until, event_registration.person_id, event_registration.id as registration_id
    --   from event_target_cohort
    --   join event_registration on event_target_cohort.event_id=event_registration.event_id
    --   join cohort_membership on cohort_membership.person_id=event_registration.person_id and cohort_membership.cohort_id=event_target_cohort.cohort_id
    --   where cohort_membership.until is not null
    --   and cohort_membership.id = OLD.id
    -- )
    -- delete from event_registration
    -- where exists (
    --   select event_attendance.id
    --   from event_attendance
    --   join event_instance on event_instance.id = event_attendance.instance_id
    --   join affected on event_attendance.registration_id = affected.registration_id
    --    and affected.until < event_instance.since
    --    and status = 'cancelled'
    -- ) and not exists (
    --   select event_attendance.id
    --   from event_attendance
    --   join event_instance on event_instance.id = event_attendance.instance_id
    --   join affected on event_attendance.registration_id = affected.registration_id
    --    and affected.until < event_instance.since
    --    and status <> 'cancelled'
    -- );

  elsif NEW.status = 'active' and (TG_OP = 'INSERT' or OLD.status <> NEW.status) then
    -- add payments
    perform app_private.register_new_cohort_member_to_events(NEW);
  end if;
  return NEW;
end;
$$;

GRANT ALL ON FUNCTION app_private.tg_cohort_membership__on_status() TO trainer;
