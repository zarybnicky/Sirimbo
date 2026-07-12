do $$
begin
  if exists (
    select 1
    from public.event_instance instance
    where instance.event_id is not null
      and (
        instance.type is null
        or instance.location_text is null
        or instance.is_visible is null
        or instance.is_public is null
        or instance.capacity is null
        or instance.description is null
        or instance.summary is null
        or instance.is_locked is null
        or instance.enable_notes is null
        or instance.files_legacy is null
      )
  ) then
    raise exception 'LEGACY_LINKED_INSTANCE_WITH_INCOMPLETE_DATA';
  end if;
end
$$;

drop function if exists public.event_overlaps_attendee_report(timestamptz, timestamptz);
drop function if exists public.event_overlaps_trainer_report(timestamptz, timestamptz);
alter type public.event_overlaps_conflict drop attribute if exists first_event_id;
alter type public.event_overlaps_conflict drop attribute if exists second_event_id;

--! include functions/event_instance_registration_last_attended.sql
--! include functions/event_overlaps_reports.sql
--! include functions/quick_create_event_instances.sql
--! include functions/update_event_instance_details.sql
--! include functions/set_event_instance_registration.sql
