CREATE FUNCTION public.regenerate_event_registration_from_attendees() RETURNS void
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  insert into event_registration (tenant_id, event_id, person_id, note, is_confirmed)
  select
    attendee_user.tenant_id,
    attendee_user.event_id,
    (select id from person where legacy_user_id = attendee_user.user_id),
    case attendee_user.notes when '' then null else attendee_user.notes end,
    true
  from attendee_user;
end;
$$;



