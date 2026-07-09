CREATE FUNCTION app_private.tg_event_attendance__propagate_status() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  update event_instance_registration eir
    set status = new.status
  where eir.legacy_registration_id = new.registration_id
    and eir.instance_id = new.instance_id
    and eir.person_id = new.person_id
    and eir.status is distinct from new.status;
  return null;
end;
$$;
