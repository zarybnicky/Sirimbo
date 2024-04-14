CREATE FUNCTION app_private.tg_cohort_membership__on_status() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if NEW.status = 'expired' and (TG_OP = 'INSERT' or OLD.status <> NEW.status) then
    -- TODO: remove event_registrations for future events
    -- remove event_attendance for ongoing events
  elsif NEW.status = 'active' and (TG_OP = 'INSERT' or OLD.status <> NEW.status) then
    -- add payments
    -- add event_registrations to cohort events
  end if;
  return NEW;
end;
$$;



