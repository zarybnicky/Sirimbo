CREATE FUNCTION app_private.tg_event_target_cohort__unregister_members() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  delete from event_registration where target_cohort_id = OLD.id;
  return OLD;
end;
$$;
