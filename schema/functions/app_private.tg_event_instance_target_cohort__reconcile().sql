CREATE FUNCTION app_private.tg_event_instance_target_cohort__reconcile() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  if tg_op = 'DELETE' then
    perform app_private.reconcile_event_instance_cohort_registrations(array[old.instance_id]);
    return old;
  end if;

  perform app_private.reconcile_event_instance_cohort_registrations(
    case when tg_op = 'UPDATE' and old.instance_id <> new.instance_id
      then array[old.instance_id, new.instance_id]
      else array[new.instance_id]
    end
  );
  return new;
end;
$$;

REVOKE ALL ON FUNCTION app_private.tg_event_instance_target_cohort__reconcile() FROM PUBLIC;
