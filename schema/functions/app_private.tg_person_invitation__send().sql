CREATE FUNCTION app_private.tg_person_invitation__send() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  perform graphile_worker.add_job('send_invitation', json_build_object('id', NEW.id));
  return NEW;
end;
$$;
