CREATE FUNCTION app_private.tg_users__notify_admin() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  perform graphile_worker.add_job('notify_admin_registration', json_build_object('id', NEW.u_id));
  return NEW;
end;
$$;

SET default_tablespace = '';

SET default_table_access_method = heap;



