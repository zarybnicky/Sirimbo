CREATE FUNCTION app_private.tg__tenant() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  NEW.tenant = (case when TG_OP = 'INSERT' then get_current_tenant() else OLD.tenant end);
  return NEW;
end;
$$;



