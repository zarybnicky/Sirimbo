CREATE FUNCTION app_private.tg_transaction__effective_date() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if NEW.effective_date is null then
    NEW.effective_date = NEW.created_at;
  end if;
  return NEW;
end;
$$;



