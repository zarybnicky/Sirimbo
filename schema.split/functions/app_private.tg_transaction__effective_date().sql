CREATE FUNCTION app_private.tg_transaction__effective_date() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  if NEW.effective_date is null then
    NEW.effective_date = app_private.calculate_transaction_effective_date(NEW);
  end if;
  return NEW;
end;
$$;



