CREATE or replace FUNCTION app_private.tg_transaction__effective_date() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  if NEW.effective_date is null then
    NEW.effective_date = (select coalesce(
      (select since from payment join event_instance on event_instance_id = event_instance.id where NEW.payment_id = payment.id),
      (select due_at from payment where NEW.payment_id = payment.id),
      NEW.created_at
    ));
  end if;
  return NEW;
end;
$$;

CREATE or replace TRIGGER _300_effective_date
  BEFORE INSERT OR UPDATE ON transaction
  FOR EACH ROW
  EXECUTE PROCEDURE app_private.tg_transaction__effective_date();
