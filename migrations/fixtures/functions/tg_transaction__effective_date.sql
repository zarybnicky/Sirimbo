create or replace function app_private.tg_transaction__effective_date() returns trigger
    language plpgsql security definer
    set search_path to pg_catalog, public, pg_temp
    as $$
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

create or replace trigger _300_effective_date
  before insert or update on transaction
  for each row
  execute procedure app_private.tg_transaction__effective_date();
