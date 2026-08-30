create or replace function app_private.tg_event_instance__delete_payment_on_cancellation()
  returns trigger
  language plpgsql
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
declare
  payment_id bigint;
begin
  delete from payment where event_instance_id = OLD.id;

  if not new.is_cancelled then
    select (create_event_instance_payment(i)).id into payment_id
    from event_instance i
    where i.type='lesson'
      and i.id = NEW.id
      and i.since < now()
      and not i.is_cancelled
      and not exists (
        select * from payment where event_instance_id = i.id
      );

    update payment set status ='unpaid' where id = payment_id;
    perform resolve_payment_with_credit(payment.*) from payment where id = payment_id;
  end if;

  return OLD;
end;
$$;

drop trigger if exists _500_delete_on_cancellation on event_instance;

create trigger _500_delete_on_cancellation
  after update of is_cancelled on event_instance
  for each row
  when (old.is_cancelled is distinct from new.is_cancelled)
  execute function app_private.tg_event_instance__delete_payment_on_cancellation();
