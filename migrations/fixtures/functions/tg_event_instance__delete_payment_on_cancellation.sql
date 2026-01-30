CREATE or replace FUNCTION app_private.tg_event_instance__delete_payment_on_cancellation() RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER AS $$
declare
  payment_id bigint;
begin
  delete from payment where event_instance_id = OLD.id;

  if not new.is_cancelled then
    select (create_event_instance_payment(i)).id into payment_id
    from event_instance i
    where i.type='lesson'
      and i.id = NEW.id
      and not i.is_cancelled
      and i.since < now()
      and not exists (
        select * from payment where event_instance_id=i.id
      );

    update payment set status ='unpaid' where id = payment_id;
    perform resolve_payment_with_credit(payment.*) from payment where id = payment_id;
  end if;

  return OLD;
end;
$$;

select verify_function('app_private.tg_event_instance__delete_payment_on_cancellation', 'event_instance');

DROP TRIGGER IF EXISTS _500_delete_on_cancellation on public.event_instance;

CREATE TRIGGER _500_delete_on_cancellation
  AFTER UPDATE OF is_cancelled ON public.event_instance
  FOR EACH ROW
  EXECUTE FUNCTION app_private.tg_event_instance__delete_payment_on_cancellation();
