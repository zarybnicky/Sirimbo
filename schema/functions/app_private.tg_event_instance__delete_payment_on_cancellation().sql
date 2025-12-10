CREATE FUNCTION app_private.tg_event_instance__delete_payment_on_cancellation() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
declare
  payment_id bigint;
begin
  delete from payment where event_instance_id = OLD.id;

  if not new.is_cancelled then
    select (create_event_instance_payment(event_instance)).id into payment_id
    from event_instance join event on event.id=event_id
    where type='lesson'
      and event_instance.id = NEW.id
      and not event_instance.is_cancelled
      and event_instance.since < now()
      and payment_type = 'after_instance'
      and not exists (
        select * from payment where event_instance_id=event_instance.id
      );

    update payment set status ='unpaid' where id = payment_id;
    perform resolve_payment_with_credit(payment.*) from payment where id = payment_id;
  end if;

  return OLD;
end;
$$;
