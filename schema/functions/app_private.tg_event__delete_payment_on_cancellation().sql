CREATE FUNCTION app_private.tg_event__delete_payment_on_cancellation() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  payment_id bigint;
begin
  delete from payment where event_id = OLD.id;

  if not new.is_cancelled then
    select (create_event_payment(i)).id into payment_id
    from event i
    where i.type='lesson'
      and i.id = NEW.id
      and i.since < now()
      and not i.is_cancelled
      and not exists (
        select * from payment where event_id = i.id
      );

    update payment set status ='unpaid' where id = payment_id;
    perform resolve_payment_with_credit(payment.*) from payment where id = payment_id;
  end if;

  return OLD;
end;
$$;
