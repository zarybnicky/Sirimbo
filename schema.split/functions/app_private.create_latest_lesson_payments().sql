CREATE FUNCTION app_private.create_latest_lesson_payments() RETURNS SETOF public.payment
    LANGUAGE plpgsql
    AS $$
declare
  v_id bigint;
  created_ids bigint[] := array[]::bigint[];
begin
  perform set_config('jwt.claims.tenant_id', '2', true);
  perform set_config('jwt.claims.my_tenant_ids', '[2]', true);
  if not (select row_security_active('event')) then
    set local role to administrator;
  end if;

  update event set payment_type='after_instance'
  where type='lesson' and payment_type <> 'after_instance';

  select array_agg((create_event_instance_payment(event_instance)).id) into created_ids
  from event_instance join event on event.id=event_id
  where type='lesson'
    and event_instance.since < now()
    and payment_type = 'after_instance'
    and not exists (
      select * from payment where event_instance_id=event_instance.id
    );

  update payment set status ='unpaid' where id = any (created_ids);

  foreach v_id in array created_ids loop
    perform resolve_payment_with_credit(payment.*) from payment where id = v_id;
  end loop;

  return query select * from payment where id = any (created_ids);
end;
$$;



