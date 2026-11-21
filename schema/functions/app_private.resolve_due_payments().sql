CREATE FUNCTION app_private.resolve_due_payments() RETURNS SETOF public.payment
    LANGUAGE plpgsql
    AS $$
declare
  tenant_ids bigint[] := array(select id from tenant);
  v_tenant_id bigint;
  payment_row payment;
begin
  if not row_security_active('payment') then
    set local role to administrator;
  end if;
  foreach v_tenant_id in array tenant_ids loop
    perform set_config('jwt.claims.tenant_id', v_tenant_id::text, true);
    perform set_config('jwt.claims.my_tenant_ids', json_build_array(v_tenant_id)::text, true);
    for payment_row in
      update payment p set status = 'unpaid'
      where p.tenant_id = v_tenant_id and p.cohort_subscription_id is not null
        and p.due_at <= now()
        and (p.status = 'tentative' or (p.status = 'unpaid' and p.is_auto_credit_allowed))
      returning p.*
    loop
      return next coalesce(resolve_payment_with_credit(payment_row), payment_row);
    end loop;
  end loop;
end;
$$;
