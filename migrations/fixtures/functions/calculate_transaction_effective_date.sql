create or replace function app_private.calculate_transaction_effective_date(t transaction)
  returns timestamptz language sql volatile
begin atomic
  select coalesce(
    (select since from payment join event_instance on event_instance_id = event_instance.id where t.payment_id = payment.id),
    (select due_at from payment where t.payment_id = payment.id),
    t.created_at
  );
end;
