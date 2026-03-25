CREATE FUNCTION app_private.create_latest_lesson_payments() RETURNS SETOF public.payment
    LANGUAGE plpgsql
    AS $$
begin
  perform set_config('jwt.claims.tenant_id', '2', true);
  perform set_config('jwt.claims.my_tenant_ids', '[2]', true);
  if not (select row_security_active('event')) then
    set local role to administrator;
  end if;

  return query WITH created AS (
    SELECT p.*
    FROM event_instance ei
      JOIN event e ON e.id = ei.event_id
      JOIN LATERAL create_event_instance_payment(ei) p ON true
    WHERE e.type = 'lesson'
      AND NOT ei.is_cancelled
      AND ei.since < now()
      AND NOT EXISTS (
        SELECT 1
        FROM payment p
        WHERE p.event_instance_id = ei.id AND p.status = 'paid'
      )
      AND p.status in ('unpaid', 'tentative')
  ),
  unpaid AS (
    UPDATE payment p
      SET status = 'unpaid'
      FROM created
      WHERE p.id = created.id
      RETURNING p.*
  )
  SELECT p.*
  FROM unpaid
    CROSS JOIN LATERAL resolve_payment_with_credit(unpaid.*) p
  WHERE p IS NOT NULL;
end;
$$;
