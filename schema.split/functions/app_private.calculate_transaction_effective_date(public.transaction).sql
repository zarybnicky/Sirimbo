CREATE FUNCTION app_private.calculate_transaction_effective_date(t public.transaction) RETURNS timestamp with time zone
    LANGUAGE sql
    BEGIN ATOMIC
 SELECT COALESCE(( SELECT event_instance.since
            FROM (public.payment
              JOIN public.event_instance ON ((payment.event_instance_id = event_instance.id)))
           WHERE ((calculate_transaction_effective_date.t).payment_id = payment.id)), ( SELECT event_instance.since
            FROM ((public.payment
              JOIN public.event_registration ON ((payment.event_registration_id = event_registration.id)))
              JOIN public.event_instance ON ((event_instance.event_id = event_registration.event_id)))
           WHERE ((calculate_transaction_effective_date.t).payment_id = payment.id)
           ORDER BY event_instance.since
          LIMIT 1), ( SELECT payment.due_at
            FROM public.payment
           WHERE ((calculate_transaction_effective_date.t).payment_id = payment.id)), (t).created_at) AS "coalesce";
END;
