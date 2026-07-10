drop index if exists public.event_instance_registration_person_key;
create unique index event_instance_registration_person_key
  on public.event_instance_registration (instance_id, person_id)
  where person_id is not null
    and legacy_registration_id is null
    and registration_status = 'active';

drop index if exists public.payment_event_instance_id_idx;
create unique index payment_event_instance_id_idx
  on public.payment (event_instance_id)
  where event_instance_id is not null;

--! include functions/create_event_instance_payment.sql

--! include functions/tg_event_instance__delete_payment_on_cancellation.sql

--! include functions/create_latest_lesson_payments.sql

--! include functions/event_instance_approx_price.sql

--! include functions/update_event_instance_details.sql

--! include functions/activity_timeline.sql

drop function if exists public.event_registration_last_attended(
  public.event_registration
);

drop function if exists public.update_event_attendance(
  bigint, bigint, public.attendance_type, text
);
drop view if exists public.event_attendance;
