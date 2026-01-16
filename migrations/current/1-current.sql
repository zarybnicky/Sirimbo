
--!include functions/create_event_instance_payment.sql
--!include functions/resolve_payment_with_credit.sql
--!include functions/event_instance_approx_price.sql
--!include functions/create_latest_lesson_payments.sql
--!include functions/tg_event_instance__delete_payment_on_cancellation.sql
--!include functions/upsert_event.sql

alter table event drop column if exists payment_type;

drop type if exists event_payment_type;
