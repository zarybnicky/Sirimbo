grant all on membership_application to anonymous;
grant execute on function app_private.normalize_name to anonymous;
grant execute on function app_private.relationship_status_next to administrator;

--!include functions/current_account_ids.sql
--!include functions/visible_payment_ids.sql
--!include policies/accounting.sql
--!include functions/payment_debtor_price.sql
--!include policies/event_instance.sql
--!include policies/event_instance_trainer.sql
--!include policies/event_external_registration.sql
--!include policies/event_lesson_demand.sql
--!include policies/announcement.sql
--!include functions/save_events.sql
--!include functions/visible_user_proxy_ids.sql
--!include policies/users.sql
--!include functions/announcement_author_name.sql
--!include policies/membership_application.sql
