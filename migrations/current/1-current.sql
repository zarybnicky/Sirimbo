-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION app_private.can_trainer_edit_event(eid bigint)
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION app_private.queue_announcement_notifications(in_announcement_id bigint)
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION app_private.tg_account_balances__update()
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION app_private.tg_announcement__after_write()
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION app_private.tg_announcement_audience__after_write()
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION app_private.tg_auth_details__refresh()
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION app_private.tg_auth_details__update()
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION app_private.tg_cohort_membership__on_status()
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION app_private.tg_event_instance__create_attendance()
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION app_private.tg_event_instance__delete_payment_on_cancellation()
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION app_private.tg_event_instance__update_parent_range()
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION app_private.tg_event_registration__create_attendance()
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION app_private.tg_event_registration__delete_attendance()
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION app_private.tg_event_target_cohort__register_members()
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION app_private.tg_event_target_cohort__unregister_members()
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION app_private.tg_payment__fill_accounting_period()
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION app_private.tg_person_invitation__send()
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION app_private.tg_transaction__effective_date()
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION public.event_remaining_lessons(e event)
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION public.event_remaining_person_spots(e event)
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION public.get_current_tenant()
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION public.get_current_user(version_id text)
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION public.invitation_info(token uuid)
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION public.invitation_name(token uuid)
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION public.log_in_as(id bigint, OUT usr users, OUT jwt jwt_token)
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION public.login(login character varying, passwd character varying, OUT usr users, OUT jwt jwt_token)
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION public.otp_login(token uuid, OUT usr users, OUT jwt jwt_token)
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION public.person_account(p_id bigint, c text, OUT acc account)
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION public.refresh_jwt()
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION public.register_to_event_many(registrations register_to_event_type[])
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION public.register_to_event(INOUT registration event_registration, lessons event_lesson_demand[])
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION public.register_using_invitation(email text, passwd text, token uuid, login text, OUT usr users, OUT jwt jwt_token)
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION public.register_without_invitation(email text, passwd text, OUT usr users, OUT jwt jwt_token)
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION public.reset_password(email character varying)
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION public.set_lesson_demand(registration_id bigint, trainer_id bigint, lesson_count integer)
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION public.submit_form(type text, data jsonb, url text)
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION public.tenant_account(c text, OUT acc account)
  SET search_path = pg_catalog, public, pg_temp;


-- security-definer-search-path: It's insecure to not specify a search_path on a SECURITY DEFINER function.
-- Update the search_path of the function to be more secure.
ALTER FUNCTION public.verify_function(f regproc, relid regclass)
  SET search_path = pg_catalog, public, pg_temp;


-- sequence-usage: Some of the roles that can INSERT to tables that this sequence is used in are not allowed to use this sequence. This will cause runtime errors on INSERT.
-- Grant USAGE on table's primary key sequence
--! The roles listed will now be able to insert into this table (change in security).
GRANT USAGE ON SEQUENCE public.scoreboard_manual_adjustment_id_seq
  TO administrator;


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.account related to a public.person may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.account (person_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.accounting_period related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.accounting_period (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.aktuality related to a public.galerie_foto may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.aktuality (at_foto_main);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.announcement_audience related to a public.cohort may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.announcement_audience (cohort_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.attachment related to a public.users may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.attachment (uploaded_by);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.cohort_group related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.cohort_group (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.cohort_subscription related to a public.account may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.cohort_subscription (account_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.cohort_subscription related to a public.cohort may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.cohort_subscription (cohort_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.cohort_subscription related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.cohort_subscription (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.cohort related to a public.cohort_group may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.cohort (cohort_group_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.cohort related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.cohort (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.couple related to a public.person may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.couple (man_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.couple related to a public.person may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.couple (woman_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.dokumenty related to a public.users may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.dokumenty (d_kdo);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.dokumenty related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.dokumenty (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.event_attendance related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.event_attendance (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.event_external_registration related to a public.users may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.event_external_registration (created_by);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.event_external_registration related to a public.event may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.event_external_registration (event_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.event_external_registration related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.event_external_registration (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.event_instance related to a public.tenant_location may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.event_instance (location_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.event_lesson_demand related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.event_lesson_demand (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.event_registration related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.event_registration (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.event_target_cohort related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.event_target_cohort (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.event related to a public.tenant_location may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.event (location_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.event related to a public.account may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.event (payment_recipient_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.form_responses related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.form_responses (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.galerie_dir related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.galerie_dir (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.galerie_foto related to a public.galerie_dir may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.galerie_foto (gf_id_rodic);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.galerie_foto related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.galerie_foto (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.membership_application related to a public.users may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.membership_application (created_by);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.membership_application related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.membership_application (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.otp_token related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.otp_token (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.otp_token related to a public.users may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.otp_token (user_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.payment_debtor related to a public.payment may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.payment_debtor (payment_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.payment_debtor related to a public.person may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.payment_debtor (person_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.payment_debtor related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.payment_debtor (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.payment_recipient related to a public.account may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.payment_recipient (account_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.payment_recipient related to a public.payment may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.payment_recipient (payment_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.payment_recipient related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.payment_recipient (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.payment related to a public.accounting_period may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.payment (accounting_period_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.payment related to a public.cohort_subscription may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.payment (cohort_subscription_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.payment related to a public.event_instance may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.payment (event_instance_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.payment related to a public.event_registration may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.payment (event_registration_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.payment related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.payment (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.person_invitation related to a public.person may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.person_invitation (person_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.person_invitation related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.person_invitation (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.platby_category_group related to a public.platby_category may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.platby_category_group (pcg_id_category);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.platby_category_group related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.platby_category_group (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.platby_category related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.platby_category (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.platby_group_skupina related to a public.platby_group may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.platby_group_skupina (pgs_id_group);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.platby_group_skupina related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.platby_group_skupina (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.platby_group related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.platby_group (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.platby_item related to a public.platby_category may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.platby_item (pi_id_category);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.platby_item related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.platby_item (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.platby_raw related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.platby_raw (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.posting related to a public.account may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.posting (account_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.posting related to a public.account may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.posting (original_account_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.posting related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.posting (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.posting related to a public.transaction may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.posting (transaction_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.tenant_administrator related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.tenant_administrator (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.tenant_location related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.tenant_location (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.tenant_trainer related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.tenant_trainer (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.transaction related to a public.accounting_period may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.transaction (accounting_period_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.transaction related to a public.payment may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.transaction (payment_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.transaction related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.transaction (tenant_id);


-- indexes-fk: Foreign key doesn't have matching local index, so finding all the public.users related to a public.tenant may require a slow table scan.
-- Create an index over the foreign key's columns
CREATE INDEX ON public.users (tenant_id);


-- unreachable-grant: Granted anonymous=X/root on function `app_private.can_trainer_edit_event(eid bigint)`, but role `anonymous` does not have access to schema `app_private`.
-- Revoke permissions that couldn't be used.
REVOKE ALL
  ON FUNCTION app_private.can_trainer_edit_event(eid bigint)
  FROM anonymous;


-- unreachable-grant: Granted administrator=X/root on function `app_private.cron_update_memberships()`, but role `administrator` does not have access to schema `app_private`.
-- Revoke permissions that couldn't be used.
REVOKE ALL
  ON FUNCTION app_private.cron_update_memberships()
  FROM administrator;


-- unreachable-grant: Granted anonymous=X/olymp on function `app_private.is_system_admin(bigint)`, but role `anonymous` does not have access to schema `app_private`.
-- Revoke permissions that couldn't be used.
REVOKE ALL
  ON FUNCTION app_private.is_system_admin(bigint)
  FROM anonymous;


-- unreachable-grant: Granted anonymous=X/olymp on function `app_private.queue_announcement_notifications(in_announcement_id bigint)`, but role `anonymous` does not have access to schema `app_private`.
-- Revoke permissions that couldn't be used.
REVOKE ALL
  ON FUNCTION app_private.queue_announcement_notifications(in_announcement_id bigint)
  FROM anonymous;


-- unreachable-grant: Granted trainer=X/root on function `app_private.tg_cohort_membership__on_status()`, but role `trainer` does not have access to schema `app_private`.
-- Revoke permissions that couldn't be used.
REVOKE ALL
  ON FUNCTION app_private.tg_cohort_membership__on_status()
  FROM trainer;


-- unreachable-grant: Granted administrator=r/olymp on table `app_private.system_admin_user`, but role `administrator` does not have access to schema `app_private`.
-- Revoke permissions that couldn't be used.
REVOKE ALL
  ON app_private.system_admin_user
  FROM administrator;

ALTER TABLE public.scoreboard_manual_adjustment
  DROP CONSTRAINT IF EXISTS  scoreboard_manual_adjustment_tenant_id_fkey,
  ADD CONSTRAINT scoreboard_manual_adjustment_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant;
