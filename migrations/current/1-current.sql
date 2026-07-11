drop function if exists public.event_instance_target_cohorts(public.event_instance);
drop function if exists app_private.register_new_cohort_member_to_events(public.cohort_membership);
drop function if exists app_private.tg_event_target_cohort__register_members();
drop function if exists app_private.tg_event_target_cohort__unregister_members();
drop function if exists app_private.detach_event_instance_impl(bigint, bigint, text);

drop function if exists public.register_to_event_many;
drop function if exists public.edit_registration(bigint, text);
drop function if exists public.cancel_registration(bigint);
drop type if exists public.register_to_event_type;

drop function if exists public.upsert_event;
drop type if exists public.event_registration_type_input;

--! include functions/upsert_event.sql
