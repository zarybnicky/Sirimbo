--! include functions/set_event_instance_registration.sql
--! include functions/update_event_instance_details.sql
--! include functions/upsert_event.sql

comment on table public.event_instance_registration is E'@omit create,update,delete\n@simpleCollections both';

comment on function public.register_to_event_many(public.register_to_event_type[]) is '@omit';
comment on function public.edit_registration(bigint, text) is '@omit';
comment on function public.cancel_registration(bigint) is '@omit';
