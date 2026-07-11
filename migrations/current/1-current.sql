--! include functions/set_event_instance_registration.sql
--! include functions/update_event_instance_details.sql

comment on table public.event_instance_registration is E'@omit create,update,delete\n@simpleCollections both';
