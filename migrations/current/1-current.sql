--! include functions/event_instance_registration_last_attended.sql
--! include functions/event_instance_approx_price.sql
--! include functions/set_event_instance_registration.sql
--! include functions/set_lesson_demand.sql

drop function if exists federated.competition_sankey_links(integer, integer, text[], text[], boolean);
