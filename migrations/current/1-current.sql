alter table event drop column if exists since, drop column if exists until;

drop function if exists sticky_announcements;

grant usage on schema app_private to anonymous;

--!include functions/event_instance_approx_price.sql
