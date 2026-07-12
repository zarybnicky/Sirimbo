drop type if exists series_info cascade;
create type series_info as (
  id bigint,
  name text,
  position integer,
  length integer,
  since timestamptz,
  until timestamptz
);

grant usage on type public.series_info to anonymous;

--! include functions/event_instance_series_info.sql
--! include functions/update_event_instance_details.sql
