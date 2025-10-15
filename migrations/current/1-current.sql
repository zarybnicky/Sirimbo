
drop function if exists my_event_instances_for_range;
drop function if exists event_instances_for_range;

alter table if exists public.event_instance
  drop column if exists range,
  add column range tstzrange generated always as (tstzrange(since, until, '[)'::text)) stored not null;

--!include functions/event_instances_for_range.sql
--!include functions/my_event_instances_for_range.sql



create schema if not exists wdsf;
grant all on schema wdsf to postgres;
grant all on schema wdsf to olymp;

create schema if not exists csts;
grant all on schema csts to postgres;
grant all on schema csts to olymp;
