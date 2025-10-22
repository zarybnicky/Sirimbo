--! Previous: sha1:6cadeebf7f25d2831104f7c06e3837aa0ec1089e
--! Hash: sha1:67fc475ace68bc63859c066f7b524d67ef60d19c

--! split: 1-current.sql
drop function if exists my_event_instances_for_range;
drop function if exists event_instances_for_range;

alter table if exists public.event_instance
  drop column if exists range,
  add column range tstzrange generated always as (tstzrange(since, until, '[)'::text)) stored not null;

CREATE or replace FUNCTION public.event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, only_mine boolean = false, trainer_ids bigint[] = null) RETURNS SETOF event_instance LANGUAGE sql STABLE
begin atomic
  select event_instance.*
  from event_instance
  join event on event_id=event.id
  where event.is_visible
    and event_instance.since <= end_range
    and event_instance.until >= start_range
    and (only_type is null or event.type = only_type)
    and (trainer_ids is null
      or exists (select 1 from event_trainer where person_id = any (trainer_ids) and event_id = event.id)
      or exists (select 1 from event_instance_trainer where person_id = any (trainer_ids) and instance_id=event_instance.id));
end;
COMMENT ON FUNCTION public.event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.event_instances_for_range TO anonymous;

CREATE or replace FUNCTION public.my_event_instances_for_range(only_type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone, only_mine boolean = false) RETURNS SETOF event_instance LANGUAGE sql STABLE
begin atomic
  select distinct on (instances.id) instances.*
  from event_instances_for_range(only_type, start_range, end_range) instances
  left join event_registration on event_registration.event_id=instances.event_id and (event_registration.person_id = any(current_person_ids()) or event_registration.couple_id = any(current_couple_ids()))
  left join event_trainer on event_trainer.event_id=instances.event_id and event_trainer.person_id = any(current_person_ids())
  left join event_instance_trainer on event_instance_trainer.instance_id=instances.id and event_instance_trainer.person_id = any(current_person_ids())
  where event_registration.id is not null or event_trainer.id is not null or event_instance_trainer.id is not null;
end;
COMMENT ON FUNCTION public.my_event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.my_event_instances_for_range TO anonymous;


alter table tenant_administrator alter column tenant_id set default current_tenant_id();
alter table tenant_membership alter column tenant_id set default current_tenant_id();
alter table tenant_trainer alter column tenant_id set default current_tenant_id();
alter table tenant_location alter column tenant_id set default current_tenant_id();

REVOKE ALL ON TABLE public.scoreboard_manual_adjustment FROM administrator;
REVOKE ALL ON TABLE public.scoreboard_manual_adjustment FROM member;
GRANT ALL ON TABLE public.scoreboard_manual_adjustment TO anonymous;

select app_private.drop_policies('public.scoreboard_manual_adjustment');
create policy current_tenant on scoreboard_manual_adjustment as restrictive using (tenant_id = (select current_tenant_id()));
CREATE POLICY admin_all ON scoreboard_manual_adjustment TO administrator USING (true);
CREATE POLICY member_read ON public.scoreboard_manual_adjustment FOR SELECT USING (true);

create schema if not exists wdsf;
grant all on schema wdsf to postgres;
grant all on schema wdsf to olymp;

create schema if not exists csts;
grant all on schema csts to postgres;
grant all on schema csts to olymp;
