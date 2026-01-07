--! Previous: sha1:9bbb0a39906aa165fc394ab662acc05e906c4d16
--! Hash: sha1:eb5a6288eb4d8a39271d9f59c13d77a2e52f0d74

--! split: 1-current.sql
alter table event
  drop column if exists since,
  drop column if exists until;

DROP TRIGGER IF EXISTS _500_update_parent_range on public.event_instance;
drop function if exists app_private.tg_event_instance__update_parent_range;

drop index if exists u_jmeno;
drop index if exists u_prijmeni;
drop index if exists u_confirmed;

alter table users drop column if exists u_confirmed;

do $$
begin
  if exists (select * from information_schema.columns where table_name = 'users' and column_name = 'u_created_at') then
    update users set u_created_at = created_at where created_at is null;
    alter table users drop column created_at;
    alter table users rename column u_created_at to created_at;
  end if;
end;
$$;

drop function if exists sticky_announcements;

grant usage on schema app_private to anonymous;

DO $do$ BEGIN
  IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname='system_admin') THEN
    CREATE ROLE system_admin;
  END IF;
END $do$;
grant administrator to system_admin;

create or replace function public.event_instance_approx_price(v_instance event_instance)
  returns table (amount numeric(19,4), currency text)
  language sql stable
as $$
  with stats as (
    select
      (select count(*)
       from event e
       join lateral event_registrants(e.*) r on true
       where e.id = v_instance.event_id)::bigint as num_participants,
      extract(epoch from (v_instance.until - v_instance.since)) / 60.0 as duration
  )
  select
    sum((tt.member_price_45min).amount * s.duration / 45 / s.num_participants) as amount,
    (tt.member_price_45min).currency as currency
  from stats s
  join lateral public.event_instance_trainers(v_instance) tt on true
  where
    s.num_participants > 0
    and s.duration > 0
    and tt.member_price_45min is not null
    and (tt.member_price_45min).amount is not null
    and (tt.member_price_45min).currency is not null
  group by (tt.member_price_45min).currency;
$$;

grant all on function event_instance_approx_price to anonymous;
COMMENT ON FUNCTION event_instance_approx_price IS '@simpleCollections only';

create or replace function app_private.create_jwt_token(u users) returns jwt_token
    language sql stable
as $$
with
  person_ids as (
    select distinct up.person_id from user_proxy up where up.user_id = u.id
  ),
  tenant_memberships as (
    select distinct tm.tenant_id from tenant_membership tm join person_ids p on p.person_id = tm.person_id where tm.status = 'active'
  ),
  tenant_trainers as (
    select distinct tt.tenant_id from tenant_trainer tt join person_ids p on p.person_id = tt.person_id where tt.status = 'active'
  ),
  tenant_admins as (
    select distinct ta.tenant_id from tenant_administrator ta join person_ids p on p.person_id = ta.person_id where ta.status = 'active'
    union
    select id from tenant where app_private.is_system_admin(u.id)
  ),
  tenant_ids as (
    select tenant_id from tenant_memberships
    union
    select tenant_id from tenant_trainers
    union
    select tenant_id from tenant_admins
  ),
  cohort_ids as (
    select distinct cm.cohort_id from cohort_membership cm join person_ids p on p.person_id = cm.person_id where cm.status = 'active'
  ),
  couple_ids as (
    select distinct c.id from couple c join person_ids p on p.person_id = c.man_id where c.status = 'active'
    union all
    select distinct c.id from couple c join person_ids p on p.person_id = c.woman_id where c.status = 'active'
  )
  select
    extract(epoch from now() + interval '7 days')::integer as exp,
    u.id as user_id,
    (select current_tenant_id()) as tenant_id,
    u.u_login as username,
    u.u_email as email,
    coalesce(to_json((select array_agg(p.person_id) from person_ids p)), '[]'::json) as my_person_ids,
    coalesce(to_json((select array_agg(t.tenant_id) from tenant_ids t)), '[]'::json) as my_tenant_ids,
    coalesce(to_json((select array_agg(c.cohort_id) from cohort_ids c)), '[]'::json) as my_cohort_ids,
    coalesce(to_json((select array_agg(c.id) from couple_ids c)), '[]'::json) as my_couple_ids,
    exists (select 1 from tenant_ids t where t.tenant_id = (select current_tenant_id())) as is_member,
    exists (select 1 from tenant_trainers t where t.tenant_id = (select current_tenant_id())) as is_trainer,
    exists (select 1 from tenant_admins a where a.tenant_id = (select current_tenant_id())) as is_admin,
    app_private.is_system_admin(u.id) as is_system_admin;
$$;

CREATE OR REPLACE FUNCTION app_private.relationship_status_next(ts timestamptz, range tstzrange, current public.relationship_status)
  RETURNS public.relationship_status LANGUAGE sql IMMUTABLE
  AS $$
  SELECT CASE
    WHEN ts < lower(range) THEN 'pending'
    WHEN NOT upper_inf(range) AND ts >= upper(range) THEN 'expired'
    WHEN range @> ts THEN 'active'
    ELSE current
  END
$$;

CREATE OR REPLACE FUNCTION app_private.cron_update_memberships() RETURNS void LANGUAGE sql
AS $$
  UPDATE public.user_proxy SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE public.couple SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE public.cohort_membership SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE public.tenant_membership SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE public.tenant_trainer SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE public.tenant_administrator SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);
$$;
