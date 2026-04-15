--! Previous: sha1:a1274c434aa8c82667993491ab95991c961ac7d3
--! Hash: sha1:c4881b00f85b442248ec09d7be2ab03b0cae5c2e

--! split: 1-current.sql
alter table aktuality
  add column if not exists is_visible boolean default true not null;
select app_private.drop_policies('public.aktuality');

CREATE POLICY current_tenant ON public.aktuality AS RESTRICTIVE USING (tenant_id = (SELECT public.current_tenant_id()));
CREATE POLICY admin_all ON public.aktuality TO administrator USING (true);
CREATE POLICY public_view ON public.aktuality FOR SELECT USING (is_visible);


alter table tenant_trainer
  alter column is_visible set not null,
  add column if not exists is_external boolean default false not null;

alter table cohort
  add column if not exists is_archived boolean default false not null;

update cohort set is_archived = not is_visible where is_archived <> not is_visible;
select app_private.drop_policies('public.cohort');

CREATE POLICY current_tenant ON cohort AS RESTRICTIVE USING (tenant_id = (SELECT public.current_tenant_id()));
CREATE POLICY admin_all ON cohort TO administrator USING (true);
CREATE POLICY public_view ON cohort FOR SELECT USING (is_visible);


alter table person
  add column if not exists note text default '' not null;

do $$
begin
  create type federated.competitor_category_progress_input as (
    category_id bigint,
    points numeric(10,3),
    domestic_finale integer,
    foreign_finale integer
  );
exception
  when duplicate_object then null;
end;
$$;

create or replace function federated.replace_competitor_category_progress(
  in_federation    text,
  in_competitor_id bigint,
  in_entries       federated.competitor_category_progress_input[] default '{}'::federated.competitor_category_progress_input[]
)
  returns void
  language plpgsql
  set search_path = federated, pg_temp
as $$
begin
  delete from federated.competitor_category_progress
  where federation = in_federation
    and competitor_id = in_competitor_id;

  insert into federated.competitor_category_progress (
    federation,
    competitor_id,
    category_id,
    points,
    domestic_finale,
    foreign_finale
  )
  select
    in_federation,
    in_competitor_id,
    (entry).category_id,
    coalesce((entry).points, 0),
    coalesce((entry).domestic_finale, 0),
    coalesce((entry).foreign_finale, 0)
  from unnest(coalesce(in_entries, '{}'::federated.competitor_category_progress_input[])) as entry
  where (entry).category_id is not null;
end;
$$;

alter table event_instance
  add column if not exists manager_person_ids bigint[] not null default '{}'::bigint[],
  add column if not exists stats jsonb not null default '{}'::jsonb;

create or replace function app_private.refresh_event_instance_stats(p_instance_id bigint) returns void language sql as $$
  with v as (
    select jsonb_build_object(
      'TOTAL', agg.person_total,
      'UNKNOWN', agg.unknown_count,
      'ATTENDED', agg.attended_count,
      'NOT_EXCUSED', agg.not_excused_count
    ) as stats
    from (
      select
        count(*) filter (where ea.status <> 'cancelled')::int as person_total,
        count(*) filter (where ea.status = 'unknown')::int as unknown_count,
        count(*) filter (where ea.status = 'attended')::int as attended_count,
        count(*) filter (where ea.status = 'not-excused')::int as not_excused_count
      from public.event_attendance ea
      where ea.instance_id = p_instance_id
    ) agg
  )
  update public.event_instance ei set stats = v.stats from v
  where ei.id = p_instance_id and ei.stats is distinct from v.stats;
$$;

create or replace function app_private.tg_event_attendance__refresh_stats()
  returns trigger
  language plpgsql
as $$
begin
  if tg_op in ('UPDATE', 'DELETE') then
    perform app_private.refresh_event_instance_stats(old.instance_id);
  end if;
  if tg_op in ('INSERT', 'UPDATE') then
    if tg_op = 'INSERT'
      or new.instance_id is distinct from old.instance_id
      or new.status is distinct from old.status
    then
      perform app_private.refresh_event_instance_stats(new.instance_id);
    end if;
  end if;
  return null;
end;
$$;

select app_private.refresh_event_instance_stats(ei.id) from public.event_instance ei;

drop trigger if exists _500_recalc_instance_attendance_summary on event_attendance;
create trigger _500_recalc_instance_attendance_summary
  after insert or delete or update of instance_id, status
  on public.event_attendance
  for each row
  execute function app_private.tg_event_attendance__refresh_stats();

create or replace function app_private.event_instance_manager_person_ids(p_instance_id bigint, p_event_id bigint)
  returns bigint[] language sql stable parallel safe
as $$
  select coalesce(array_agg(s.person_id order by s.person_id), '{}'::bigint[])
  from (
    select eit.person_id
    from public.event_instance_trainer eit
    where eit.instance_id = p_instance_id
    union
    select et.person_id
    from public.event_trainer et
    where et.event_id = p_event_id
  ) s;
$$;

create or replace function app_private.refresh_event_instance_manager_person_ids(p_instance_id bigint, p_event_id bigint)
    returns boolean language sql volatile as $$
  with v as (
    select app_private.event_instance_manager_person_ids(p_instance_id, p_event_id) as ids
  ),
  u as (
    update public.event_instance ei set manager_person_ids = v.ids from v
    where ei.id = p_instance_id and ei.event_id = p_event_id and ei.manager_person_ids is distinct from v.ids
    returning 1
  )
  select exists(select 1 from u);
$$;

create or replace function app_private.tg_event_instance_trainer__refresh_manager_person_ids()
    returns trigger language plpgsql as $$
begin
  if tg_op = 'DELETE' then
    perform app_private.refresh_event_instance_manager_person_ids(old.instance_id, old.event_id);
    return null;
  elsif tg_op = 'INSERT' then
    perform app_private.refresh_event_instance_manager_person_ids(new.instance_id, new.event_id);
    return null;
  elsif old.instance_id is distinct from new.instance_id
     or old.event_id is distinct from new.event_id
     or old.person_id is distinct from new.person_id then
    perform app_private.refresh_event_instance_manager_person_ids(old.instance_id, old.event_id);
    perform app_private.refresh_event_instance_manager_person_ids(new.instance_id, new.event_id);
  end if;
  return null;
end;
$$;

drop trigger if exists _500_refresh_manager_person_ids on public.event_instance_trainer;
create trigger _500_refresh_manager_person_ids
  after insert or delete or update of instance_id, event_id, person_id
  on public.event_instance_trainer
  for each row
  execute function app_private.tg_event_instance_trainer__refresh_manager_person_ids();

create or replace function app_private.tg_event_trainer__refresh_manager_person_ids()
returns trigger
language plpgsql
set search_path = pg_catalog, public
as $$
begin
  if tg_op in ('DELETE', 'UPDATE') then
    if tg_op = 'DELETE' or old.event_id is distinct from new.event_id or old.person_id is distinct from new.person_id then
      perform app_private.refresh_event_instance_manager_person_ids(ei.id, ei.event_id)
      from public.event_instance ei
      where ei.event_id = old.event_id;
    end if;
  end if;
  if tg_op in ('INSERT', 'UPDATE') then
    if tg_op = 'INSERT' or old.event_id is distinct from new.event_id or old.person_id is distinct from new.person_id then
      perform app_private.refresh_event_instance_manager_person_ids(ei.id, ei.event_id)
      from public.event_instance ei
      where ei.event_id = new.event_id;
    end if;
  end if;
  return null;
end;
$$;

drop trigger if exists _500_refresh_manager_person_ids on public.event_trainer;

create trigger _500_refresh_manager_person_ids
  after insert or delete or update of event_id, person_id
  on public.event_trainer
  for each row
  execute function app_private.tg_event_trainer__refresh_manager_person_ids();
