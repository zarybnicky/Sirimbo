--! Previous: sha1:183a1a8b45e51b743af638f0c23beed8e4740a92
--! Hash: sha1:ab3ea254ab9f11ea7a156bff384659ab27db556f

--! split: 1-current.sql
alter table file add column if not exists url text
  generated always as ('/f/' || id || '/' || name) stored not null;

create or replace function app_private.tg__timestamps() returns trigger language plpgsql as $$
declare
  ignored text[];
begin
  if tg_op = 'UPDATE' then
    -- Don't bump updated_at for no-op updates
    if new is not distinct from old then
      return new;
    end if;

    select coalesce(array_agg(attribute.attname::text order by attribute.attnum), '{}') || tg_argv
      into ignored
    from pg_catalog.pg_attribute as attribute
    where attribute.attrelid = tg_relid
      and attribute.attnum > 0
      and not attribute.attisdropped
      and attribute.attgenerated <> '';

    -- Ignore generated columns and any independent state named by the trigger
    if ignored <> '{}' and to_jsonb(new) - ignored is not distinct from to_jsonb(old) - ignored then
      return new;
    end if;
  end if;

  new.created_at = case when tg_op = 'INSERT' then now() else old.created_at end;
  new.updated_at = now();
  return new;
end;
$$;

create or replace trigger _100_timestamps
  before insert or update on event_instance_registration
  for each row execute function app_private.tg__timestamps(
    'attendance_created_at', 'attendance_updated_at'
  );

create or replace trigger _100_timestamps
  before insert or update on event_instance
  for each row execute function app_private.tg__timestamps('manager_person_ids', 'stats');

create or replace trigger _100_timestamps
  before insert or update on users
  for each row execute function app_private.tg__timestamps(
    'last_login', 'last_active_at', 'last_version'
  );

create or replace trigger _100_timestamps
  before insert or update on event_instance_trainer
  for each row execute function app_private.tg__timestamps();

create or replace trigger _100_timestamps
  before insert or update on event_lesson_demand
  for each row execute function app_private.tg__timestamps();

create or replace trigger _100_timestamps
  before insert or update on otp_token
  for each row execute function app_private.tg__timestamps();

create or replace trigger _100_timestamps
  before insert or update on person_invitation
  for each row execute function app_private.tg__timestamps();

create or replace trigger _100_timestamps
  before insert or update on tenant_location
  for each row execute function app_private.tg__timestamps();

--! Included functions/competition_reports.sql
create or replace function competition_brief(
  p_since date default (date_trunc('week', now())::date + 4),
  p_until date default (date_trunc('week', now())::date + 7),
  p_cohort_id bigint default null,
  p_person_ids bigint[] default null
) returns setof competition_participation_record
  language sql stable
as $$
  with scoped_people as (
    select distinct p.id, p.name, p.csts_id, p.wdsf_id
    from current_tenant_membership tm
    join person p on p.id = tm.person_id
    where (p_person_ids is null or p.id = any(p_person_ids))
      and (p_cohort_id is null
       or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
  ),
  federated_people as (
    select
      sp.id as person_id,
      sp.name as person_name,
      fp.id as federated_person_id,
      fp.federation
    from scoped_people sp
    cross join lateral (
      values
        ('csts'::text, sp.csts_id::bigint),
        ('wdsf'::text, sp.wdsf_id::bigint)
    ) ids(federation, external_id)
    join federated.person fp
      on fp.federation = ids.federation
     and fp.external_id = ids.external_id
     and ids.external_id <> 0
  )
  select
    fp.person_id,
    fp.person_name,
    fp.federation,
    fp.federated_person_id,
    c.id as competitor_id,
    c.name as competitor_name,
    c.competitor_type,
    e.id as event_id,
    e.name as event_name,
    coalesce(e.location, e.city) as event_location,
    comp.id as competition_id,
    comp.start_date as competition_date,
    comp.check_in_end,
    cat as category,
    dances.dances,
    comp.participants_total as participants,
    null::integer as ranking,
    null::integer as ranking_to,
    null::numeric(10,3) as point_gain,
    null::boolean as is_final,
    false as has_result,
    comp.competition_type,
    e.external_id as event_external_id,
    comp.external_id as competition_external_id
  from federated.competition comp
  join federated.event e on e.id = comp.event_id
  join federated.category cat on cat.id = comp.category_id
  join federated.competition_entry ce
    on ce.competition_id = comp.id
   and not ce.cancelled
  join federated.competitor c on c.id = ce.competitor_id
  join federated.competitor_component cc on cc.competitor_id = c.id
  join federated_people fp on fp.federated_person_id = cc.person_id
  left join lateral (
    select coalesce(array_agg(d.name order by dpd.dance_order), '{}'::text[]) as dances
    from federated.dance_program_dance dpd
    join federated.dance d on d.code = dpd.dance_code
    where dpd.program_id = cat.base_dance_program_id
  ) dances on true
  where comp.start_date >= p_since
    and comp.start_date < p_until
  order by
    comp.start_date,
    comp.check_in_end nulls last,
    fp.person_name,
    cat.discipline,
    cat.class,
    c.name;
$$;

create or replace function competition_report(
  p_since date default (date_trunc('week', now())::date - 2),
  p_until date default date_trunc('week', now())::date,
  p_cohort_id bigint default null,
  p_person_ids bigint[] default null
) returns setof competition_participation_record
  language sql stable
as $$
  with scoped_competitions as (
    select *
    from federated.competition
    where start_date >= p_since
      and start_date < p_until
  ),
  latest_rounds as (
    select distinct on (r.competition_id)
      r.id,
      r.competition_id
    from federated.competition_round r
    join scoped_competitions comp on comp.id = r.competition_id
    order by r.competition_id, r.round_index desc, (r.round_key = 'F') desc, r.id desc
  ),
  competition_dances as (
    select
      r.competition_id,
      array_agg(d.name order by rd.dance_order) as dances
    from latest_rounds r
    join federated.round_dance rd on rd.round_id = r.id
    join federated.dance d on d.code = rd.dance_code
    group by r.competition_id
  ),
  scoped_people as (
    select distinct p.id, p.name, p.csts_id, p.wdsf_id
    from current_tenant_membership tm
    join person p on p.id = tm.person_id
    where (p_person_ids is null or p.id = any(p_person_ids))
      and (p_cohort_id is null
       or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
  ),
  federated_people as (
    select
      sp.id as person_id,
      sp.name as person_name,
      fp.id as federated_person_id,
      fp.federation
    from scoped_people sp
    cross join lateral (
      values
        ('csts'::text, sp.csts_id::bigint),
        ('wdsf'::text, sp.wdsf_id::bigint)
    ) ids(federation, external_id)
    join federated.person fp
      on fp.federation = ids.federation
     and fp.external_id = ids.external_id
     and ids.external_id <> 0
  )
  select
    fp.person_id,
    fp.person_name,
    fp.federation,
    fp.federated_person_id,
    c.id as competitor_id,
    c.name as competitor_name,
    c.competitor_type,
    e.id as event_id,
    e.name as event_name,
    coalesce(e.location, e.city) as event_location,
    comp.id as competition_id,
    comp.start_date as competition_date,
    comp.check_in_end,
    cat as category,
    coalesce(dances.dances, '{}'::text[]) as dances,
    comp.participants_total as participants,
    cr.ranking,
    cr.ranking_to,
    cr.point_gain,
    cr.is_final,
    true as has_result,
    comp.competition_type,
    e.external_id as event_external_id,
    comp.external_id as competition_external_id
  from scoped_competitions comp
  join federated.event e on e.id = comp.event_id
  join federated.category cat on cat.id = comp.category_id
  join federated.competition_result cr on cr.competition_id = comp.id
  join federated.competitor c on c.id = cr.competitor_id
  join federated.competitor_component cc on cc.competitor_id = c.id
  join federated_people fp on fp.federated_person_id = cc.person_id
  left join competition_dances dances on dances.competition_id = comp.id
  order by
    comp.start_date,
    fp.person_name,
    cat.discipline,
    cat.class,
    cr.ranking,
    c.name;
$$;

comment on function competition_brief is '@omit';
grant all on function competition_brief to anonymous;

comment on function competition_report is '@omit';
grant all on function competition_report to anonymous;
--! EndIncluded functions/competition_reports.sql

comment on table aktuality is '@omit create,update
@behavior -query:resource:list';

alter table crawler.frontier alter column next_fetch_at set default now();

update crawler.frontier set next_fetch_at = discovered_at where next_fetch_at is null;

alter table crawler.frontier alter column next_fetch_at set not null;

drop function if exists crawler.frontier_fetch_due(boolean);

create index if not exists json_response_frontier_fetched_desc_idx
  on crawler.json_response (frontier_id, fetched_at desc, id desc);

create index if not exists frontier_failure_idx
  on crawler.frontier (federation, kind, id)
  where fetch_status in ('error', 'transient')
     or process_status = 'error';
