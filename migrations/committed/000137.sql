--! Previous: sha1:372d6fb366be7371586b0fba342bdb88d5216445
--! Hash: sha1:f78a407474f97e92ab0059048346e2095f7a0c49

--! split: 1-current.sql
drop function if exists person_is_admin(person);
drop function if exists person_is_trainer(person);
drop function if exists person_is_member(person);
drop function if exists person_cohort_ids(person);
drop function if exists payment_debtor_is_unpaid(payment_debtor);
drop function if exists account_balance(account);
drop function if exists tenant_membership_tenant_name(tenant_membership);
drop function if exists tenant_trainer_tenant_name(tenant_trainer);
drop function if exists tenant_administrator_tenant_name(tenant_administrator);
drop function if exists access_credential_is_allowed(access_credential);

alter table crawler.frontier add column if not exists reference_date date;

create table if not exists crawler.refetch_policy (
  federation text not null,
  kind text not null,
  reference_offset interval,
  fetch_interval interval not null check (fetch_interval > interval '0 seconds'),
  unique nulls not distinct (federation, kind, reference_offset)
);

comment on table crawler.refetch_policy is '@behavior -*';
grant select on crawler.refetch_policy to system_admin;

alter table crawler.refetch_policy enable row level security;
drop policy if exists system_admin_read on crawler.refetch_policy;
create policy system_admin_read on crawler.refetch_policy for select to system_admin using (true);

--! Included data/crawler_refetch_policy.sql
delete from crawler.refetch_policy;

insert into crawler.refetch_policy (federation, kind, reference_offset, fetch_interval)
values
  ('wdsf', 'ageGroups', null, '30 days'),
  ('wdsf', 'memberIndex', null, '2 days'),
  ('wdsf', 'member', null, '5 days'),
  ('wdsf', 'couple', null, '7 days'),
  ('wdsf', 'competitionIndex', null, '1 month'),
  ('wdsf', 'competitionIndex', '-1 month', '12 hours'),
  ('wdsf', 'competitionIndex', '-1 week', '12 hours'),
  ('wdsf', 'competitionIndex', '1 month', '1 month'),
  ('wdsf', 'competitionIndex', '1 year', '1 year'),
  ('wdsf', 'competition', null, '1 month'),
  ('wdsf', 'competition', '-1 month', '1 week'),
  ('wdsf', 'competition', '-1 week', '1 hour'),
  ('wdsf', 'competition', '-1 day', '15 minutes'),
  ('wdsf', 'competition', '2 days', '1 day'),
  ('wdsf', 'competition', '1 month', '1 month'),
  ('wdsf', 'competition', '1 year', '1 year'),
  ('wdsf', 'participantIndex', null, '1 month'),
  ('wdsf', 'participantIndex', '-1 month', '1 week'),
  ('wdsf', 'participantIndex', '-1 week', '1 hour'),
  ('wdsf', 'participantIndex', '-1 day', '15 minutes'),
  ('wdsf', 'participantIndex', '2 days', '1 day'),
  ('wdsf', 'participantIndex', '1 month', '1 month'),
  ('wdsf', 'participantIndex', '1 year', '1 year'),
  ('wdsf', 'participant', null, '1 month'),
  ('wdsf', 'participant', '-1 month', '1 week'),
  ('wdsf', 'participant', '-1 week', '1 hour'),
  ('wdsf', 'participant', '-1 day', '15 minutes'),
  ('wdsf', 'participant', '2 days', '1 day'),
  ('wdsf', 'participant', '1 month', '1 month'),
  ('wdsf', 'participant', '1 year', '1 year'),
  ('wdsf', 'officialIndex', null, '1 month'),
  ('wdsf', 'officialIndex', '-1 month', '1 week'),
  ('wdsf', 'officialIndex', '-1 week', '6 hours'),
  ('wdsf', 'officialIndex', '2 days', '1 day'),
  ('wdsf', 'officialIndex', '1 month', '1 month'),
  ('wdsf', 'officialIndex', '1 year', '1 year'),
  ('csts', 'member', null, '3 days'),
  ('csts', 'memberProfile', null, '7 days'),
  ('csts', 'partnershipRegistrations', null, '3 days'),
  ('csts', 'ranklistIndex', null, '1 day'),
  ('csts', 'ranklist', null, '1 month'),
  ('csts', 'ranklist', '-1 month', '1 week'),
  ('csts', 'ranklist', '-1 week', '1 day'),
  ('csts', 'ranklist', '1 month', '1 month'),
  ('csts', 'ranklist', '1 year', '1 year'),
  ('csts', 'eventIndex', null, '1 month'),
  ('csts', 'eventIndex', '-1 month', '1 day'),
  ('csts', 'eventIndex', '-1 week', '1 day'),
  ('csts', 'eventIndex', '1 month', '1 month'),
  ('csts', 'eventIndex', '1 year', '1 year'),
  ('csts', 'competitionEventIndex', null, '1 month'),
  ('csts', 'competitionEventIndex', '-1 month', '15 minutes'),
  ('csts', 'competitionEventIndex', '-1 week', '15 minutes'),
  ('csts', 'competitionEventIndex', '1 month', '1 month'),
  ('csts', 'competitionEventIndex', '1 year', '1 year'),
  ('csts', 'event', null, '1 month'),
  ('csts', 'event', '-1 month', '1 week'),
  ('csts', 'event', '-1 week', '1 day'),
  ('csts', 'event', '-2 days', '15 minutes'),
  ('csts', 'event', '2 days', '1 day'),
  ('csts', 'event', '1 month', '1 month'),
  ('csts', 'event', '1 year', '1 year'),
  ('csts', 'eventCompetitors', null, '1 month'),
  ('csts', 'eventCompetitors', '-1 month', '1 week'),
  ('csts', 'eventCompetitors', '-1 week', '1 day'),
  ('csts', 'eventCompetitors', '-2 days', '15 minutes'),
  ('csts', 'eventCompetitors', '2 days', '1 day'),
  ('csts', 'eventCompetitors', '1 month', '1 month'),
  ('csts', 'eventCompetitors', '1 year', '1 year'),
  ('csts', 'competitionResults', null, '1 month'),
  ('csts', 'competitionResults', '-1 month', '1 week'),
  ('csts', 'competitionResults', '-1 week', '1 day'),
  ('csts', 'competitionResults', '-2 days', '15 minutes'),
  ('csts', 'competitionResults', '2 days', '1 day'),
  ('csts', 'competitionResults', '1 month', '1 month'),
  ('csts', 'competitionResults', '1 year', '1 year'),
  ('csts', 'clubIndex', null, '1 day'),
  ('csts', 'divisionIndex', null, '1 day'),
  ('csts', 'trainerIndex', null, '1 day'),
  ('csts', 'judgeIndex', null, '1 day'),
  ('csts', 'officialIndex', null, '1 day'),
  ('szts', 'competitionIndex', null, '1 day'),
  ('szts', 'memberIndex', null, '1 day'),
  ('szts', 'trainerIndex', null, '1 day'),
  ('szts', 'officialIndex', null, '1 day'),
  ('szts', 'scrutineerIndex', null, '1 day'),
  ('szts', 'judgeIndex', null, '1 day'),
  ('szts', 'clubIndex', null, '1 day'),
  ('szts', 'soloIndex', null, '1 day'),
  ('szts', 'coupleIndex', null, '1 day')
on conflict (federation, kind, reference_offset) do update
set fetch_interval = excluded.fetch_interval
where crawler.refetch_policy.fetch_interval is distinct from excluded.fetch_interval;
--! EndIncluded data/crawler_refetch_policy.sql

--! Included functions/crawler_refetch.sql
-- Evaluate both interval arithmetic and date boundaries in UTC.
create or replace function crawler.next_fetch_at(
  p_federation text, p_kind text, p_reference_date date, p_fetched_at timestamptz
) returns timestamptz language plpgsql stable set timezone = 'UTC' as $$
declare
  fetch_interval interval;
  next_boundary timestamptz;
begin
  select p.fetch_interval into fetch_interval
  from crawler.refetch_policy p
  where p.federation = p_federation and p.kind = p_kind
    and (p.reference_offset is null or p_reference_date + p.reference_offset <= p_fetched_at)
  order by (p_reference_date + p.reference_offset) desc nulls last
  limit 1;

  select min((p_reference_date + p.reference_offset)::timestamptz) into next_boundary
  from crawler.refetch_policy p
  where p.federation = p_federation and p.kind = p_kind
    and p_reference_date + p.reference_offset > p_fetched_at;

  return least(p_fetched_at + fetch_interval, next_boundary);
end;
$$;

create or replace function crawler.tg_frontier_reference_date() returns trigger
language plpgsql as $$
begin
  if new.reference_date is distinct from old.reference_date and new.fetch_status = 'ok' then
    new.next_fetch_at := crawler.next_fetch_at(new.federation, new.kind, new.reference_date, new.last_fetched_at);
  end if;
  return new;
end;
$$;

create or replace trigger frontier_reference_date
  before update of reference_date on crawler.frontier
  for each row execute function crawler.tg_frontier_reference_date();

create or replace function crawler.recalculate_fetch_times(p_federation text, p_kind text)
returns integer language plpgsql as $$
declare
  updated integer;
begin
  with times as materialized (
    select f.id, crawler.next_fetch_at(f.federation, f.kind, f.reference_date, f.last_fetched_at) as next_fetch_at
    from crawler.frontier f
    where f.fetch_status = 'ok' and f.last_fetched_at is not null
      and f.federation = p_federation and f.kind = p_kind
  )
  update crawler.frontier f set next_fetch_at = t.next_fetch_at
  from times t where f.id = t.id and f.next_fetch_at is distinct from t.next_fetch_at;
  get diagnostics updated = row_count;
  return updated;
end;
$$;

comment on function crawler.next_fetch_at is '@behavior -*';
comment on function crawler.recalculate_fetch_times is '@behavior -*';
--! EndIncluded functions/crawler_refetch.sql

update crawler.frontier f set reference_date = coalesce(c.end_date, c.start_date)
from federated.competition c
where f.reference_date is null and c.federation = f.federation and c.external_id = f.key
  and ((f.federation = 'wdsf' and f.kind in ('competition', 'participantIndex', 'officialIndex'))
    or (f.federation = 'csts' and f.kind = 'competitionResults'));

update crawler.frontier f set reference_date = coalesce(e.end_date, e.start_date)
from federated.event e
where f.reference_date is null and e.federation = f.federation and e.external_id = f.key
  and f.federation = 'csts' and f.kind in ('event', 'eventCompetitors');

update crawler.frontier set reference_date = (key || '-01')::date + interval '1 month -1 day'
where reference_date is null and key ~ '^\d{4}-\d{2}$'
  and ((federation = 'wdsf' and kind = 'competitionIndex')
    or (federation = 'csts' and kind in ('eventIndex', 'competitionEventIndex')));

with participants as (
  select p->>'id' as key, f.reference_date
  from crawler.frontier f
  join crawler.json_response r on r.id = f.last_successful_response_id
  join crawler.json_response_cache rc on rc.content_hash = r.content_hash
  cross join lateral jsonb_array_elements(rc.content) p
  where f.federation = 'wdsf' and f.kind = 'participantIndex' and f.reference_date is not null
)
update crawler.frontier f set reference_date = p.reference_date
from participants p
where f.federation = 'wdsf' and f.kind = 'participant' and f.key = p.key and f.reference_date is null;

update crawler.frontier f set reference_date = (rc.content->'entity'->>'date')::date
from crawler.json_response r join crawler.json_response_cache rc on rc.content_hash = r.content_hash
where f.federation = 'csts' and f.kind = 'ranklist' and f.reference_date is null
  and r.id = f.last_successful_response_id;

alter type federated.person_license_status add value if not exists 'temporary';
alter type federated.person_license_discipline add value if not exists 'esports';

drop function if exists event_trainer_tenant_trainer;
drop function if exists account_assets;
drop function if exists account_liabilities;
drop function if exists event_trainers;
drop function if exists competition_brief;
drop function if exists competition_report;
drop function if exists system_admin_tenants;

do $$
begin
  if not exists (
    select 1 from pg_attribute
    where attrelid = 'public.event'::regclass
      and attname = 'registration_period' and not attisdropped
  ) then
    alter table event add column registration_period tstzrange default '(,)'::tstzrange;
    update event set registration_period = null where is_locked;
  end if;
end;
$$;

comment on function app_private.tg_aktuality__sync_attachments is null;
comment on column event.registration_period is null;
comment on column event.is_locked is '@behavior -*';

--! Included functions/save_events.sql
drop function save_events;
drop type event_details_input;
drop type event_input;
drop type event_registration_input;

create type event_registration_input as (
  person_id bigint,
  couple_id bigint,
  target_cohort_id bigint,
  is_cancelled boolean
);

create type event_input as (
  id bigint,
  since timestamptz,
  until timestamptz,
  is_cancelled boolean,
  registrations event_registration_input[]
);

create type event_details_input as (
  parent_id bigint,
  name text,
  type event_type,
  location_id bigint,
  location_text text,
  capacity integer,
  capacity_unit event_capacity_unit,
  is_visible boolean,
  is_public boolean,
  has_public_details boolean,
  is_locked boolean,
  enable_notes boolean,
  registration_period tstzrange
);

create or replace function save_events(
  details event_details_input,
  events event_input[],
  trainers event_trainer_input[] default '{}'::event_trainer_input[],
  cohort_ids bigint[] default '{}'::bigint[],
  series event_series_input default null
) returns setof event
  language plpgsql
as $$
declare
  event_to_save event_input;
  v_saved_event event;
  v_saved_event_ids bigint[] := '{}'::bigint[];
  v_tenant_id bigint := current_tenant_id();
  v_series_id bigint;
  v_assign_series boolean := (series).id is not null or (series).name is not null;
  v_is_visible boolean := coalesce((details).is_visible, true);
  v_is_public boolean := coalesce((details).is_public, false);
  v_has_public_details boolean := v_is_public and coalesce((details).has_public_details, false);
  v_registration_period tstzrange := case
    when (details).is_locked is null then (details).registration_period
    when (details).is_locked then null
    else '(,)'::tstzrange
  end;
  v_enable_notes boolean := coalesce((details).enable_notes, false);
  v_expected_existing_count bigint;
  v_locked_existing_count bigint;
begin
  if details is null then
    raise exception 'event details are required';
  end if;

  if (details).type is null
    or (details).capacity is null
    or (details).capacity < 0
    or (details).capacity_unit is null then
    raise exception 'event details are incomplete';
  end if;

  if cardinality(coalesce(events, '{}'::event_input[])) = 0 then
    raise exception 'at least one event is required';
  end if;

  if exists (
    select 1 from unnest(events) i
    where i is null or i.since is null or i.until is null or i.until <= i.since
  ) then
    raise exception 'every event requires a valid time range';
  end if;

  if exists (select i.id from unnest(events) i where i.id is not null group by i.id having count(*) > 1) then
    raise exception 'an event may only be submitted once';
  end if;

  if exists (
    select 1
    from unnest(events) i
    cross join lateral unnest(coalesce(i.registrations, '{}'::event_registration_input[])) registration
    where (registration.person_id is null) = (registration.couple_id is null)
  ) then
    raise exception 'an event registration requires exactly one person or couple';
  end if;

  if exists (
    select 1
    from unnest(coalesce(trainers, '{}'::event_trainer_input[])) trainer
    where trainer.person_id is null or trainer.lessons_offered < 0
  ) then
    raise exception 'an event trainer requires a person and a non-negative lesson limit';
  end if;

  if exists (
    select 1 from unnest(events) i
    cross join lateral unnest(i.registrations) registration
    where registration.target_cohort_id is not null and (
      registration.person_id is null or registration.is_cancelled
      or not registration.target_cohort_id = any(coalesce(cohort_ids, '{}'))
    )
  ) then
    raise exception 'a cohort registration requires an active person and a selected cohort' using errcode = '22023';
  end if;

  if exists (
    select 1
    from unnest(coalesce(cohort_ids, '{}'::bigint[])) i(cohort_id)
    left join cohort cohort on cohort.id = i.cohort_id and cohort.tenant_id = v_tenant_id
    where i.cohort_id is not null and cohort.id is null
  ) then
    raise exception 'event cohort not found';
  end if;

  if exists (
    select 1 from unnest(events) i where i.id is null
  ) and (details).parent_id is not null and not exists (
    select 1
    from event parent
    where parent.id = (details).parent_id and parent.tenant_id = v_tenant_id
  ) then
    raise exception 'event parent % not found or not editable', (details).parent_id;
  end if;

  if v_assign_series then
    if (series).id is null then
      insert into event_series (name)
      values (coalesce((series).name, (details).name))
      returning id into v_series_id;
    else
      select e.id into v_series_id
      from event_series e where e.id = (series).id and e.tenant_id = v_tenant_id
      for update;

      if not found then
        raise exception 'event series % not found or not editable', (series).id;
      end if;
    end if;
  end if;

  select count(*) into v_expected_existing_count from unnest(events) i where i.id is not null;

  perform e.id
  from event e
  join unnest(events) i on i.id = e.id
  where i.id is not null and e.tenant_id = v_tenant_id
  order by e.id
  for update of e;

  get diagnostics v_locked_existing_count = row_count;
  if v_locked_existing_count <> v_expected_existing_count then
    raise exception 'one or more events were not found or are not editable';
  end if;

  foreach event_to_save in array events loop
    if event_to_save.id is null then
      insert into event (
        parent_id,
        series_id,
        since,
        until,
        is_cancelled,
        name,
        type,
        location_id,
        location_text,
        capacity,
        capacity_unit,
        is_visible,
        is_public,
        has_public_details,
        registration_period,
        enable_notes,
        description,
        summary,
        files_legacy
      ) values (
        (details).parent_id,
        v_series_id,
        event_to_save.since,
        event_to_save.until,
        coalesce(event_to_save.is_cancelled, false),
        (details).name,
        (details).type,
        (details).location_id,
        coalesce((details).location_text, ''),
        (details).capacity,
        (details).capacity_unit,
        v_is_visible,
        v_is_public,
        v_has_public_details,
        v_registration_period,
        v_enable_notes,
        '',
        '',
        ''
      )
      returning * into v_saved_event;
    else
      update event e
      set since = event_to_save.since,
          until = event_to_save.until,
          is_cancelled = coalesce(event_to_save.is_cancelled, false),
          name = (details).name,
          type = (details).type,
          location_id = (details).location_id,
          location_text = coalesce((details).location_text, ''),
          capacity = (details).capacity,
          capacity_unit = (details).capacity_unit,
          is_visible = v_is_visible,
          is_public = v_is_public,
          has_public_details = v_has_public_details,
          registration_period = v_registration_period,
          enable_notes = v_enable_notes,
          series_id = case
            when v_assign_series then v_series_id
            else e.series_id
          end
      where e.id = event_to_save.id and e.tenant_id = v_tenant_id
      returning * into v_saved_event;

      if not found then
        raise exception 'event % not found or not editable', event_to_save.id;
      end if;
    end if;

    v_saved_event_ids := array_append(v_saved_event_ids, v_saved_event.id);

    perform registration.id
    from event_registration registration
    where registration.event_id = v_saved_event.id
    order by registration.id
    for update;

    -- Release person slots before replacing couples with individuals or vice versa.
    with removed as (
      select registration.id
      from event_registration registration
      where registration.event_id = v_saved_event.id
        and registration.parent_registration_id is null
        and not exists (
          select 1 from unnest(event_to_save.registrations) desired
          where desired.person_id is not distinct from registration.person_id
            and desired.couple_id is not distinct from registration.couple_id
            and not coalesce(desired.is_cancelled, false)
        )
    )
    update event_registration registration
    set registration_status = 'cancelled',
        source = case when registration.parent_registration_id is null and registration.source is distinct from 'cohort'
          then 'manager'::event_registration_source else registration.source end
    from removed
    where registration.registration_status = 'active'
      and (registration.id = removed.id or registration.parent_registration_id = removed.id);

    -- Detach person rows from cancelled registrations when submitted as individuals.
    update event_registration registration
    set parent_registration_id = null
    where registration.id in (
      select distinct on (r.person_id) r.id
      from event_registration r
      join event_registration parent on parent.id = r.parent_registration_id
      where r.event_id = v_saved_event.id and parent.registration_status = 'cancelled'
        and exists (select 1 from unnest(event_to_save.registrations) desired where desired.person_id = r.person_id)
        and not exists (select 1 from event_registration standalone
          where standalone.event_id = r.event_id and standalone.person_id = r.person_id
            and standalone.parent_registration_id is null)
      order by r.person_id, r.id desc
    );

    -- Save the displayed list and explicit removals; reuse rows to preserve attendance.
    insert into event_registration as registration (
      event_id, person_id, couple_id, target_cohort_id, source, registration_status, status
    )
    select v_saved_event.id, choice.person_id, choice.couple_id, choice.target_cohort_id,
      case when choice.target_cohort_id is null then 'manager'::event_registration_source else 'cohort'::event_registration_source end,
      case when choice.is_cancelled then 'cancelled'::event_registration_status else 'active'::event_registration_status end,
      case when choice.person_id is not null then 'unknown'::attendance_type end
    from (
      select distinct on (choice.person_id, choice.couple_id) choice.*
      from unnest(event_to_save.registrations) with ordinality
        choice(person_id, couple_id, target_cohort_id, is_cancelled, position)
      order by choice.person_id, choice.couple_id, choice.position
    ) choice
    on conflict (event_id, couple_id, person_id) where parent_registration_id is null
    do update set registration_status = excluded.registration_status,
      source = excluded.source,
      target_cohort_id = excluded.target_cohort_id
    where (registration.registration_status, registration.target_cohort_id)
      is distinct from (excluded.registration_status, excluded.target_cohort_id);

    -- Cancel children of cancelled registrations; reuse person rows for active couple registrations.
    update event_registration child
    set registration_status = parent.registration_status, source = parent.source
    from event_registration parent
    where parent.event_id = v_saved_event.id
      and child.parent_registration_id = parent.id
      and parent.registration_status = 'cancelled'
      and (child.registration_status, child.source) is distinct from (parent.registration_status, parent.source);

    perform app_private.ensure_couple_person_registrations(registration.id)
    from event_registration registration
    where registration.event_id = v_saved_event.id
      and registration.couple_id is not null
      and registration.registration_status = 'active';
    insert into event_target_cohort (tenant_id, event_id, cohort_id)
    select v_tenant_id, v_saved_event.id, id
    from unnest(cohort_ids) cohort(id)
    where id is not null
    on conflict (event_id, cohort_id) do nothing;

    delete from event_target_cohort target
    where target.event_id = v_saved_event.id
      and not exists (select 1 from unnest(cohort_ids) cohort(id) where id = target.cohort_id);

  end loop;

  -- Keep the caller's trainer assignment until all edits and replacements are saved.
  with desired as (
    select distinct on (trainer.person_id) trainer.person_id, trainer.lessons_offered
    from unnest(coalesce(trainers, '{}'::event_trainer_input[]))
      with ordinality trainer(person_id, lessons_offered, position)
    order by trainer.person_id, trainer.position
  )
  insert into event_trainer (tenant_id, event_id, person_id, lessons_offered)
  select stored_event.tenant_id, stored_event.id, desired.person_id, desired.lessons_offered
  from event stored_event
  join unnest(v_saved_event_ids) saved(id) on saved.id = stored_event.id
  cross join desired
  on conflict (event_id, person_id) do update
  set lessons_offered = excluded.lessons_offered;

  delete from event_trainer e
  where e.event_id = any(v_saved_event_ids)
    and not exists (
      select 1
      from unnest(coalesce(trainers, '{}'::event_trainer_input[])) trainer
      where trainer.person_id = e.person_id
    );

  return query
  select stored_event.*
  from unnest(v_saved_event_ids) with ordinality saved(event_id, position)
  join event stored_event on stored_event.id = saved.event_id
  order by saved.position;
end;
$$;

comment on function save_events is '@behavior +list -connection';
grant execute on function save_events to anonymous;
--! EndIncluded functions/save_events.sql
--! Included functions/event_series_info.sql
create or replace function app_private.refresh_event_series_info(p_series_ids bigint[])
  returns void
  language plpgsql security definer
  set search_path = pg_catalog, public, pg_temp
as $$
begin
  if coalesce(cardinality(p_series_ids), 0) = 0 then
    return;
  end if;

  perform id from event_series
  where id = any(p_series_ids) order by id for no key update;

  with actual as (
    select member.id,
      row(series.id, series.name,
        row_number() over ordered::integer,
        count(*) over whole_series::integer,
        min(member.since) over whole_series,
        max(member.until) over whole_series)::series_info as info
    from event member
    join event_series series
      on series.tenant_id = member.tenant_id and series.id = member.series_id
    where series.id = any(p_series_ids)
    window whole_series as (partition by member.tenant_id, member.series_id),
      ordered as (whole_series order by member.since, member.id)
  )
  update event member set series_info = actual.info
  from actual
  where member.id = actual.id and member.series_info is distinct from actual.info;
end;
$$;

create or replace function app_private.tg_event__refresh_series_info()
  returns trigger
  language plpgsql security definer
  set search_path = pg_catalog, public, pg_temp
as $$
-- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
declare
  series_ids bigint[];
begin
  if tg_op = 'INSERT' then
    select array_agg(distinct series_id) into series_ids
    from changed_rows where series_id is not null;
  elsif tg_op = 'DELETE' then
    select array_agg(distinct series_id) into series_ids
    from deleted_rows where series_id is not null;
  else
    select array_agg(distinct affected.series_id) into series_ids
    from deleted_rows old_row full join changed_rows new_row using (id)
    cross join lateral (values (old_row.series_id), (new_row.series_id)) affected(series_id)
    where row(old_row.id, old_row.tenant_id, old_row.series_id, old_row.since, old_row.until)
      is distinct from
      row(new_row.id, new_row.tenant_id, new_row.series_id, new_row.since, new_row.until)
      and affected.series_id is not null;
  end if;

  if tg_op <> 'DELETE' then
    if exists (select 1 from changed_rows
      where series_id is null and series_info is distinct from null) then
      update event member set series_info = null
      from changed_rows changed
      where member.id = changed.id and changed.series_id is null
        and member.series_info is distinct from null;
    end if;
  end if;

  perform app_private.refresh_event_series_info(series_ids);
  if tg_op <> 'INSERT' then
    delete from event_series series
    where series.id = any(series_ids)
      and not exists (select 1 from event where series_id = series.id);
  end if;
  return null;
end;
$$;

create or replace function app_private.tg_event_series__refresh_info()
  returns trigger
  language plpgsql security definer
  set search_path = pg_catalog, public, pg_temp
as $$
-- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
declare
  series_ids bigint[];
begin
  select array_agg(changed.id) into series_ids
  from changed_rows changed join deleted_rows previous using (id)
  where changed.name is distinct from previous.name;
  perform app_private.refresh_event_series_info(series_ids);
  return null;
end;
$$;

create or replace trigger _600_refresh_series_info_ins
  after insert on public.event
  referencing new table as changed_rows for each statement
  execute function app_private.tg_event__refresh_series_info();
create or replace trigger _600_refresh_series_info_del
  after delete on public.event
  referencing old table as deleted_rows for each statement
  execute function app_private.tg_event__refresh_series_info();
create or replace trigger _600_refresh_series_info_upd
  after update on public.event
  referencing old table as deleted_rows new table as changed_rows for each statement
  execute function app_private.tg_event__refresh_series_info();
create or replace trigger _600_refresh_series_info_name
  after update on public.event_series
  referencing old table as deleted_rows new table as changed_rows for each statement
  execute function app_private.tg_event_series__refresh_info();
--! EndIncluded functions/event_series_info.sql
--! Included functions/tg_event__refresh_manager_person_ids.sql
drop trigger if exists _500_refresh_manager_person_ids_from_parent on event;
drop function if exists app_private.tg_event_instance__refresh_manager_person_ids;
drop function if exists app_private.refresh_event_instance_manager_person_ids;
drop function if exists app_private.event_instance_manager_person_ids;

create or replace function app_private.event_manager_person_ids(p_event_id bigint)
  returns bigint[] language sql stable parallel safe as $$
  with recursive chain as (
    select id, parent_id
    from event where id = p_event_id
    union all
    select parent.id, parent.parent_id
    from event parent
    join chain child on child.parent_id = parent.id
  ), managers as (
    select distinct person_id from event_trainer
    where event_id in (select id from chain)
  )
  select coalesce(array_agg(person_id order by person_id), '{}'::bigint[])
  from managers;
$$;

drop function if exists app_private.refresh_event_manager_person_ids;

create function app_private.refresh_event_manager_person_ids(p_event_id bigint)
  returns void language sql security definer
  set search_path = pg_catalog, public, pg_temp as $$
  with recursive desired as (
    select id, app_private.event_manager_person_ids(id) as person_ids
    from event where id = p_event_id
    union all
    select child.id, array(
      select distinct person_id
      from unnest(parent.person_ids || array(
        select person_id from event_trainer where event_id = child.id
      )) person_id
      order by person_id
    )
    from event child
    join desired parent on child.parent_id = parent.id
  )
  update event instance
  set manager_person_ids = desired.person_ids
  from desired
  where instance.id = desired.id
    and instance.id = any(array(select id from desired))
    and instance.manager_person_ids is distinct from desired.person_ids
$$;

grant execute on function app_private.refresh_event_manager_person_ids to anonymous;

create or replace function app_private.tg_event__refresh_manager_person_ids()
  returns trigger language plpgsql as $$
begin
  perform app_private.refresh_event_manager_person_ids(new.id);
  return null;
end;
$$;

create trigger _500_refresh_manager_person_ids_from_parent
  after insert or update of parent_id on event
  for each row execute function app_private.tg_event__refresh_manager_person_ids();

drop trigger if exists _500_refresh_manager_person_ids on event_trainer;
drop function if exists app_private.tg_event_instance_trainer__refresh_manager_person_ids();

create or replace function app_private.tg_event_trainer__refresh_manager_person_ids()
  returns trigger language plpgsql as $$
begin
  if tg_op = 'DELETE' then
    perform app_private.refresh_event_manager_person_ids(old.event_id);
  elsif tg_op = 'INSERT' then
    perform app_private.refresh_event_manager_person_ids(new.event_id);
  elsif old.event_id is distinct from new.event_id
    or old.person_id is distinct from new.person_id then
    perform app_private.refresh_event_manager_person_ids(old.event_id);
    perform app_private.refresh_event_manager_person_ids(new.event_id);
  end if;
  return null;
end;
$$;

create trigger _500_refresh_manager_person_ids
  after insert or delete or update of event_id, person_id
  on event_trainer
  for each row execute function app_private.tg_event_trainer__refresh_manager_person_ids();
--! EndIncluded functions/tg_event__refresh_manager_person_ids.sql
--! Included functions/event_registration_info.sql
drop function if exists event_registration_info;
drop type if exists event_registration_info;

create type event_registration_info as (
  registrations integer,
  people integer,
  remaining_capacity integer,
  my boolean,
  can_register boolean,
  registration_open boolean
);

create or replace function event_registration_info(e event)
  returns event_registration_info
  language sql stable security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  with counts as (
    select
      count(*) filter (where parent_registration_id is null)::integer as registrations,
      count(person_id)::integer as people,
      count(*) filter (where person_id = any(current_person_ids())) > 0 as my
    from event_registration
    where event_id = e.id and registration_status = 'active'
  ), capacity as (
    select counts.*, coalesce(e.registration_period @> now(), false) as registration_open, case
      when e.capacity is null or e.capacity <= 0 then null
      else e.capacity
        - case e.capacity_unit
            when 'people' then people
            when 'registrations' then registrations
          end
    end as remaining_capacity
    from counts
  )
  select row(
    registrations, people, remaining_capacity, my,
    coalesce(
          registration_open
      and not e.is_cancelled
      and e.until > now()
      and coalesce(remaining_capacity > 0, true),
      false
    ),
    registration_open
  )::event_registration_info
  from capacity;
$$;

grant all on function event_registration_info to anonymous;
--! EndIncluded functions/event_registration_info.sql
--! Included functions/set_lesson_request.sql
drop function if exists set_lesson_demand;
drop function if exists set_lesson_request;
drop function if exists app_private.set_lesson_demand;
drop function if exists app_private.set_lesson_request;

create function app_private.set_lesson_request(
  registration event_registration,
  trainer_id bigint,
  lesson_count integer
) returns event_lesson_request
language plpgsql strict
as $$
declare
  trainer event_trainer;
  lesson_request event_lesson_request;
  other_lessons bigint;
begin
  select * into trainer
  from event_trainer
  where id = $2 and event_id = registration.event_id and tenant_id = current_tenant_id()
  for update;

  if not found then
    raise exception 'TRAINER_NOT_FOUND' using errcode = '28000';
  end if;

  if $3 = 0 then
    delete from event_lesson_request eld
    where eld.registration_id = registration.id and eld.trainer_id = $2;

    return null;
  end if;

  if trainer.lessons_offered = 0 then
    raise exception 'LESSONS_NOT_OFFERED' using errcode = '28000';
  end if;

  if trainer.lessons_offered is not null then
    select coalesce(sum(eld.lesson_count), 0) into other_lessons
    from event_lesson_request eld
    join event_registration other_registration
      on other_registration.id = eld.registration_id
     and other_registration.registration_status = 'active'
    where eld.trainer_id = $2 and eld.registration_id <> registration.id;

    if $3 > trainer.lessons_offered - other_lessons then
      raise exception 'LESSON_LIMIT_EXCEEDED' using errcode = '22023';
    end if;
  end if;

  insert into event_lesson_request (registration_id, trainer_id, lesson_count)
  values (registration.id, $2, $3)
  on conflict on constraint event_lesson_request_registration_id_trainer_id_key do update set lesson_count = $3
  returning * into lesson_request;

  return lesson_request;
end;
$$;

create function set_lesson_request(
  registration_id bigint,
  trainer_id bigint,
  lesson_count integer
) returns event_lesson_request
language plpgsql strict security definer
set search_path = pg_catalog, public, pg_temp
as $$
declare
  registration event_registration;
  instance event;
  is_manager boolean;
  is_self boolean;
begin
  select * into registration
  from event_registration
  where id = $1;

  if not found then
    raise exception 'REGISTRATION_NOT_FOUND' using errcode = '28000';
  end if;

  select * into instance
  from event
  where id = registration.event_id and tenant_id = current_tenant_id();

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '28000';
  end if;

  is_self := registration.person_id = any(current_person_ids())
    or registration.couple_id = any(current_couple_ids());
  is_manager := pg_has_role(current_setting('role'), 'administrator', 'member')
    or (pg_has_role(current_setting('role'), 'trainer', 'member') and app_private.can_trainer_edit_event(instance.id));

  if not coalesce(is_self, false) and not coalesce(is_manager, false) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  if not coalesce(instance.registration_period @> now(), false) and not is_manager then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;

  return app_private.set_lesson_request(registration, $2, $3);
end;
$$;

grant all on function set_lesson_request to anonymous;
--! EndIncluded functions/set_lesson_request.sql
--! Included functions/set_event_registration.sql
drop function if exists set_event_instance_registration;
drop function if exists set_event_registration;
drop function if exists register_to_event;
drop function if exists app_private.set_event_instance_registration;
drop type if exists event_registration_person_input;

create type event_registration_person_input as (
  first_name text,
  last_name text,
  gender gender_type,
  nationality text,
  email text,
  phone text,
  birth_date date,
  csts_id integer,
  wdsf_id integer
);

create function set_event_registration(
  event_id bigint,
  person_id bigint,
  couple_id bigint,
  is_registered boolean,
  note text default null,
  lesson_trainer_ids bigint[] default null,
  lesson_counts integer[] default null,
  person event_registration_person_input default null
) returns event_registration
language plpgsql security definer
set search_path = pg_catalog, public, pg_temp
as $$
declare
  target_instance event;
  registration event_registration;
  registration_found boolean;
  is_self boolean;
  is_manager boolean;
  registration_source event_registration_source;
  required_capacity integer;
  remaining_capacity integer;
  request record;
begin
  if is_registered is null or num_nonnulls(person_id, couple_id, person) <> 1
    or (person is distinct from null and not is_registered) then
    raise exception 'INVALID_PARTICIPANT' using errcode = '22023';
  end if;

  if num_nonnulls(lesson_trainer_ids, lesson_counts) = 1
    or cardinality(lesson_trainer_ids) <> cardinality(lesson_counts)
    or exists (
      select 1
      from unnest(lesson_trainer_ids, lesson_counts) demand(trainer_id, lesson_count)
      where demand.trainer_id is null
        or demand.lesson_count is null
        or demand.lesson_count < 0
    ) then
    raise exception 'INVALID_LESSON_DEMANDS' using errcode = '22023';
  end if;
  if not is_registered and lesson_trainer_ids is not null then
    raise exception 'INVALID_LESSON_DEMANDS' using errcode = '22023';
  end if;

  select * into target_instance
  from event
  where id = event_id
    and tenant_id = current_tenant_id()
  for update;

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '22023';
  end if;

  is_manager := pg_has_role(current_setting('role'), 'administrator', 'member')
    or (pg_has_role(current_setting('role'), 'trainer', 'member') and app_private.can_trainer_edit_event(event_id));

  if person is distinct from null then
    if nullif(btrim(person.first_name), '') is null
      or nullif(btrim(person.last_name), '') is null
      or person.gender is null
      or nullif(btrim(person.nationality), '') is null
      or person.email is null or person.email !~ '^[^[:space:]@]+@[^[:space:]@]+\.[^[:space:]@]+$'
      or nullif(btrim(person.phone), '') is null
      or person.birth_date > current_date
      or person.csts_id <= 0 or person.wdsf_id <= 0 then
      raise exception 'INVALID_PERSON_DETAILS' using errcode = '22023';
    end if;

    select r.* into registration
    from event_registration r
    join person p on p.id = r.person_id
    where r.event_id = set_event_registration.event_id
      and r.tenant_id = current_tenant_id()
      and r.created_by is not distinct from current_user_id()
      and r.registration_status = 'active'
      and r.parent_registration_id is null
      and r.note is not distinct from set_event_registration.note
      and row(
        p.first_name, p.last_name, p.gender, p.nationality,
        p.email, p.phone, p.birth_date, p.csts_id, p.wdsf_id
      ) is not distinct from row(
        btrim((set_event_registration.person).first_name), btrim((set_event_registration.person).last_name),
        (set_event_registration.person).gender, (set_event_registration.person).nationality,
        btrim((set_event_registration.person).email), btrim((set_event_registration.person).phone),
        (set_event_registration.person).birth_date, (set_event_registration.person).csts_id,
        (set_event_registration.person).wdsf_id
      )
      and coalesce((
        select jsonb_object_agg(d.trainer_id, d.lesson_count)
        from event_lesson_request d where d.registration_id = r.id
      ), '{}'::jsonb) = coalesce((
        select jsonb_object_agg(d.trainer_id, d.lesson_count)
        from unnest(lesson_trainer_ids, lesson_counts) d(trainer_id, lesson_count)
        where d.lesson_count > 0
      ), '{}'::jsonb)
    order by r.id
    limit 1;

    if found then
      return registration;
    end if;

    insert into person (
      first_name, last_name, gender, nationality, email, phone, birth_date, csts_id, wdsf_id
    ) values (
      btrim((set_event_registration.person).first_name), btrim((set_event_registration.person).last_name),
      (set_event_registration.person).gender, (set_event_registration.person).nationality,
      btrim((set_event_registration.person).email), btrim((set_event_registration.person).phone),
      (set_event_registration.person).birth_date, (set_event_registration.person).csts_id,
      (set_event_registration.person).wdsf_id
    )
    returning id into person_id;

    is_self := not is_manager;
  else
    is_self := coalesce(person_id = any(current_person_ids()), false)
      or coalesce(couple_id = any(current_couple_ids()), false);
  end if;

  if not is_self and not is_manager then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;
  if not coalesce(target_instance.registration_period @> now(), false) and not is_manager then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  registration_source := case when is_self
    then 'self'::event_registration_source
    else 'manager'::event_registration_source
  end;

  select * into registration
  from event_registration r
  where r.event_id = set_event_registration.event_id
    and r.person_id is not distinct from set_event_registration.person_id
    and r.couple_id is not distinct from set_event_registration.couple_id
  order by (r.registration_status = 'active') desc, (r.parent_registration_id is null) desc, r.id desc
  limit 1;
  registration_found := found;

  if not is_registered then
    if not registration_found then
      raise exception 'REGISTRATION_NOT_FOUND' using errcode = '22023';
    end if;

    update event_registration
    set registration_status = 'cancelled',
        target_cohort_id = null,
        source = registration_source
    where id = registration.id or parent_registration_id = registration.id;

    select * into registration from event_registration where id = registration.id;
    return registration;
  end if;

  if registration_found and registration.registration_status = 'active' then
    if note is not null then
      update event_registration set note = set_event_registration.note where id = registration.id
      returning * into registration;
    end if;
  else
    if not is_manager and (
      target_instance.is_cancelled
        or target_instance.until <= now()
        or not (
          coalesce(target_instance.is_public, false)
          or (
            coalesce(target_instance.is_visible, false)
            and current_tenant_id() = any(my_tenants_array())
          )
        )
    ) then
      raise exception 'NOT_ALLOWED' using errcode = '28000';
    end if;

    if not is_manager then
      remaining_capacity := (event_registration_info(target_instance)).remaining_capacity;
      required_capacity := case
        when target_instance.capacity_unit = 'people' and couple_id is not null then 2
        when target_instance.capacity_unit = 'registrations' and exists (
          select 1 from event_registration parent
          where parent.id = registration.parent_registration_id and parent.registration_status = 'active'
        ) then 0
        else 1
      end;
      if couple_id is not null then
        required_capacity := required_capacity - (
          select count(*)::integer from event_registration r
          join couple c on c.id = set_event_registration.couple_id and r.person_id in (c.man_id, c.woman_id)
          where r.event_id = set_event_registration.event_id and r.registration_status = 'active'
            and (target_instance.capacity_unit = 'people' or r.parent_registration_id is null)
        );
      end if;
      if remaining_capacity is not null and remaining_capacity < required_capacity then
        raise exception 'CAPACITY_EXCEEDED' using errcode = '22023';
      end if;
    end if;

    if registration_found then
      update event_registration
      set registration_status = 'active',
          target_cohort_id = null,
          source = registration_source,
          parent_registration_id = (
            select parent.id from event_registration parent
            where parent.id = registration.parent_registration_id and parent.registration_status = 'active'
          )
      where id = registration.id;

      if note is not null then
        update event_registration set note = set_event_registration.note where id = registration.id;
      end if;
    else
      insert into event_registration (
        event_id, person_id, couple_id, source, status, note
      ) values (
        event_id, set_event_registration.person_id, set_event_registration.couple_id, registration_source,
        case when set_event_registration.person_id is not null then 'unknown'::attendance_type end,
        set_event_registration.note
      ) returning * into registration;
    end if;
  end if;

  if couple_id is not null then
    perform app_private.ensure_couple_person_registrations(registration.id);
  end if;

  select * into registration from event_registration where id = registration.id;

  if lesson_trainer_ids is not null then
    delete from event_lesson_request
    where registration_id = registration.id
      and trainer_id <> all(lesson_trainer_ids);

    for request in
      select demand.trainer_id, demand.lesson_count
      from unnest(lesson_trainer_ids, lesson_counts)
        demand(trainer_id, lesson_count)
      order by demand.trainer_id
    loop
      perform app_private.set_lesson_request(registration, request.trainer_id, request.lesson_count);
    end loop;
  end if;

  if person is distinct from null then
    insert into person_invitation (person_id, email, event_id)
    values (registration.person_id, btrim((set_event_registration.person).email), event_id);
  end if;

  return registration;
end;
$$;

grant execute on function set_event_registration to anonymous;
--! EndIncluded functions/set_event_registration.sql
--! Included views/crawler_frontier_fetch_job.sql
create or replace view crawler.frontier_fetch_job AS
WITH jobs AS (
  SELECT
    j.id,
    j.key,
    substring(j.key FROM '^fetch:(.*):[0-9]+$') AS host,
    CASE
      WHEN (pj.payload->>'id') ~ '^[0-9]+$' THEN (pj.payload->>'id')::bigint
    END AS frontier_id,
    j.run_at,
    j.locked_at,
    j.attempts,
    j.max_attempts,
    j.last_error,
    j.updated_at
  FROM graphile_worker.jobs j
  JOIN graphile_worker._private_jobs pj ON pj.id = j.id
  WHERE j.task_identifier = 'frontier_fetch'
)
SELECT
  j.id AS job_id,
  j.key AS job_key,
  j.host,
  j.run_at,
  j.locked_at,
  j.attempts,
  j.max_attempts,
  j.last_error AS job_error,
  j.updated_at AS job_updated_at,
  f.id AS frontier_id,
  f.federation,
  f.kind,
  f.key AS frontier_key,
  f.fetch_status,
  f.process_status,
  f.last_process_error AS process_error,
  CASE
    WHEN j.attempts >= j.max_attempts THEN 'failed'
    WHEN j.locked_at IS NOT NULL THEN 'locked'
    ELSE 'queued'
  END AS state
FROM jobs j
JOIN crawler.frontier f ON f.id = j.frontier_id
WHERE j.frontier_id IS NOT NULL;


comment on view crawler.frontier_fetch_job is '@name crawler_fetch_job
@behavior -insert -update -delete -list';
grant select on crawler.frontier_fetch_job to system_admin;
--! EndIncluded views/crawler_frontier_fetch_job.sql
--! Included functions/tg_event_people__refresh.sql
create or replace function app_private.tg_event_people__refresh() returns trigger
language plpgsql security definer
set search_path = pg_catalog, pg_temp
as $$
-- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
declare
  has_changes boolean;
begin
  if tg_op = 'INSERT' then
    has_changes := exists (select 1 from changed_rows);
  elsif tg_op = 'DELETE' then
    has_changes := exists (select 1 from deleted_rows);
  else
    -- The trigger argument supplies the view's input columns for each table.
    -- Compare sets: row order and duplicate inputs do not affect event_people.
    execute format(
      'select exists (
        (select %1$s from changed_rows except select %1$s from deleted_rows)
        union all
        (select %1$s from deleted_rows except select %1$s from changed_rows)
      )', tg_argv[0]
    ) into has_changes;
  end if;

  if has_changes then
    perform graphile_worker.add_job(
      'refresh_event_people',
      queue_name := 'refresh_event_people',
      job_key := 'refresh_event_people'
    );
  end if;
  return null;
end;
$$;

drop trigger if exists _900_refresh_event_people on event_registration;

create or replace trigger _900_refresh_event_people_ins
  after insert on event_registration
  referencing new table as changed_rows for each statement
  execute function app_private.tg_event_people__refresh();
create or replace trigger _900_refresh_event_people_del
  after delete on event_registration
  referencing old table as deleted_rows for each statement
  execute function app_private.tg_event_people__refresh();
create or replace trigger _900_refresh_event_people_upd
  after update on event_registration
  referencing old table as deleted_rows new table as changed_rows for each statement
  execute function app_private.tg_event_people__refresh('event_id, person_id, registration_status');

drop trigger if exists _900_refresh_event_people on event_trainer;

create or replace trigger _900_refresh_event_people_ins
  after insert on event_trainer
  referencing new table as changed_rows for each statement
  execute function app_private.tg_event_people__refresh();
create or replace trigger _900_refresh_event_people_del
  after delete on event_trainer
  referencing old table as deleted_rows for each statement
  execute function app_private.tg_event_people__refresh();
create or replace trigger _900_refresh_event_people_upd
  after update on event_trainer
  referencing old table as deleted_rows new table as changed_rows for each statement
  execute function app_private.tg_event_people__refresh('event_id, person_id');

drop trigger if exists _900_refresh_event_people on event;

create or replace trigger _900_refresh_event_people_ins
  after insert on event
  referencing new table as changed_rows for each statement
  execute function app_private.tg_event_people__refresh();
create or replace trigger _900_refresh_event_people_del
  after delete on event
  referencing old table as deleted_rows for each statement
  execute function app_private.tg_event_people__refresh();
create or replace trigger _900_refresh_event_people_upd
  after update on event
  referencing old table as deleted_rows new table as changed_rows for each statement
  execute function app_private.tg_event_people__refresh('id, parent_id');
--! EndIncluded functions/tg_event_people__refresh.sql
