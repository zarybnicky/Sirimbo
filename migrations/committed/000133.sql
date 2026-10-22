--! Previous: sha1:ab76d81ce4dd572d73f54f0f6768246ac629da11
--! Hash: sha1:1178896e66200153c9c92845f2d5fcc16d2a1037

--! split: 1-current.sql
do $$
begin
  if to_regclass('public.event_external_registration') is not null then
    lock table event_external_registration in access exclusive mode;
    if exists (select 1 from event_external_registration) then
      raise exception 'Convert external registrations with the manual script before removing the table';
    end if;
  end if;
end;
$$;

drop table if exists event_external_registration;

do $$
declare
  renamed_table record;
  renamed_column record;
  object_name text;
begin
  if not exists (
    select 1 from pg_catalog.pg_attribute
    where attrelid = 'public.aktuality'::regclass
      and attname = 'published_at'
      and not attisdropped
  ) then
    alter table aktuality add column published_at timestamptz;

    alter table aktuality disable trigger _100_timestamps;
    alter table aktuality disable trigger _200_author;
    update aktuality set published_at = coalesce(updated_at, created_at) where published_at is null;
    alter table aktuality enable trigger _100_timestamps;
    alter table aktuality enable trigger _200_author;
  end if;

  if to_regtype('public.event_instance_registration_status') is not null then
    alter type event_instance_registration_status rename to event_registration_status;
  end if;

  for renamed_table in
    select * from (values
      ('event_instance_target_cohort', 'event_target_cohort'),
      ('event_instance_trainer', 'event_trainer'),
      ('event_lesson_demand', 'event_lesson_request'),
      ('event_instance_registration', 'event_registration'),
      ('event_instance', 'event')
    ) names(old_name, new_name)
  loop
    if to_regclass('public.' || renamed_table.old_name) is null then
      continue;
    end if;

    execute format('alter table public.%I rename to %I', renamed_table.old_name, renamed_table.new_name);
    if starts_with(renamed_table.old_name, 'event_instance_') then
      execute format('alter table public.%I rename column instance_id to event_id', renamed_table.new_name);
    end if;

    for object_name in
      select conname from pg_catalog.pg_constraint
      where conrelid = to_regclass('public.' || renamed_table.new_name)
        and starts_with(conname::text, renamed_table.old_name || '_')
    loop
      execute format('alter table public.%I rename constraint %I to %I',
        renamed_table.new_name, object_name,
        replace(replace(object_name, renamed_table.old_name, renamed_table.new_name), 'instance_id', 'event_id'));
    end loop;

    -- Constraint-backed indexes follow their constraints; rename the remaining indexes.
    for object_name in
      select indexname from pg_catalog.pg_indexes
      where schemaname = 'public' and tablename = renamed_table.new_name
        and starts_with(indexname::text, renamed_table.old_name || '_')
    loop
      execute format('alter index public.%I rename to %I', object_name,
        replace(replace(replace(object_name, renamed_table.old_name, renamed_table.new_name),
          'instance_id', 'event_id'), '_instance_idx', '_event_idx'));
    end loop;

    execute format('alter sequence public.%I rename to %I',
      renamed_table.old_name || '_id_seq', renamed_table.new_name || '_id_seq');

    if renamed_table.new_name = 'event_lesson_request' then
      alter table event_lesson_request rename constraint eld_unique_registration_trainer_key
        to event_lesson_request_registration_id_trainer_id_key;
    end if;
  end loop;
end;
$$;

alter table aktuality alter column published_at set default now();

alter table aktuality
  drop constraint if exists aktuality_visible_requires_published_at,
  add constraint aktuality_visible_requires_published_at
  check (not is_visible or published_at is not null);

grant execute on function app_private.announcement_status_next to anonymous;

--! Included functions/upsert_article.sql
drop function if exists upsert_article;
drop type if exists article_type_input;

create type article_type_input as (
  id bigint,
  title text,
  preview text,
  body text,
  title_photo_url text,
  is_visible boolean,
  published_at timestamptz
);

create or replace function upsert_article(info article_type_input, attachments bigint[] default null)
  returns aktuality language plpgsql
  as $$
declare
  result aktuality;
begin
  if info.id is null then
    insert into aktuality (
      at_jmeno, at_preview, at_text, title_photo_url, is_visible, published_at
    ) values (
      info.title,
      coalesce(info.preview, ''),
      coalesce(info.body, ''),
      info.title_photo_url,
      coalesce(info.is_visible, true),
      coalesce(info.published_at, case when coalesce(info.is_visible, true) then now() end)
    ) returning * into result;
  else
    select * into result from aktuality where id = info.id;

    if not found then
      raise exception 'Article with id % not found', info.id;
    end if;
  end if;

  if attachments is not null then
    select coalesce(array_agg(id), '{}'::bigint[])
    into attachments
    from file
    where id = any(attachments)
      and tenant_id = result.tenant_id
      and uploaded_at is not null;

    delete from article_attachment
    where aktuality_id = result.id
      and not inline
      and file_id <> all(attachments);

    insert into article_attachment (tenant_id, aktuality_id, file_id, inline)
    select result.tenant_id, result.id, file_id, false
    from unnest(attachments) input(file_id)
    on conflict (tenant_id, aktuality_id, file_id)
    do update set inline = false;
  end if;

  if info.id is not null then
    -- Make sure the update trigger re-populates inline references
    update aktuality set
      at_jmeno = info.title,
      at_preview = coalesce(info.preview, ''),
      at_text = coalesce(info.body, ''),
      title_photo_url = info.title_photo_url,
      is_visible = coalesce(info.is_visible, true),
      published_at = coalesce(
        info.published_at,
        published_at,
        case when coalesce(info.is_visible, true) then now() end
      )
    where id = info.id
    returning * into result;
  end if;

  return result;
end;
$$;

grant all on function upsert_article to anonymous;
--! EndIncluded functions/upsert_article.sql

alter table event_registration
  add column if not exists created_by bigint references users(id) on delete set null;
alter table event_registration alter column created_by set default current_user_id();

comment on column event_registration.created_by is '@omit';
comment on constraint event_registration_created_by_fkey on event_registration is '@omit';

do $$
begin
  if exists (
    select 1 from pg_catalog.pg_attribute
    where attrelid = 'public.payment'::regclass
      and attname = 'event_instance_id' and not attisdropped
  ) then
    alter table payment rename column event_instance_id to event_id;
    alter table payment rename constraint payment_event_instance_id_fkey to payment_event_id_fkey;
  end if;
end;
$$;

do $$
begin
  if exists (
    select 1 from pg_catalog.pg_attribute
    where attrelid = 'public.person_invitation'::regclass
      and attname = 'instance_id' and not attisdropped
  ) then
    alter table person_invitation rename column instance_id to event_id;
    alter table person_invitation rename constraint person_invitation_instance_id_fkey to person_invitation_event_id_fkey;
  end if;
end;
$$;

alter table person_invitation add column if not exists event_id bigint references event(id) on delete set null;

comment on column person_invitation.event_id is '@omit';
comment on constraint person_invitation_event_id_fkey on person_invitation is '@omit';

--! Included functions/reconcile_event_cohort_registrations.sql
drop function if exists app_private.reconcile_event_instance_cohort_registrations(bigint[], bigint[], bigint[]);
drop function if exists app_private.reconcile_event_instance_cohort_registrations(bigint[], bigint[]);

create or replace function app_private.reconcile_event_cohort_registrations(
  p_event_ids bigint[],
  p_person_ids bigint[] default null
) returns void
  language sql
  security invoker
  set search_path = pg_catalog, public, pg_temp
as $$
  select instance.id from event instance
  where instance.id = any(p_event_ids) order by instance.id for update;

  -- Direct registrations (including cancellations) take precedence. Couples
  -- already supply their person rows; cohorts supply only the remaining people.
  with desired as materialized (
    select instance.id as event_id, instance.tenant_id, membership.person_id,
      min(target.cohort_id) as target_cohort_id
    from event instance
    join event_target_cohort target on target.event_id = instance.id
    join cohort_membership membership on membership.cohort_id = target.cohort_id
    where instance.id = any(p_event_ids)
      and instance.since >= now()
      and membership.status = 'active' and membership.active_range @> now()
      and (p_person_ids is null or membership.person_id = any(p_person_ids))
      and not exists (
        select 1 from event_registration registration
        where registration.event_id = instance.id
          and registration.person_id = membership.person_id
          and (
            (registration.parent_registration_id is null and registration.source is distinct from 'cohort')
            or (registration.parent_registration_id is not null and registration.registration_status = 'active')
          )
      )
    group by instance.id, instance.tenant_id, membership.person_id
  ), cancelled as (
    update event_registration registration
    set registration_status = 'cancelled'
    from event instance
    where registration.event_id = any(p_event_ids)
      and instance.id = registration.event_id and instance.since >= now()
      and registration.source = 'cohort'
      and registration.registration_status = 'active'
      and (p_person_ids is null or registration.person_id = any(p_person_ids))
      and not exists (
        select 1 from desired
        where desired.event_id = registration.event_id
          and desired.person_id = registration.person_id
      )
  )
  insert into event_registration as registration (
    tenant_id, event_id, person_id, target_cohort_id, source, status
  )
  select tenant_id, event_id, person_id, target_cohort_id, 'cohort', 'unknown'
  from desired
  on conflict (event_id, couple_id, person_id) where parent_registration_id is null
  do update set registration_status = 'active', target_cohort_id = excluded.target_cohort_id
  where registration.source = 'cohort'
    and (registration.registration_status, registration.target_cohort_id)
      is distinct from ('active'::event_registration_status, excluded.target_cohort_id);
$$;

revoke all on function app_private.reconcile_event_cohort_registrations from public;
grant execute on function app_private.reconcile_event_cohort_registrations to trainer, administrator;

drop trigger if exists _500_reconcile_registrations on event_target_cohort;
drop function if exists app_private.tg_event_instance_target_cohort__reconcile();
--! EndIncluded functions/reconcile_event_cohort_registrations.sql
--! Included functions/tg_cohort_membership__on_status.sql
CREATE or replace FUNCTION app_private.tg_cohort_membership__on_status()
  RETURNS trigger LANGUAGE plpgsql
  security definer
  SET search_path = pg_catalog, public, app_private
  AS $$
declare
  event_ids bigint[];
  person_ids bigint[];
begin
  select array_agg(distinct target.event_id)
  into event_ids
  from event_target_cohort target
  join event instance on instance.id = target.event_id
  where target.cohort_id = any (
    case when tg_op = 'INSERT' then array[new.cohort_id]
      when tg_op = 'DELETE' then array[old.cohort_id]
      else array[old.cohort_id, new.cohort_id]
    end
  ) and instance.since >= now();

  if event_ids is not null then
    person_ids := case when tg_op = 'INSERT' then array[new.person_id]
      when tg_op = 'DELETE' then array[old.person_id]
      else array[old.person_id, new.person_id]
    end;
    perform app_private.reconcile_event_cohort_registrations(event_ids, person_ids);
  end if;

  return case when tg_op = 'DELETE' then old else new end;
end;
$$;

grant all on function app_private.tg_cohort_membership__on_status to trainer;

DROP TRIGGER IF EXISTS _500_on_status ON cohort_membership;
CREATE TRIGGER _500_on_status
  AFTER INSERT OR DELETE OR UPDATE OF status, cohort_id, person_id, since, until
  ON cohort_membership
  FOR EACH ROW EXECUTE FUNCTION app_private.tg_cohort_membership__on_status();
--! EndIncluded functions/tg_cohort_membership__on_status.sql
--! Included functions/can_trainer_edit_event.sql
create or replace function app_private.can_trainer_edit_event(p_event_id bigint)
  returns boolean
  language sql stable security definer leakproof parallel safe
  set search_path = pg_catalog, public, pg_temp
as $$
  with recursive chain as (
    select i.id, i.parent_id from event i where i.id = p_event_id
    union all
    select p.id, p.parent_id from event p join chain c on p.id = c.parent_id
  ),
  trainers as (
    select person_id from event_trainer where event_id in (select id from chain)
  )
  select exists (
    select 1 from trainers where person_id = any ((select current_person_ids())::bigint[])
  ) or not exists (
    select 1 from trainers
  );
$$;

grant all on function app_private.can_trainer_edit_event to anonymous;
--! EndIncluded functions/can_trainer_edit_event.sql
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
  with recursive affected as (
    select id from event where id = p_event_id
    union all
    select child.id
    from event child
    join affected parent on child.parent_id = parent.id
  ), desired as (
    select id, app_private.event_manager_person_ids(id) as person_ids
    from affected
  )
  update event instance
  set manager_person_ids = desired.person_ids
  from desired
  where instance.id = desired.id
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
--! Included functions/ensure_couple_person_registrations.sql
create or replace function app_private.ensure_couple_person_registrations(p_registration_id bigint)
returns void language plpgsql
set search_path = pg_catalog, public, pg_temp
as $$
declare
  parent event_registration;
  child event_registration;
  partner_id bigint;
begin
  perform e.id from event e
  join event_registration r on r.event_id = e.id
  where r.id = p_registration_id for update of e;

  select * into parent from event_registration
  where id = p_registration_id and couple_id is not null and registration_status = 'active';
  if not found then return; end if;

  for partner_id in
    select unnest(array[man_id, woman_id]) from couple where id = parent.couple_id
  loop
    select * into child from event_registration
    where event_id = parent.event_id and person_id = partner_id
    order by (registration_status = 'active') desc,
      (parent_registration_id = parent.id) desc nulls last,
      (parent_registration_id is null) desc, id desc
    limit 1 for update;

    if found then
      if child.registration_status = 'active' and child.parent_registration_id is not null
        and child.parent_registration_id <> parent.id then
        raise exception 'PERSON_ALREADY_REGISTERED' using errcode = '22023';
      end if;

      -- Keep attendance, notes and personal lesson requests on the same row.
      update event_registration
      set parent_registration_id = parent.id, registration_status = 'active',
          source = parent.source, target_cohort_id = null
      where id = child.id and (parent_registration_id, registration_status, source, target_cohort_id)
        is distinct from (parent.id, 'active'::event_registration_status, parent.source, null);
    else
      insert into event_registration (event_id, tenant_id, parent_registration_id, person_id, status, source)
      values (parent.event_id, parent.tenant_id, parent.id, partner_id, 'unknown', parent.source);
    end if;
  end loop;
end;
$$;

revoke all on function app_private.ensure_couple_person_registrations from public;
grant execute on function app_private.ensure_couple_person_registrations to trainer, administrator;
--! EndIncluded functions/ensure_couple_person_registrations.sql
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
  enable_notes boolean
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
  v_is_locked boolean := coalesce((details).is_locked, false);
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
        is_locked,
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
        v_is_locked,
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
          is_locked = v_is_locked,
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
  is_manager := pg_has_role(
    coalesce(nullif(current_setting('role'), 'none'), session_user), 'administrator', 'member'
  ) or (
    pg_has_role(
      coalesce(nullif(current_setting('role'), 'none'), session_user), 'trainer', 'member'
    ) and app_private.can_trainer_edit_event(instance.id)
  );

  if not coalesce(is_self, false) and not coalesce(is_manager, false) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  if instance.is_locked and not is_manager then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;

  return app_private.set_lesson_request(registration, $2, $3);
end;
$$;

grant all on function set_lesson_request to anonymous;

create function set_lesson_demand(
  instance_registration_id bigint,
  instance_trainer_id bigint,
  lesson_count integer
) returns event_lesson_request
language sql strict
as $$
  select public.set_lesson_request($1, $2, $3);
$$;

comment on function set_lesson_demand is E'@deprecated Use setLessonRequest.\n@resultFieldName eventLessonDemand';
grant execute on function set_lesson_demand to anonymous;
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
    raise exception 'INVALID_REGISTRANT' using errcode = '22023';
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

  is_manager := pg_has_role(
    coalesce(nullif(current_setting('role'), 'none'), session_user), 'administrator', 'member'
  ) or (
    pg_has_role(
      coalesce(nullif(current_setting('role'), 'none'), session_user), 'trainer', 'member'
    ) and app_private.can_trainer_edit_event(event_id)
  );

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
  if target_instance.is_locked and not is_manager then
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
      remaining_capacity := event_remaining_person_spots(target_instance);
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

create function set_event_instance_registration(
  p_instance_id bigint,
  p_person_id bigint,
  p_couple_id bigint,
  p_is_registered boolean,
  p_note text default null,
  p_lesson_trainer_ids bigint[] default null,
  p_lesson_counts integer[] default null,
  p_person event_registration_person_input default null
) returns event_registration
language sql
as $$
  select public.set_event_registration($1, $2, $3, $4, $5, $6, $7, $8);
$$;

comment on function set_event_instance_registration is E'@deprecated Use setEventRegistration.\n@resultFieldName eventInstanceRegistration';
grant execute on function set_event_instance_registration to anonymous;
--! EndIncluded functions/set_event_registration.sql
--! Included functions/event_trainers.sql
drop function if exists event_instance_trainers;
drop function if exists app_private.event_instance_trainers_at;

create or replace function app_private.event_trainers_at(
  v_instance event,
  v_at timestamptz
)
  returns setof tenant_trainer
  language sql stable
as $$
select distinct on (tt.tenant_id, tt.person_id) tt.*
from event_trainer k
join tenant_trainer tt on tt.tenant_id = k.tenant_id and tt.person_id = k.person_id
where k.event_id = v_instance.id
  and tt.active_range @> v_at
order by tt.tenant_id, tt.person_id, lower(tt.active_range) desc;
$$;
grant all on function app_private.event_trainers_at to anonymous;

create or replace function event_trainers(v_instance event)
  returns setof tenant_trainer
  language sql stable
as $$
select * from app_private.event_trainers_at(v_instance, v_instance.since);
$$;

grant all on function event_trainers to anonymous;
comment on function event_trainers is '@simpleCollections only
@deprecated Use eventTrainersList.';
--! EndIncluded functions/event_trainers.sql
--! Included functions/event_remaining_x.sql
drop function if exists event_instance_remaining_person_spots;

drop function if exists event_instance_trainer_lessons_remaining;

CREATE or replace FUNCTION event_trainer_lessons_remaining(e event_trainer) RETURNS integer AS $$
  select case
    when e.lessons_offered is null then null
    else e.lessons_offered - (
      select coalesce(sum(demand.lesson_count), 0)
      from event_lesson_request demand
      join event_registration registration
        on registration.id = demand.registration_id
       and registration.registration_status = 'active'
      where demand.trainer_id = e.id
    )
  end;
$$ LANGUAGE sql STABLE security definer set search_path = public, pg_catalog, pg_temp;

CREATE or replace FUNCTION event_remaining_person_spots(inst event) RETURNS integer AS $$
  select case
    when inst.capacity is null or inst.capacity <= 0 then null
    else inst.capacity - case inst.capacity_unit
        when 'people' then (
          select count(*)::integer
          from event_registration registration
          where registration.event_id = inst.id
            and registration.person_id is not null
            and registration.registration_status = 'active'
        )
        when 'registrations' then (
          select count(*)::integer
          from event_registration registration
          where registration.event_id = inst.id
            and registration.parent_registration_id is null
            and registration.registration_status = 'active'
        )
      end
  end;
$$ LANGUAGE sql STABLE security definer set search_path = public, pg_catalog, pg_temp;

GRANT ALL ON FUNCTION event_trainer_lessons_remaining(e event_trainer) TO anonymous;
GRANT ALL ON FUNCTION event_remaining_person_spots(inst event) TO anonymous;
--! EndIncluded functions/event_remaining_x.sql
--! Included functions/event_series_info.sql
drop function if exists event_instance_series_info;

create or replace function event_series_info(instance event)
  returns series_info
  language sql
  stable
as $$
  select row(
    ranked.series_id,
    ranked.name,
    ranked.position,
    ranked.length,
    ranked.since,
    ranked.until
  )::series_info
  from (
    select
      member.id as member_id,
      series.id as series_id,
      series.name,
      row_number() over (order by member.since, member.id)::integer as position,
      count(*) over ()::integer as length,
      min(member.since) over () as since,
      max(member.until) over () as until
    from event member
    join event_series series on series.tenant_id = member.tenant_id and series.id = member.series_id
    where member.series_id = instance.series_id
  ) ranked
  where ranked.member_id = instance.id;
$$;

grant execute on function event_series_info to anonymous;
--! EndIncluded functions/event_series_info.sql
--! Included functions/event_share_token.sql
drop function if exists event_instance_share_token;

create or replace function event_share_token(i event) returns text
  language sql stable
as $$
  select share_token
  from event
  where id = $1.id
    and (pg_has_role(current_user, 'administrator', 'member')
      or (pg_has_role(current_user, 'trainer', 'member')
      and app_private.can_trainer_edit_event($1.id)))
$$;

grant execute on function event_share_token to anonymous;
--! EndIncluded functions/event_share_token.sql
--! Included functions/event_sharing.sql
create or replace function set_event_sharing(id bigint, p_enabled boolean) returns text
  language plpgsql volatile
as $$
declare
  v_share_token text;
begin
  update event instance
  set share_token = case
    when not coalesce(p_enabled, false) then null
    when instance.share_token is null
      then translate(encode(gen_random_bytes(24), 'base64'), '+/', '-_')
    else instance.share_token
  end
  where instance.id = $1
  returning instance.share_token into v_share_token;

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '22023';
  end if;

  return v_share_token;
end;
$$;

grant execute on function set_event_sharing to anonymous;
--! EndIncluded functions/event_sharing.sql
--! Included functions/tg_event__validate_parent.sql
drop trigger if exists _200_validate_parent on event;
drop function if exists app_private.tg_event_instance__validate_parent;

create or replace function app_private.tg_event__validate_parent()
  returns trigger
  language plpgsql
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
begin
  if new.parent_id is null then return new; end if;

  perform 1 from event parent where parent.id = new.parent_id for no key update;

  if not found or exists (
    with recursive ancestors as (
      select id, parent_id
      from event where id = new.parent_id
      union
      select parent.id, parent.parent_id
      from event parent
      join ancestors child on parent.id = child.parent_id
    )
    select 1 from ancestors where id = new.id
  ) then
    raise exception 'Event instance parent must not create a cycle'
      using errcode = '23514';
  end if;

  return new;
end;
$$;

create trigger _200_validate_parent
  before insert or update of tenant_id, parent_id on event
  for each row execute function app_private.tg_event__validate_parent();
--! EndIncluded functions/tg_event__validate_parent.sql
--! Included functions/tg_event__delete_payment_on_cancellation.sql
drop trigger if exists _500_delete_on_cancellation on event;
drop function if exists app_private.tg_event_instance__delete_payment_on_cancellation;

create or replace function app_private.tg_event__delete_payment_on_cancellation()
  returns trigger
  language plpgsql
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
declare
  payment_id bigint;
begin
  delete from payment where event_id = OLD.id;

  if not new.is_cancelled then
    select (create_event_payment(i)).id into payment_id
    from event i
    where i.type='lesson'
      and i.id = NEW.id
      and i.since < now()
      and not i.is_cancelled
      and not exists (
        select * from payment where event_id = i.id
      );

    update payment set status ='unpaid' where id = payment_id;
    perform resolve_payment_with_credit(payment.*) from payment where id = payment_id;
  end if;

  return OLD;
end;
$$;


create trigger _500_delete_on_cancellation
  after update of is_cancelled on event
  for each row
  when (old.is_cancelled is distinct from new.is_cancelled)
  execute function app_private.tg_event__delete_payment_on_cancellation();
--! EndIncluded functions/tg_event__delete_payment_on_cancellation.sql
--! Included functions/tg_transaction__effective_date.sql
create or replace function app_private.tg_transaction__effective_date() returns trigger
    language plpgsql security definer
    set search_path to pg_catalog, public, pg_temp
    as $$
begin
  if NEW.effective_date is null then
    NEW.effective_date = (select coalesce(
      (select since from payment join event on event_id = event.id where NEW.payment_id = payment.id),
      (select due_at from payment where NEW.payment_id = payment.id),
      NEW.created_at
    ));
  end if;
  return NEW;
end;
$$;

create or replace trigger _300_effective_date
  before insert or update on transaction
  for each row
  execute procedure app_private.tg_transaction__effective_date();
--! EndIncluded functions/tg_transaction__effective_date.sql
--! Included functions/create_latest_lesson_payments.sql
CREATE or replace FUNCTION app_private.create_latest_lesson_payments() RETURNS SETOF payment
  LANGUAGE plpgsql
AS $$
begin
  perform set_config('jwt.claims.tenant_id', '2', true);
  perform set_config('jwt.claims.my_tenant_ids', '[2]', true);
  if not (select row_security_active('event')) then
    set local role to administrator;
  end if;

  return query WITH created AS (
    SELECT p.*
    FROM event ei
      JOIN LATERAL create_event_payment(ei) p ON true
    WHERE ei.type = 'lesson'
      AND NOT ei.is_cancelled
      AND ei.since < now()
      AND NOT EXISTS (
        SELECT 1
        FROM payment p
        WHERE p.event_id = ei.id AND p.status = 'paid'
      )
      AND p.status in ('unpaid', 'tentative')
  ),
  unpaid AS (
    UPDATE payment p
      SET status = 'unpaid'
      FROM created
      WHERE p.id = created.id
      RETURNING p.*
  )
  SELECT p.*
  FROM unpaid
    CROSS JOIN LATERAL resolve_payment_with_credit(unpaid.*) p
  WHERE p IS NOT NULL;
end;
$$;
--! EndIncluded functions/create_latest_lesson_payments.sql
--! Included functions/system_admin_tenants.sql
create or replace function system_admin_tenants()
returns table (
  id bigint,
  name text,
  description text,
  bank_account text,
  origins text[],
  cz_ico text,
  cz_dic text,
  address address_domain,
  settings text,
  membership_count bigint,
  trainer_count bigint,
  administrator_count bigint,
  session_count_last_30_days bigint,
  session_count_per_trainer_last_30_days double precision
)
  language plpgsql
  stable
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
begin
  if not app_private.is_system_admin(current_user_id()) then
    raise exception 'permission denied for system admin tenant overview'
      using errcode = '42501';
  end if;

  return query
  select
    t.id,
    t.name,
    t.description,
    t.bank_account,
    t.origins,
    t.cz_ico,
    t.cz_dic,
    t.address,
    coalesce(ts.settings::text, '{}'),
    membership_counts.membership_count,
    staffing.trainer_count,
    administrators.administrator_count,
    load.session_count_last_30_days,
    load.session_count_per_trainer_last_30_days
  from tenant t
  left join tenant_settings ts on ts.tenant_id = t.id
  cross join lateral (
    select count(*) as membership_count from tenant_membership tm where tm.tenant_id = t.id and tm.status = 'active'
  ) as membership_counts
  cross join lateral (
    select count(*) as trainer_count from tenant_trainer tt where tt.tenant_id = t.id and tt.status = 'active'
  ) as staffing
  cross join lateral (
    select count(*) as administrator_count from tenant_administrator ta where ta.tenant_id = t.id and ta.status = 'active'
  ) as administrators
  cross join lateral (
    select
      count(*) as session_count_last_30_days,
      case
        when coalesce(staffing.trainer_count, 0) > 0 then count(*)::double precision / staffing.trainer_count::double precision
        else 0::double precision
      end as session_count_per_trainer_last_30_days
    from event ei
    where ei.tenant_id = t.id
      and coalesce(ei.is_cancelled, false) = false
      and ei.since >= now() - interval '30 days'
  ) as load
  order by t.name;
end;
$$;

grant execute on function system_admin_tenants to anonymous;
--! EndIncluded functions/system_admin_tenants.sql
--! Included functions/event_stats.sql
drop trigger if exists _500_eir_refresh_stats_ins on event_registration;
drop trigger if exists _500_eir_refresh_stats_del on event_registration;
drop trigger if exists _500_eir_refresh_stats_upd on event_registration;
drop function if exists app_private.tg_eir__refresh_stats_stmt;
drop function if exists app_private.refresh_event_instance_stats;

create or replace function app_private.refresh_event_stats(p_event_id bigint)
  returns void
  language sql volatile security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  select 1 from event where id = p_event_id for no key update;

  update event instance
  set stats = actual.stats
  from (
    select jsonb_build_object(
      'TOTAL', count(*)::int,
      'UNKNOWN', count(*) filter (where status = 'unknown')::int,
      'ATTENDED', count(*) filter (where status = 'attended')::int,
      'NOT_EXCUSED', count(*) filter (where status = 'not-excused')::int
    ) as stats
    from event_registration
    where event_id = p_event_id
      and person_id is not null
      and registration_status = 'active'
  ) actual
  where instance.id = p_event_id and instance.stats is distinct from actual.stats;
$$;

create or replace function app_private.tg_event_registration__refresh_stats_stmt()
  returns trigger
  language plpgsql security definer
  set search_path = pg_catalog, pg_temp
as $$
-- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
begin
  if tg_op = 'INSERT' then
    perform app_private.refresh_event_stats(affected.event_id)
    from (
      select distinct event_id from changed_rows
      where person_id is not null order by event_id
    ) affected;
  elsif tg_op = 'DELETE' then
    perform app_private.refresh_event_stats(affected.event_id)
    from (
      select distinct event_id from deleted_rows
      where person_id is not null order by event_id
    ) affected;
  else
    perform app_private.refresh_event_stats(affected.event_id)
    from (
      select distinct affected.event_id
      from deleted_rows old_row
      join changed_rows new_row using (id)
      cross join lateral (values
        (old_row.event_id, old_row.person_id),
        (new_row.event_id, new_row.person_id)
      ) affected(event_id, person_id)
      where row(old_row.event_id, old_row.person_id, old_row.status, old_row.registration_status)
        is distinct from
        row(new_row.event_id, new_row.person_id, new_row.status, new_row.registration_status)
        and affected.person_id is not null
      order by affected.event_id
    ) affected;
  end if;
  return null;
end;
$$;

revoke execute on function app_private.tg_event_registration__refresh_stats_stmt() from public, anonymous;

create or replace trigger _500_refresh_stats_ins
  after insert on event_registration
  referencing new table as changed_rows for each statement
  execute function app_private.tg_event_registration__refresh_stats_stmt();
create or replace trigger _500_refresh_stats_del
  after delete on event_registration
  referencing old table as deleted_rows for each statement
  execute function app_private.tg_event_registration__refresh_stats_stmt();
create or replace trigger _500_refresh_stats_upd
  after update on event_registration
  referencing old table as deleted_rows new table as changed_rows for each statement
  execute function app_private.tg_event_registration__refresh_stats_stmt();
--! EndIncluded functions/event_stats.sql
--! Included functions/tg_event_registration__attendance_timestamps.sql
drop trigger if exists _200_eir_attendance_timestamps on event_registration;
drop function if exists app_private.tg_eir__attendance_timestamps;

create or replace function app_private.tg_event_registration__attendance_timestamps()
  returns trigger
  language plpgsql
  set search_path = pg_catalog, pg_temp
as $$
begin
  if new.person_id is null then
    new.attendance_created_at := null;
    new.attendance_updated_at := null;
  elsif tg_op = 'INSERT' or new.person_id is distinct from old.person_id then
    new.attendance_created_at := new.created_at;
    new.attendance_updated_at := new.updated_at;
  else
    new.attendance_created_at := old.attendance_created_at;
    new.attendance_updated_at := case
      when new.status is not distinct from old.status
        and new.attendance_note is not distinct from old.attendance_note
        then old.attendance_updated_at
      when old.attendance_updated_at >= new.updated_at
        then old.attendance_updated_at + interval '1 millisecond'
      else new.updated_at
    end;
  end if;
  return new;
end;
$$;
revoke execute on function app_private.tg_event_registration__attendance_timestamps() from public, anonymous;

create or replace trigger _200_attendance_timestamps
  before insert or update of person_id, status, attendance_note
  on event_registration
  for each row execute function app_private.tg_event_registration__attendance_timestamps();
--! EndIncluded functions/tg_event_registration__attendance_timestamps.sql
--! Included functions/create_event_payment.sql
drop function if exists create_event_instance_payment;

create or replace function create_event_payment(target_event event)
  returns payment
  language plpgsql
as $$
declare
  created_payment payment;
  duration numeric(19, 4);
begin
  if current_tenant_id() <> 2 then
    return null;
  end if;

  -- Serialize initial charging with registration changes and their deferred sync.
  select * into target_event from event
  where id = target_event.id and tenant_id = current_tenant_id() for no key update;
  if not found or target_event.is_cancelled then return null; end if;

  select p.* into created_payment from payment p where p.event_id = target_event.id;

  if found then
    return created_payment;
  end if;

  if target_event.type <> 'lesson' or not exists (
    select 1
    from event_registration registration
    where registration.event_id = target_event.id
      and registration.person_id is not null
      and registration.registration_status = 'active'
  ) then
    return null;
  end if;

  duration := extract(epoch from (target_event.until - target_event.since)) / 60;

  insert into payment (accounting_period_id, status, event_id, due_at)
  values (
    (select id from accounting_period where tenant_id = current_tenant_id() and range @> now()),
    'tentative', target_event.id, now() + interval '2 weeks'
  )
  on conflict (event_id) where event_id is not null do nothing
  returning * into created_payment;

  if not found then
    return (select p from payment p where p.event_id = target_event.id);
  end if;

  insert into payment_recipient (payment_id, account_id, amount)
  select distinct on (account.id)
    created_payment.id,
    account.id,
    trainer.member_price_45min_amount / 45 * duration
  from event_trainers(target_event) trainer
  cross join lateral person_account(trainer.person_id, trainer.currency) account
  where trainer.tenant_id = current_tenant_id()
    and trainer.member_price_45min_amount is not null;

  insert into payment_debtor (payment_id, person_id)
  select distinct created_payment.id, registration.person_id
  from event_registration registration
  where registration.event_id = target_event.id
    and registration.person_id is not null
    and registration.registration_status = 'active';

  return created_payment;
end
$$;

COMMENT ON FUNCTION create_event_payment IS '@omit';
GRANT ALL ON FUNCTION create_event_payment TO anonymous;
--! EndIncluded functions/create_event_payment.sql
--! Included functions/tg_event_registration__sync_lesson_payment.sql
drop trigger if exists _800_sync_lesson_payment on event_registration;
drop function if exists app_private.tg_eir__sync_lesson_payment;

create or replace function app_private.tg_event_registration__sync_lesson_payment()
  returns trigger language plpgsql security definer
  set search_path = pg_catalog, public, pg_temp
as $$
declare
  affected_event_id bigint;
  instance event;
  payment_id bigint;
begin
  for affected_event_id in select distinct unnest(array[old.event_id, new.event_id]) loop
    select * into instance from event where id = affected_event_id for no key update;
    if not found then continue; end if;

    if (
      not exists (select 1 from payment where event_id = instance.id)
      or array(select d.person_id from payment_debtor d join payment p on p.id = d.payment_id
          where p.event_id = instance.id order by d.person_id)
        = array(select distinct r.person_id from event_registration r
          where r.event_id = instance.id and r.registration_status = 'active'
            and r.person_id is not null and not instance.is_cancelled order by r.person_id)
    ) then
      continue;
    end if;

    delete from payment where event_id = instance.id;
    if not instance.is_cancelled and instance.type = 'lesson' and instance.since < now() then
      payment_id := (create_event_payment(instance)).id;
      update payment set status = 'unpaid' where id = payment_id;
      perform resolve_payment_with_credit(payment.*) from payment where id = payment_id;
    end if;
  end loop;
  return null;
end;
$$;

create constraint trigger _800_sync_lesson_payment
  after insert or delete or update of event_id, person_id, registration_status
  on event_registration
  deferrable initially deferred for each row
  execute function app_private.tg_event_registration__sync_lesson_payment();
--! EndIncluded functions/tg_event_registration__sync_lesson_payment.sql
--! Included functions/event_registration_last_attended.sql
drop function if exists event_instance_registration_last_attended;

create or replace function event_registration_last_attended(reg event_registration)
  returns timestamptz
  language sql stable
as $$
  select max(attended.since)
  from event_registration r
  join event current on current.id = reg.event_id
  join event attended on attended.id = r.event_id
  where current.series_id is not null
    and attended.series_id = current.series_id
    and r.person_id = reg.person_id
    and r.status = 'attended'
$$;

grant all on function event_registration_last_attended to anonymous;
--! EndIncluded functions/event_registration_last_attended.sql
--! Included functions/scoreboard.sql
drop view if exists scoreboard;
drop function if exists scoreboard_entries;

create or replace function scoreboard_entries(
  since date,
  until date,
  cohort_id bigint default null
)
returns setof scoreboard_record
language sql
stable
as $$
  with membership as (
    select cm.person_id, cm.cohort_id
    from current_cohort_membership cm
    where (cm.cohort_id = scoreboard_entries.cohort_id or scoreboard_entries.cohort_id is null)
  ),
  instances as (
    select
      i.id as event_id,
      i.since,
      i.until,
      i.type as event_type
    from event i
    where not i.is_cancelled
      and i.since >= scoreboard_entries.since::timestamptz
      and i.until  < scoreboard_entries.until::timestamptz
      and i.type <> 'reservation'
  ),
  member_people as (
    select distinct person_id
    from membership
  ),
  attendance as materialized (
    select
      ea.person_id,
      scoreboard_entries.cohort_id as cohort_id,
      case when inst.event_type = 'lesson' then 1 else 0 end as lesson_score,
      case when inst.event_type = 'group' then floor((extract(epoch from (inst.until - inst.since)) / 60)::numeric / 45::numeric) else 0 end as group_score,
      case when inst.event_type = 'camp'  then 3 + 2 * ((extract(epoch from (inst.until - inst.since)) > 86400)::int) else 0 end as event_score,
      date_trunc('day', inst.since)::date as day
    from instances inst
    join event_registration ea
      on ea.event_id = inst.event_id and ea.person_id is not null
    left join event_registration registration
      on registration.id = ea.parent_registration_id
    left join cohort tc
      on tc.id = coalesce(ea.target_cohort_id, registration.target_cohort_id)
    where
      ea.registration_status = 'active'
      and (ea.status = 'attended' or inst.event_type = 'lesson')
      and ea.person_id = any (select person_id from member_people)
      and (scoreboard_entries.cohort_id is null or tc.id = scoreboard_entries.cohort_id)
  ),
  per_day as (
    select
      person_id,
      cohort_id,
      least(sum(lesson_score), 4) as lesson_score,
      sum(group_score) as group_score,
      sum(event_score) as event_score
    from attendance
    group by person_id, cohort_id, day
  ),
  aggregated as (
    select
      person_id,
      cohort_id,
      coalesce(sum(lesson_score), 0)::bigint as lesson_total_score,
      coalesce(sum(group_score), 0)::bigint as group_total_score,
      coalesce(sum(event_score), 0)::bigint as event_total_score
    from per_day
    group by person_id, cohort_id
  ),
  manual as (
    select
      sma.person_id,
      case
        when scoreboard_entries.cohort_id is null then null
        else coalesce(sma.cohort_id, scoreboard_entries.cohort_id)
      end as cohort_id,
      sum(sma.points)::bigint as manual_total_score
    from scoreboard_manual_adjustment sma
    where sma.tenant_id = current_tenant_id()
      and sma.awarded_at >= scoreboard_entries.since
      and sma.awarded_at < scoreboard_entries.until
      and (
        scoreboard_entries.cohort_id is null or
        sma.cohort_id is null or
        sma.cohort_id = scoreboard_entries.cohort_id
      )
      and exists (
        select 1
        from membership mem
        where mem.person_id = sma.person_id
          and (
            (scoreboard_entries.cohort_id is null and sma.cohort_id is null)
            or mem.cohort_id is not distinct from coalesce(sma.cohort_id, scoreboard_entries.cohort_id)
          )
      )
    group by 1, 2
  ),
  totals as (
    select
      coalesce(a.person_id, m.person_id) as person_id,
      coalesce(a.cohort_id, m.cohort_id) as cohort_id,
      coalesce(a.lesson_total_score, 0)::bigint as lesson_total_score,
      coalesce(a.group_total_score, 0)::bigint as group_total_score,
      coalesce(a.event_total_score, 0)::bigint as event_total_score,
      coalesce(m.manual_total_score, 0)::bigint as manual_total_score,
      (
        coalesce(a.lesson_total_score, 0)::bigint +
        coalesce(a.group_total_score, 0)::bigint +
        coalesce(a.event_total_score, 0)::bigint +
        coalesce(m.manual_total_score, 0)::bigint
      ) as total_score
    from aggregated a
    full join manual m using (person_id, cohort_id)
  )
  select
    person_id,
    cohort_id,
    lesson_total_score,
    group_total_score,
    event_total_score,
    manual_total_score,
    total_score,
    rank() over (order by total_score desc, person_id) as ranking
  from totals
  order by total_score desc, person_id;
$$;

comment on function scoreboard_entries is '@simpleCollections only';

grant all on function scoreboard_entries to anonymous;
--! EndIncluded functions/scoreboard.sql
--! Included functions/event_share_claims.sql
drop function if exists app_private.event_share_claims;

create function app_private.event_share_claims(p_tenant_id bigint, p_share text)
  returns table (instance_ids bigint[])
  language sql stable
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  with recursive root as (
    select id
    from event
    where tenant_id = p_tenant_id
      and (
        share_token = p_share
        or (
          is_public
          and has_public_details
          and id = case
            when p_share ~ '^\d{1,18}$' then p_share::bigint
          end
        )
      )
  ),
  events as (
    select id from root
    union
    select child.id
    from event child
    join events parent on child.parent_id = parent.id
    where child.tenant_id = p_tenant_id
  )
  select array(select id from events)
  where exists (select 1 from root);
$$;

revoke all on function app_private.event_share_claims from public;
--! EndIncluded functions/event_share_claims.sql

--! Included views/event_people.sql
create materialized view if not exists app_private.event_people as
with recursive participants as (
  select tenant_id, event_id,
    array_agg(distinct person_id order by person_id) filter (where is_attendee) as viewers,
    array_agg(distinct person_id order by person_id) as people
  from (
    select tenant_id, event_id, person_id, true as is_attendee
    from event_registration
    where registration_status = 'active' and person_id is not null
    union all
    select tenant_id, event_id, person_id, false
    from event_trainer
  ) p
  group by tenant_id, event_id
), attended(tenant_id, viewers, event_id) as (
  select tenant_id, viewers, event_id from participants where viewers is not null
  union
  select parent.tenant_id, parent.viewers, child.id
  from attended parent
  join event child
    on child.parent_id = parent.event_id and child.tenant_id = parent.tenant_id
  where child.parent_id is not null
), rosters as (
  -- Repeated events with the same people need only one expansion into person pairs.
  select distinct a.tenant_id, a.viewers, p.people
  from attended a join participants p using (tenant_id, event_id)
)
select distinct tenant_id, viewer_person_id, visible_person_id
from rosters
cross join lateral unnest(viewers) v(viewer_person_id)
cross join lateral unnest(people) p(visible_person_id);

create unique index if not exists event_people_key
  on app_private.event_people(tenant_id, viewer_person_id, visible_person_id);
--! EndIncluded views/event_people.sql
--! Included functions/accessible_event_ids.sql
create or replace function app_private.accessible_event_ids() returns setof bigint
language sql stable security definer
set search_path = pg_catalog, public, pg_temp
as $$
  with recursive attended(id) as (
    select event_id from public.event_registration
    where tenant_id = (select public.current_tenant_id())
      and person_id = any((select public.current_person_ids())::bigint[])
      and registration_status = 'active'
    union
    select child.id from public.event child
    join attended parent on child.parent_id = parent.id
    where child.parent_id is not null
      and child.tenant_id = (select public.current_tenant_id())
  )
  select id from attended
  union
  select id from public.event
  where tenant_id = (select public.current_tenant_id())
    and id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[]);
$$;

grant execute on function app_private.accessible_event_ids to anonymous;
--! EndIncluded functions/accessible_event_ids.sql
--! Included functions/tg_event_people__refresh.sql
create or replace function app_private.tg_event_people__refresh() returns trigger
language plpgsql security definer
set search_path = pg_catalog, public, pg_temp
as $$
begin
  perform graphile_worker.add_job(
    'refresh_event_people',
    queue_name := 'refresh_event_people',
    job_key := 'refresh_event_people'
  );
  return null;
end;
$$;

create or replace trigger _900_refresh_event_people
after insert or delete or update of tenant_id, event_id, person_id, registration_status
on event_registration
for each statement execute function app_private.tg_event_people__refresh();

create or replace trigger _900_refresh_event_people
after insert or delete or update of tenant_id, event_id, person_id
on event_trainer
for each statement execute function app_private.tg_event_people__refresh();

create or replace trigger _900_refresh_event_people
after insert or delete or update of tenant_id, parent_id
on event
for each statement execute function app_private.tg_event_people__refresh();
--! EndIncluded functions/tg_event_people__refresh.sql
--! Included functions/visible_person_ids.sql
create or replace function app_private.visible_person_ids() returns setof bigint
language sql stable leakproof parallel safe security definer
set search_path = pg_catalog, public, pg_temp
as $$
  select unnest(public.current_person_ids()) union all
  -- always visible: trainers + admins in current tenant
  select person_id from public.current_tenant_trainer
  union all
  select person_id from public.current_tenant_administrator
  union all
  -- members visible only if viewer is in tenant (any role)
  select person_id from public.current_tenant_membership
  where (select public.current_tenant_id() = any(public.my_tenants_array()))
  union all
  select visible_person_id from app_private.event_people
  where tenant_id = (select public.current_tenant_id())
    and viewer_person_id = any((select public.current_person_ids())::bigint[]);
$$;

grant all on function app_private.visible_person_ids() to anonymous;
--! EndIncluded functions/visible_person_ids.sql

--! Included policies/event.sql
select app_private.drop_policies('public.event');

create policy current_tenant on event as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_same_tenant on event to administrator
  using (true);
create policy trainer_select on event for select to trainer
  using (cardinality(manager_person_ids) = 0 or manager_person_ids && (select current_person_ids()));
create policy trainer_insert on event for insert to trainer
  with check (parent_id is null or app_private.can_trainer_edit_event(parent_id));
create policy trainer_update on event for update to trainer
  using (app_private.can_trainer_edit_event(id));
create policy trainer_delete on event for delete to trainer
  using (app_private.can_trainer_edit_event(id));
create policy member_view on event for select to member
  using (is_visible);
create policy public_view on event for select to anonymous
  using (is_public);
create policy event_access_view on event for select to anonymous
  using (id in (select app_private.accessible_event_ids()));
--! EndIncluded policies/event.sql
--! Included policies/event_series.sql
select app_private.drop_policies('public.event_series');

create policy current_tenant on event_series as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_series to administrator
  using (true);
create policy trainer_all on event_series to trainer
  using (true);
create policy member_view on event_series for select to member
  using (exists (select 1 from event i where i.series_id = event_series.id));
create policy public_view on event_series for select to anonymous
  using (exists (select 1 from event i where i.series_id = event_series.id));
--! EndIncluded policies/event_series.sql
--! Included policies/event_trainer.sql
select app_private.drop_policies('public.event_trainer');

create policy current_tenant on event_trainer as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_trainer to administrator
  using (true);
create policy trainer_same_tenant on event_trainer to trainer
  using (app_private.can_trainer_edit_event(event_id));
create policy member_view on event_trainer for select to member
  using (true);
create policy public_view on event_trainer for select to anonymous
  using (event_id in (select id from event where is_public));
create policy event_access_view on event_trainer for select to anonymous
  using (event_id in (select app_private.accessible_event_ids()));
--! EndIncluded policies/event_trainer.sql
--! Included policies/event_target_cohort.sql
select app_private.drop_policies('public.event_target_cohort');

create policy current_tenant on event_target_cohort as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_target_cohort to administrator
  using (true);
create policy trainer_select on event_target_cohort to trainer
  using (app_private.can_trainer_edit_event(event_id));
create policy member_view on event_target_cohort for select to member
  using (exists (select 1 from event e where e.id = event_target_cohort.event_id));
create policy event_access_view on event_target_cohort for select to anonymous
  using (event_id in (select app_private.accessible_event_ids()));
--! EndIncluded policies/event_target_cohort.sql
--! Included policies/cohort.sql
select app_private.drop_policies('public.cohort');

create policy current_tenant on cohort as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on cohort to administrator
  using (true);
create policy public_view on cohort for select to anonymous
  using (is_visible
     or id in (
       select cohort_id from event_target_cohort
       where event_id in (select app_private.accessible_event_ids())));
--! EndIncluded policies/cohort.sql
--! Included policies/event_registration.sql
select app_private.drop_policies('public.event_registration');

create policy current_tenant on event_registration as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_registration to administrator
  using (true);
create policy trainer_insert on event_registration for insert to trainer
  with check (app_private.can_trainer_edit_event(event_id));
create policy trainer_update on event_registration for update to trainer
  using (app_private.can_trainer_edit_event(event_id));
create policy insert_my on event_registration for insert
  with check (person_id = any((select current_person_ids())::bigint[])
    or couple_id = any((select current_couple_ids())::bigint[]));
create policy delete_my on event_registration for delete
  using (person_id = any((select current_person_ids())::bigint[])
    or couple_id = any((select current_couple_ids())::bigint[]));
create policy view_visible_instance on event_registration for select
  using (exists (select 1 from event e where e.id = event_registration.event_id));

grant all on event_registration to anonymous;
--! EndIncluded policies/event_registration.sql
--! Included policies/event_lesson_request.sql
select app_private.drop_policies('public.event_lesson_request');

create policy current_tenant on event_lesson_request as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_lesson_request to administrator
  using (true);
create policy view_visible_instance on event_lesson_request for select
  using (exists (select 1 from event_registration r where r.id = event_lesson_request.registration_id));

grant all on event_lesson_request to anonymous;
--! EndIncluded policies/event_lesson_request.sql
--! Included policies/file.sql
select app_private.drop_policies('public.file');
create policy current_tenant on file as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on file to administrator
  using (true);
create policy uploader_manage on file to trainer
  using (uploaded_by = (select current_user_id()));
create policy visible on file for select
  using (id in (select id from app_private.visible_file_ids()));

select app_private.drop_policies('public.announcement_attachment');
create policy current_tenant on announcement_attachment as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on announcement_attachment to administrator
  using (true);
create policy trainer_manage on announcement_attachment to trainer
  using (exists (
    select 1 from announcement a
    where a.id = announcement_attachment.announcement_id
      and a.author_id = (select current_user_id())));
create policy member_view on announcement_attachment for select
  using (exists (select 1 from announcement a where a.id = announcement_attachment.announcement_id));

select app_private.drop_policies('public.article_attachment');
create policy current_tenant on article_attachment as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on article_attachment to administrator
  using (true);
create policy public_view on article_attachment for select
  using (exists (select 1 from aktuality a where a.id = article_attachment.aktuality_id));
--! EndIncluded policies/file.sql
--! Included policies/person.sql
select app_private.drop_policies('public.person');

create policy admin_all on person to administrator using (true);
create policy admin_myself on person for update
  using (id = any ((select current_person_ids())::bigint[]));
create policy view_tenant_or_trainer on person for select
  using (id in (select app_private.visible_person_ids()));
create policy event_share_view on person for select to anonymous
  using (id in (
    select person_id from event_registration
    where registration_status = 'active' and person_id is not null
      and event_id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[])
    union all
    select person_id from event_trainer
    where event_id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[])
  ));
--! EndIncluded policies/person.sql
--! Included policies/couple.sql
select app_private.drop_policies('public.couple');

CREATE POLICY admin_all ON couple TO administrator USING (true);
create policy view_visible_person on couple for select
using (man_id in (select app_private.visible_person_ids())
  or woman_id in (select app_private.visible_person_ids()));
create policy event_share_view on couple for select to anonymous
  using (id in (
    select couple_id from event_registration
    where registration_status = 'active' and parent_registration_id is null and couple_id is not null
      and event_id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[])
  ));
--! EndIncluded policies/couple.sql
--! Included policies/access_event.sql
select app_private.drop_policies('public.access_event');

create policy current_tenant on access_event as restrictive
  using (tenant_id = current_tenant_id());
create policy admin_view on access_event for select to administrator
  using (true);
create policy my_view on access_event for select
  using (person_id = any ((select public.current_person_ids())::bigint[]));
--! EndIncluded policies/access_event.sql
--! Included policies/accounting_period.sql
select app_private.drop_policies('public.accounting_period');

create policy current_tenant on accounting_period as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_manage on accounting_period to administrator
  using (true);
create policy public_view on accounting_period for select
  using (true);
--! EndIncluded policies/accounting_period.sql
--! Included policies/cohort_group.sql
select app_private.drop_policies('public.cohort_group');

create policy current_tenant on cohort_group as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on cohort_group to administrator
  using (true);
create policy public_view on cohort_group for select
  using (true);
--! EndIncluded policies/cohort_group.sql
--! Included policies/cohort_membership.sql
select app_private.drop_policies('public.cohort_membership');

create policy my_tenant on cohort_membership as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on cohort_membership to administrator
  using (true);
create policy view_all on cohort_membership for select
  using (true);
--! EndIncluded policies/cohort_membership.sql
--! Included policies/cohort_subscription.sql
select app_private.drop_policies('public.cohort_subscription');

create policy current_tenant on cohort_subscription as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_manage on cohort_subscription to administrator
  using (true);
create policy member_view on cohort_subscription for select to member
  using (true);
--! EndIncluded policies/cohort_subscription.sql
--! Included policies/dokumenty.sql
select app_private.drop_policies('public.dokumenty');

create policy current_tenant on dokumenty as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on dokumenty to administrator
  using (true);
create policy member_view on dokumenty for select to member
  using (true);
--! EndIncluded policies/dokumenty.sql
--! Included policies/form_responses.sql
select app_private.drop_policies('public.form_responses');

create policy current_tenant on form_responses as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on form_responses to administrator
  using (true);
--! EndIncluded policies/form_responses.sql
--! Included policies/location.sql
select app_private.drop_policies('public.location');

create policy current_tenant on location as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on location to administrator
  using (true);
create policy public_view on location for select
  using (true);
--! EndIncluded policies/location.sql
--! Included policies/location_image.sql
select app_private.drop_policies('public.location_image');

create policy current_tenant on location_image as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on location_image to administrator
  using (true);
create policy public_view on location_image for select
  using (true);
--! EndIncluded policies/location_image.sql
--! Included policies/person_invitation.sql
select app_private.drop_policies('public.person_invitation');

create policy current_tenant on person_invitation as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on person_invitation to administrator
  using (true);
--! EndIncluded policies/person_invitation.sql
--! Included policies/scoreboard_manual_adjustment.sql
select app_private.drop_policies('public.scoreboard_manual_adjustment');

create policy current_tenant on scoreboard_manual_adjustment as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on scoreboard_manual_adjustment to administrator
  using (true);
create policy member_read on scoreboard_manual_adjustment for select
  using (true);
--! EndIncluded policies/scoreboard_manual_adjustment.sql
--! Included policies/security_event.sql
select app_private.drop_policies('public.security_event');

create policy current_tenant on security_event as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_view on security_event for select to administrator
  using (true);
create policy self_view on security_event for select to member
  using (user_id = (select current_user_id())
    or person_id = any((select current_person_ids())::bigint[]));
--! EndIncluded policies/security_event.sql
--! Included policies/tenant.sql
select app_private.drop_policies('public.tenant');

create policy system_admin_all on tenant to system_admin
  using (true);
create policy admin_all on tenant to administrator
  using (id = (select current_tenant_id()));
create policy public_view on tenant for select to anonymous
  using (id = (select current_tenant_id()));
--! EndIncluded policies/tenant.sql
--! Included policies/tenant_administrator.sql
select app_private.drop_policies('public.tenant_administrator');

create policy system_admin_all on tenant_administrator to system_admin
  using (true);
create policy admin_all on tenant_administrator to administrator
  using (tenant_id = (select current_tenant_id()));
create policy public_view on tenant_administrator for select
  using (true);
--! EndIncluded policies/tenant_administrator.sql
--! Included policies/tenant_membership.sql
select app_private.drop_policies('public.tenant_membership');

create policy system_admin_all on tenant_membership to system_admin
  using (true);
create policy admin_all on tenant_membership to administrator
  using (tenant_id = (select current_tenant_id()));
create policy view_visible_person on tenant_membership for select
  using (true);
--! EndIncluded policies/tenant_membership.sql
--! Included policies/tenant_settings.sql
select app_private.drop_policies('public.tenant_settings');

create policy system_admin_all on tenant_settings to system_admin
  using (true);
create policy admin_own on tenant_settings to administrator
  using (tenant_id = (select current_tenant_id()));
--! EndIncluded policies/tenant_settings.sql
--! Included policies/tenant_trainer.sql
select app_private.drop_policies('public.tenant_trainer');

create policy system_admin_all on tenant_trainer to system_admin
  using (true);
create policy admin_all on tenant_trainer to administrator
  using (tenant_id = (select current_tenant_id()));
create policy public_view on tenant_trainer for select
  using (true);
--! EndIncluded policies/tenant_trainer.sql
--! Included policies/aktuality.sql
select app_private.drop_policies('public.aktuality');

create policy current_tenant on aktuality as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on aktuality to administrator
  using (true);
create policy public_view on aktuality for select
  using (is_visible);

grant all on table aktuality to anonymous;
--! EndIncluded policies/aktuality.sql

-- Drop the old permission helper after its policies have been replaced.
drop function if exists app_private.can_trainer_edit_instance;

alter table otp_token
  add column if not exists intent text not null default 'password_reset',
  add column if not exists event_id bigint references event(id) on delete cascade;

alter table otp_token drop constraint if exists otp_token_intent_check;
alter table otp_token add constraint otp_token_intent_check check (
  (intent = 'password_reset' and event_id is null)
  or (intent = 'event_handoff' and event_id is not null)
);

--! Included functions/otp_login.sql
drop function if exists otp_login;

CREATE or replace FUNCTION otp_login(token uuid)
  RETURNS login_result
  LANGUAGE plpgsql
  STRICT SECURITY DEFINER
  SET search_path TO pg_catalog, public, pg_temp
  AS $$
declare
  v_token otp_token;
  usr users;
  jwt jwt_token;
begin
  update otp_token t set used_at = now()
  where t.access_token = token and t.used_at is null and t.expires_at > now()
    and t.user_id is not null
    and (
      t.intent = 'password_reset'
      or (t.intent = 'event_handoff' and t.tenant_id = current_tenant_id())
    )
  returning t.* into v_token;
  if not found then
    raise exception 'INVALID_CREDENTIALS' using errcode = '28P01';
  end if;
  select * into usr from users where id = v_token.user_id;

  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);

  update users set last_login = now() where id = usr.id;
  insert into security_event (user_id, kind, method)
  values (usr.id, 'login_succeeded', 'otp');
  return (usr, jwt);
end;
$$;

GRANT ALL ON FUNCTION otp_login TO anonymous;
--! EndIncluded functions/otp_login.sql

alter table federated.person
  add column if not exists birth_year integer,
  add column if not exists national_reference text;

comment on column federated.person.national_reference is '@behavior -*';

create index if not exists federated_person_wdsf_national_reference_idx
  on federated.person (national_reference) where federation = 'wdsf';

update federated.person p
set birth_year = nullif((c.content->>'yearOfBirth')::integer, 0),
    national_reference = nullif(c.content->>'nationalReference', '')
from crawler.frontier f
join crawler.json_response r on r.id = f.last_successful_response_id
join crawler.json_response_cache c using (content_hash)
where f.federation = 'wdsf' and f.kind = 'member'
  and p.federation = 'wdsf' and p.external_id::text = f.key
  and (p.birth_year, p.national_reference) is distinct from
    (nullif((c.content->>'yearOfBirth')::integer, 0), nullif(c.content->>'nationalReference', ''));

--! Included functions/person_federation_candidates.sql
drop function if exists person_federation_candidates;
drop function if exists federation_person_candidates;
drop type if exists person_federation_candidates_record;

create type person_federation_candidates_record as (
  id integer,
  name text,
  age_group text,
  similarity real,
  birth_year integer,
  csts_id integer,
  wdsf_id integer,
  is_strong_match boolean
);

comment on column person_federation_candidates_record.id is '@notNull';
comment on column person_federation_candidates_record.similarity is '@notNull';
comment on column person_federation_candidates_record.is_strong_match is '@notNull';

create function federation_person_candidates(
  first_name text, last_name text, federation text,
  gender gender_type default null, birth_date date default null,
  "limit" integer default 10, threshold real default 0.4
) returns setof person_federation_candidates_record
language sql stable as $$
  with input as materialized (
    select app_private.normalize_name(concat_ws(' ', trim($1), trim($2))) as name,
      app_private.normalize_name(trim($1)) as first_name,
      app_private.normalize_name(trim($2)) as last_name,
      extract(year from $5)::integer as birth_year,
      extract(year from current_date)::integer - extract(year from $5)::integer as age,
      case $4 when 'man' then 'male'::federated.gender
        when 'woman' then 'female'::federated.gender end as gender
  ), matches as (
    select fp.*, similarity(fp.search_name, i.name) as name_score,
      nullif(fp.gender, 'unknown') = i.gender as gender_match,
      coalesce(fp.birth_year, extract(year from fp.dob)::integer) = i.birth_year
        or (fp.birth_year is null and fp.dob is null
          and coalesce(g.age, g.person_age) @> i.age) as age_match
    from federated.person fp
    cross join input i
    left join federated.age_group g on g.code = fp.age_group_code
    where fp.federation = $3
      and nullif(i.first_name, '') is not null and nullif(i.last_name, '') is not null
      and fp.external_id between 1 and 2147483647
      and fp.search_name % i.name
      and similarity(fp.search_name, i.name)
        >= greatest(0.3::real, least(coalesce(threshold, 0.4), 1::real))
      -- Indexed word matching uses pg_trgm's default 0.6 threshold for each name part.
      and fp.search_name %> i.first_name
      and fp.search_name %> i.last_name
      and (i.gender is null or fp.gender is null or fp.gender = 'unknown' or fp.gender = i.gender)
      and (
        i.birth_year is null
        or coalesce(fp.birth_year, extract(year from fp.dob)::integer) = i.birth_year
        or (fp.birth_year is null and fp.dob is null and (
          coalesce(g.age, g.person_age) is null
          or coalesce(g.age, g.person_age) @> i.age
          -- Adult is also a broad label for people eligible for senior categories.
          or (g.code = 'Adult' and i.age >= lower(g.age))
        ))
      )
  ), candidates as (
    select * from matches
    order by gender_match desc nulls last, age_match desc nulls last,
      name_score desc, canonical_name, external_id
    limit greatest(0, least(coalesce($6, 10), 50))
  )
  select fp.external_id::integer, fp.canonical_name, fp.age_group_code, fp.name_score,
    coalesce(fp.birth_year, extract(year from fp.dob)::integer),
    case fp.federation when 'csts' then fp.external_id::integer else linked.external_id end,
    case fp.federation when 'wdsf' then fp.external_id::integer else linked.external_id end,
    fp.name_score = 1 and fp.gender_match is true and fp.age_match is true
      and not exists (
        select 1 from matches other
        where other.external_id <> fp.external_id and other.name_score = 1
      )
  from candidates fp
  left join lateral (
    select min(other.external_id)::integer as external_id
    from federated.person other
    where (
      (fp.federation = 'wdsf' and other.federation = 'csts'
        and other.external_id::text = fp.national_reference)
      or (fp.federation = 'csts' and other.federation = 'wdsf'
        and other.national_reference = fp.external_id::text)
    )
      and other.external_id between 1 and 2147483647
      and similarity(other.search_name, fp.search_name) >= 0.8
      and (other.gender is null or other.gender = 'unknown'
        or fp.gender is null or fp.gender = 'unknown' or other.gender = fp.gender)
      and (coalesce(other.birth_year, extract(year from other.dob)::integer) is null
        or coalesce(fp.birth_year, extract(year from fp.dob)::integer) is null
        or coalesce(other.birth_year, extract(year from other.dob)::integer)
          = coalesce(fp.birth_year, extract(year from fp.dob)::integer))
    having count(*) = 1
  ) linked on true
  order by fp.gender_match desc nulls last, fp.age_match desc nulls last,
    fp.name_score desc, fp.canonical_name, fp.external_id;
$$;

comment on function federation_person_candidates is '@simpleCollections only
@notNull';
grant execute on function federation_person_candidates to anonymous;

create function person_federation_candidates(
  in_person person, federation text, "limit" integer default 10, threshold real default 0.4
) returns setof person_federation_candidates_record
language sql stable as $$
  select * from federation_person_candidates(
    in_person.first_name, in_person.last_name, $2, in_person.gender, in_person.birth_date, $3, $4
  );
$$;

comment on function person_federation_candidates is '@simpleCollections only
@notNull';
grant execute on function person_federation_candidates to anonymous;
--! EndIncluded functions/person_federation_candidates.sql

alter table federated.competitor
  add column if not exists latest_result_at date,
  add column if not exists latest_entry_at date;

--! Included functions/refresh_competitor_activity.sql
create or replace function app_private.refresh_competitor_activity(competitor_ids text[])
returns void language plpgsql as $$
begin
  if coalesce(cardinality(competitor_ids), 0) = 0 then
    return;
  end if;

  perform c.id from federated.competitor c where c.id = any(competitor_ids) order by c.id for no key update;

  with activity as (
    select c.id,
      (select max(comp.start_date)
       from federated.competition_result r
       join federated.competition comp on comp.id = r.competition_id
       where r.competitor_id = c.id) as latest_result_at,
      (select max(comp.start_date)
       from federated.competition_entry e
       join federated.competition comp on comp.id = e.competition_id
       where e.competitor_id = c.id and not e.cancelled
         and not exists (
           select 1 from federated.competition_result r
           where (r.competition_id, r.competitor_id) = (e.competition_id, e.competitor_id)
         )) as latest_entry_at
    from federated.competitor c
    where c.id = any(competitor_ids)
  )
  update federated.competitor c
  set latest_result_at = a.latest_result_at, latest_entry_at = a.latest_entry_at
  from activity a
  where c.id = a.id
    and (c.latest_result_at, c.latest_entry_at) is distinct from
      (a.latest_result_at, a.latest_entry_at);
end;
$$;

create or replace function app_private.tg_competitor_activity__refresh()
returns trigger language plpgsql as $$
-- @plpgsql_check_options: oldtable = oldtable, newtable = newtable
declare
  competitor_ids text[];
begin
  if TG_OP = 'INSERT' then
    select array_agg(distinct competitor_id) into competitor_ids from newtable;
  elsif TG_OP = 'DELETE' then
    select array_agg(distinct competitor_id) into competitor_ids from oldtable;
  else
    select array_agg(competitor_id) into competitor_ids
    from (select competitor_id from oldtable union select competitor_id from newtable) affected;
  end if;
  perform app_private.refresh_competitor_activity(competitor_ids);
  return null;
end;
$$;

create or replace trigger _900_refresh_competitor_activity_insert
  after insert on federated.competition_result
  referencing new table as newtable
  for each statement execute function app_private.tg_competitor_activity__refresh();
create or replace trigger _900_refresh_competitor_activity_update
  after update on federated.competition_result
  referencing old table as oldtable new table as newtable
  for each statement execute function app_private.tg_competitor_activity__refresh();
create or replace trigger _900_refresh_competitor_activity_delete
  after delete on federated.competition_result
  referencing old table as oldtable
  for each statement execute function app_private.tg_competitor_activity__refresh();

create or replace trigger _900_refresh_competitor_activity_insert
  after insert on federated.competition_entry
  referencing new table as newtable
  for each statement execute function app_private.tg_competitor_activity__refresh();
create or replace trigger _900_refresh_competitor_activity_update
  after update on federated.competition_entry
  referencing old table as oldtable new table as newtable
  for each statement execute function app_private.tg_competitor_activity__refresh();
create or replace trigger _900_refresh_competitor_activity_delete
  after delete on federated.competition_entry
  referencing old table as oldtable
  for each statement execute function app_private.tg_competitor_activity__refresh();

create or replace function app_private.tg_competition_dates__refresh_competitor_activity()
returns trigger language plpgsql as $$
declare
  competitor_ids text[];
begin
  with changed as (
    select n.id from newtable n join oldtable o using (id)
    where n.start_date is distinct from o.start_date
  ), affected as (
    select r.competitor_id from federated.competition_result r join changed c on c.id = r.competition_id
    union
    select e.competitor_id from federated.competition_entry e join changed c on c.id = e.competition_id
  )
  select array_agg(competitor_id) into competitor_ids from affected;
  perform app_private.refresh_competitor_activity(competitor_ids);
  return null;
end;
$$;

create or replace trigger _900_refresh_competitor_activity_dates
  after update on federated.competition
  referencing old table as oldtable new table as newtable
  for each statement execute function app_private.tg_competition_dates__refresh_competitor_activity();
--! EndIncluded functions/refresh_competitor_activity.sql

with results as (
  select r.competitor_id, max(c.start_date) as latest_result_at
  from federated.competition_result r
  join federated.competition c on c.id = r.competition_id
  group by r.competitor_id
), entries as (
  select e.competitor_id, max(c.start_date) as latest_entry_at
  from federated.competition_entry e
  join federated.competition c on c.id = e.competition_id
  where not e.cancelled and not exists (
    select 1 from federated.competition_result r
    where (r.competition_id, r.competitor_id) = (e.competition_id, e.competitor_id)
  )
  group by e.competitor_id
), activity as (
  select c.id, r.latest_result_at, e.latest_entry_at
  from federated.competitor c
  left join results r on r.competitor_id = c.id
  left join entries e on e.competitor_id = c.id
)
update federated.competitor c
set latest_result_at = a.latest_result_at, latest_entry_at = a.latest_entry_at
from activity a
where c.id = a.id
  and (c.latest_result_at, c.latest_entry_at) is distinct from
    (a.latest_result_at, a.latest_entry_at);

do $$
begin
  if not exists (select 1 from pg_catalog.pg_constraint where conrelid = 'public.file'::regclass and conname = 'file_tenant_id_id_key') then
    alter table file
      add constraint file_tenant_id_id_key unique using index file_tenant_id_id_idx;
  end if;
end;
$$;

comment on constraint file_tenant_id_id_key on file
  is '@behavior -query:resource:single';

--! Included functions/create_credit_transaction_for_person.sql
create or replace function create_credit_transaction_for_person(v_person_id bigint, v_description text, v_amount numeric(19, 4), v_currency text, v_date timestamp with time zone DEFAULT now()) returns transaction language sql as $$
  with txn as (
    insert into transaction (source, description, effective_date)
    values ('manual-credit', v_description, v_date) returning *
  ), posting as (
    insert into posting (transaction_id, account_id, amount)
    values ((select id from txn), (select id from person_account(V_person_id, v_currency)), v_amount)
  )
  select * from txn;
$$;

grant all on function create_credit_transaction_for_person to anonymous;
--! EndIncluded functions/create_credit_transaction_for_person.sql
--! Included functions/upsert_announcement.sql
drop function if exists upsert_announcement;
drop type if exists announcement_type_input;
drop type if exists announcement_audience_type_input;

create type announcement_type_input as (
  id bigint,
  title text,
  body text,
  is_sticky boolean,
  scheduled_since timestamptz,
  scheduled_until timestamptz,
  status announcement_status
);

create type announcement_audience_type_input as (
  id bigint,
  cohort_id bigint,
  audience_role announcement_audience_role
);

create or replace function upsert_announcement(
  info announcement_type_input,
  audiences announcement_audience_type_input[] default null,
  attachments bigint[] default null
)
  returns announcement language plpgsql
as $$
declare
  result announcement;
begin
  if info.id is null then
    insert into announcement (
      title, body, status, is_sticky, scheduled_since, scheduled_until
    ) values (
      info.title,
      info.body,
      coalesce(info.status, 'draft'),
      coalesce(info.is_sticky, false),
      info.scheduled_since,
      info.scheduled_until
    ) returning * into result;
  else
    select * into result from announcement where id = info.id;
    if not found then
      raise exception 'Announcement with id % not found', info.id;
    end if;
  end if;

  if attachments is not null then
    select coalesce(array_agg(id), '{}'::bigint[])
    into attachments
    from file
    where id = any(attachments)
      and tenant_id = result.tenant_id
      and uploaded_at is not null;

    delete from announcement_attachment
    where announcement_id = result.id
      and not inline
      and file_id <> all(attachments);

    insert into announcement_attachment (tenant_id, announcement_id, file_id, inline)
    select result.tenant_id, result.id, file_id, false
    from unnest(attachments) input(file_id)
    on conflict (tenant_id, announcement_id, file_id)
    do update set inline = false;
  end if;

  if info.id is not null then
    -- Make sure the update trigger re-populates inline references
    update announcement set
      title = info.title,
      body = info.body,
      status = coalesce(info.status, status),
      is_sticky = coalesce(info.is_sticky, false),
      scheduled_since = info.scheduled_since,
      scheduled_until = info.scheduled_until
    where id = info.id
    returning * into result;
  end if;

  if audiences is not null then
    with input as (
      select distinct (item).id, (item).cohort_id, (item).audience_role
      from unnest(audiences) item
    )
    delete from announcement_audience audience
    using input
    where audience.announcement_id = result.id
      and audience.id = input.id
      and input.id is not null
      and input.cohort_id is null
      and input.audience_role is null;

    with input as (
      select distinct (item).id, (item).cohort_id, (item).audience_role
      from unnest(audiences) item
    )
    update announcement_audience audience
    set cohort_id = input.cohort_id,
        audience_role = input.audience_role
    from input
    where audience.announcement_id = result.id
      and audience.id = input.id
      and input.id is not null
      and ((input.cohort_id is not null) <> (input.audience_role is not null))
      and (audience.cohort_id, audience.audience_role)
        is distinct from (input.cohort_id, input.audience_role);

    with input as (
      select distinct (item).cohort_id
      from unnest(audiences) item
      where (item).id is null
        and (item).cohort_id is not null
        and (item).audience_role is null
    )
    insert into announcement_audience (announcement_id, cohort_id)
    select result.id, input.cohort_id
    from input
    on conflict (announcement_id, cohort_id) do nothing;

    with input as (
      select distinct (item).audience_role
      from unnest(audiences) item
      where (item).id is null
        and (item).cohort_id is null
        and (item).audience_role is not null
    )
    insert into announcement_audience (announcement_id, audience_role)
    select result.id, input.audience_role
    from input
    on conflict (announcement_id, audience_role) do nothing;
  end if;

  return result;
end;
$$;

grant all on function upsert_announcement to anonymous;
--! EndIncluded functions/upsert_announcement.sql
--! Included functions/upsert_location.sql
drop function if exists upsert_location;
drop type if exists location_details_input;

create type location_details_input as (
  id bigint,
  name text,
  long_name text,
  description text,
  address address_domain,
  show_in_lists boolean,
  ordering integer,
  latitude double precision,
  longitude double precision
);

create or replace function upsert_location(
  details location_details_input,
  image_ids bigint[] default null,
  cover_image_id bigint default null
)
returns location
language plpgsql
as $$
declare
  result location;
begin
  if details.id is null then
    insert into location (
      name,
      long_name,
      description,
      address,
      show_in_lists,
      ordering,
      latitude,
      longitude,
      cover_image_id
    )
    values (
      details.name,
      nullif(nullif(btrim(details.long_name), ''), details.name),
      coalesce(details.description, ''),
      details.address,
      coalesce(details.show_in_lists, false),
      coalesce(details.ordering, 1),
      details.latitude,
      details.longitude,
      cover_image_id
    )
    returning * into result;
  else
    update location
    set name = details.name,
        long_name = nullif(nullif(btrim(details.long_name), ''), details.name),
        description = coalesce(details.description, ''),
        address = details.address,
        show_in_lists = coalesce(details.show_in_lists, location.show_in_lists),
        ordering = coalesce(details.ordering, location.ordering),
        latitude = details.latitude,
        longitude = details.longitude,
        cover_image_id = upsert_location.cover_image_id
    where id = details.id
    returning * into result;

    if not found then
      raise exception 'Location with id % not found', details.id;
    end if;
  end if;

  if image_ids is not null then
    select coalesce(array_agg(id), '{}'::bigint[])
    into image_ids
    from file
    where id = any(image_ids)
      and tenant_id = result.tenant_id
      and uploaded_at is not null
      and content_type like 'image/%';

    delete from location_image image
    where image.tenant_id = result.tenant_id
      and image.location_id = result.id
      and image.file_id <> all(image_ids);

    insert into location_image (tenant_id, location_id, file_id)
    select result.tenant_id, result.id, file_id
    from unnest(image_ids) input(file_id)
    on conflict do nothing;
  end if;

  return result;
end;
$$;

revoke all on function upsert_location from anonymous;
grant all on function upsert_location to administrator;
--! EndIncluded functions/upsert_location.sql
--! Included functions/confirm_membership_application.sql
drop function if exists confirm_membership_application(bigint);

create or replace function confirm_membership_application(
  application_id bigint,
  is_member boolean default true,
  is_trainer boolean default false,
  is_admin boolean default false,
  join_date timestamptz default now(),
  cohort_ids bigint[] default array[]::bigint[]
)
  returns person
  language sql
as $$
  with application as materialized (
    select *
    from membership_application
    where id = application_id and status = 'sent'
    for update
  ), t_person as (
    insert into person (
      first_name, last_name, gender, birth_date, nationality, tax_identification_number,
      national_id_number, csts_id, wdsf_id, prefix_title, suffix_title, bio, email, phone,
      note
    )
    select
      first_name, last_name, gender, birth_date, nationality, tax_identification_number,
      national_id_number, csts_id, wdsf_id,
      coalesce(prefix_title, ''), coalesce(suffix_title, ''), coalesce(bio, ''),
      email, phone, coalesce(note, '')
    from application
    returning *
  ), appl as (
    update membership_application
    set status = 'approved'
    where id = (select id from application)
  ), member as (
    insert into tenant_membership (tenant_id, person_id, since)
    select current_tenant_id(), id, join_date from t_person where is_member
  ), trainer as (
    insert into tenant_trainer (tenant_id, person_id, since)
    select current_tenant_id(), id, join_date from t_person where is_trainer
  ), administrator as (
    insert into tenant_administrator (tenant_id, person_id, since)
    select current_tenant_id(), id, join_date from t_person where is_admin
  ), cohorts as (
    insert into cohort_membership (cohort_id, person_id, since)
    select cohort_id, t_person.id, join_date
    from t_person
    cross join unnest(coalesce(cohort_ids, array[]::bigint[])) selected(cohort_id)
  ), proxy as (
    insert into user_proxy (person_id, user_id)
    select t_person.id, application.created_by
    from t_person cross join application
  )
  select * from t_person;
$$;

grant all on function confirm_membership_application to administrator;
--! EndIncluded functions/confirm_membership_application.sql
--! Included functions/create_person.sql
drop function if exists create_person;

create or replace function create_person(
  person_id bigint,
  inout p person,
  is_member boolean,
  is_trainer boolean,
  is_admin boolean,
  send_invitation boolean,
  join_date timestamptz
)
  returns person
  language plpgsql
as $$
begin
  if person_id is null then
    insert into person (
      first_name,
      last_name,
      gender,
      birth_date,
      nationality,
      tax_identification_number,
      national_id_number,
      csts_id,
      wdsf_id,
      prefix_title,
      suffix_title,
      bio,
      email,
      phone,
      external_ids,
      instagram_username,
      tiktok_username,
      facebook_url,
      website_url
    ) values (
      p.first_name,
      p.last_name,
      p.gender,
      p.birth_date,
      p.nationality,
      p.tax_identification_number,
      p.national_id_number,
      p.csts_id,
      p.wdsf_id,
      p.prefix_title,
      p.suffix_title,
      p.bio,
      p.email,
      p.phone,
      p.external_ids,
      p.instagram_username,
      p.tiktok_username,
      p.facebook_url,
      p.website_url
    ) returning * into p;
  else
    select * into p from person where person.id=person_id;
  end if;
  if is_member = true then
    insert into tenant_membership (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if is_trainer = true then
    insert into tenant_trainer (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if is_admin = true then
    insert into tenant_administrator (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if send_invitation = true and p.email is not null and p.email <> '' then
    insert into person_invitation (person_id, tenant_id, email) values (p.id, current_tenant_id(), p.email);
  end if;
end
$$;

grant all on function create_person to anonymous;
--! EndIncluded functions/create_person.sql
--! Included functions/update_attendance.sql
create or replace function update_attendance(eir_id bigint, status attendance_type, note text)
  returns event_registration
  language sql
as $$
  update event_registration set status = $2, attendance_note = $3 where id = $1 returning *;
$$;
--! EndIncluded functions/update_attendance.sql
--! Included functions/federation_people_facets.sql
create or replace function federated.federation_people_facets(
  federation federated.federation,
  search text default null,
  age_groups text[] default null,
  genders text[] default null,
  nationalities text[] default null
) returns table(facet text, value text, count integer)
language sql stable as $$
  with candidates as (
    select
      coalesce(p.age_group_code, nullif(p.age_group, ''), 'unknown') as age_group,
      coalesce(p.gender::text, 'unknown') as gender,
      coalesce(nullif(p.nationality, ''), 'unknown') as nationality
    from federated.person p
    where p.federation = federation.code
      and (nullif(trim(search), '') is null or p.external_id::text = trim(search) or p.id = trim(search)
        or p.search_name like '%' || replace(replace(replace(
          app_private.normalize_name(trim(search)), '!', '!!'), '%', '!%'), '_', '!_') || '%' escape '!')
  ), matches as (
    select *,
      coalesce(cardinality(age_groups), 0) = 0 or age_group = any(age_groups) as age_matches,
      coalesce(cardinality(genders), 0) = 0 or gender = any(genders) as gender_matches,
      coalesce(cardinality(nationalities), 0) = 0 or nationality = any(nationalities) as nationality_matches
    from candidates
  )
  select option.facet, option.value, count(*)::integer
  from matches m
  cross join lateral (values
    ('ageGroup', m.age_group, m.gender_matches and m.nationality_matches),
    ('gender', m.gender, m.age_matches and m.nationality_matches),
    ('nationality', m.nationality, m.age_matches and m.gender_matches)
  ) option(facet, value, matches)
  where option.matches
  group by option.facet, option.value
  order by option.facet, option.value;
$$;

comment on function federated.federation_people_facets is '@name federation_people_facets
@behavior +list -connection';
revoke all on function federated.federation_people_facets from public;
grant execute on function federated.federation_people_facets to anonymous;
--! EndIncluded functions/federation_people_facets.sql
--! Included functions/federation_competitor_facets.sql
create or replace function federated.federation_competitor_facets(
  federation federated.federation,
  search text default null,
  competitor_types federated.competitor_type[] default null,
  age_groups text[] default null,
  classes text[] default null,
  disciplines text[] default null,
  competed_since date default null
) returns table(facet text, value text, count integer)
language sql stable as $$
  with candidates as materialized (
    select c.id, c.competitor_type::text as competitor_type,
      cat.age_group_code, cat.class, cat.discipline,
      coalesce(cardinality(competitor_types), 0) = 0 or c.competitor_type = any(competitor_types) as type_matches,
      coalesce(cardinality(age_groups), 0) = 0 or cat.age_group_code = any(
        array(select federated.resolve_age_group(value) from unnest(age_groups) value)
      ) as age_matches,
      coalesce(cardinality(classes), 0) = 0 or cat.class = any(classes) as class_matches,
      coalesce(cardinality(disciplines), 0) = 0 or cat.discipline = any(disciplines) as discipline_matches
    from federated.competitor c
    left join federated.competitor_category_progress p on p.competitor_id = c.id
    left join federated.category cat on cat.id = p.category_id
    where c.federation = federation.code
      and (nullif(trim(search), '') is null or c.external_id::text = trim(search) or c.id = trim(search)
        or c.search_name like '%' || replace(replace(replace(
          app_private.normalize_name(trim(search)), '!', '!!'), '%', '!%'), '_', '!_') || '%' escape '!')
      and (competed_since is null or exists (
        select 1 from federated.competition_result r
        join federated.competition comp on comp.id = r.competition_id
        where r.competitor_id = c.id and comp.start_date >= competed_since and comp.start_date <= current_date
      ))
  )
  select option.facet, option.value, count(distinct c.id)::integer
  from candidates c
  cross join lateral (values
    ('type', c.competitor_type, c.age_matches and c.class_matches and c.discipline_matches),
    ('ageGroup', c.age_group_code, c.type_matches and c.class_matches and c.discipline_matches),
    ('class', c.class, c.type_matches and c.age_matches and c.discipline_matches),
    ('discipline', c.discipline, c.type_matches and c.age_matches and c.class_matches)
  ) option(facet, value, matches)
  where option.matches and option.value is not null
  group by option.facet, option.value
  order by option.facet, option.value;
$$;

comment on function federated.federation_competitor_facets is '@name federation_competitor_facets
@behavior +list -connection';
revoke all on function federated.federation_competitor_facets from public;
grant execute on function federated.federation_competitor_facets to anonymous;
--! EndIncluded functions/federation_competitor_facets.sql
--! Included functions/federation_event_facets.sql
drop function if exists federated.federation_event_facets;
drop type if exists federated.federation_event_facets;
drop type if exists federated.federation_event_year;

create type federated.federation_event_year as (
  year integer,
  event_count integer,
  mapped_event_count integer
);
create type federated.federation_event_facets as (
  years federated.federation_event_year[]
);

comment on column federated.federation_event_year.year is '@notNull';
comment on column federated.federation_event_year.event_count is '@notNull';
comment on column federated.federation_event_year.mapped_event_count is '@notNull';
comment on column federated.federation_event_facets.years is '@notNull';

create function federated.federation_event_facets(
  federation federated.federation, include_cancelled boolean default true
)
returns federated.federation_event_facets
language sql stable as $$
  select row(array(
    select row(
      extract(year from e.start_date)::integer,
      count(*) filter (where include_cancelled is distinct from false or e.status <> 'cancelled')::integer,
      count(*) filter (
        where (include_cancelled is distinct from false or e.status <> 'cancelled')
          and e.venue_lat is not null and e.venue_lng is not null
      )::integer
    )::federated.federation_event_year
    from federated.event e
    where e.federation = federation.code
    group by extract(year from e.start_date)
    order by extract(year from e.start_date) desc
  ))::federated.federation_event_facets;
$$;

revoke all on function federated.federation_event_facets from public;
grant execute on function federated.federation_event_facets to anonymous;

comment on type federated.federation_event_year is '@name federation_event_year';
comment on type federated.federation_event_facets is '@name federation_event_facets';
comment on function federated.federation_event_facets is '@name federation_event_facets';
--! EndIncluded functions/federation_event_facets.sql
--! Included views/competition_participation.sql
create or replace view federated.competition_participation
with (security_invoker = true) as
select competition_id, competitor_id from federated.competition_result
union all
select e.competition_id, e.competitor_id
from federated.competition_entry e
where not e.cancelled
  and not exists (
    select 1 from federated.competition_result r
    where (r.competition_id, r.competitor_id) = (e.competition_id, e.competitor_id)
  );

comment on view federated.competition_participation is '@name competition_participation
@primaryKey competition_id,competitor_id
@foreignKey (competition_id) references federated.competition (id)|@fieldName competition|@foreignFieldName participations
@foreignKey (competitor_id) references federated.competitor (id)|@fieldName competitor|@foreignFieldName competitionParticipations
@behavior -single -list -connection -insert -update -delete';
grant select on federated.competition_participation to anonymous;
--! EndIncluded views/competition_participation.sql

comment on table federated.age_group is '@name age_group
@behavior -insert -update -delete +list -connection';
comment on table federated.category is '@name category
@behavior -insert -update -delete +list -connection';
comment on table federated.federation is '@name federation
@behavior -insert -update -delete +list -connection';
comment on table federated.competition is '@name competition
@behavior -insert -update -delete -list';
comment on table federated.ranklist_entry is '@name ranklist_entry
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
comment on table federated.competition_entry is '@name competition_entry
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
comment on table federated.competition_official is '@name competition_official
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
comment on table federated.competition_result is '@name competition_result
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
comment on table federated.competition_round is '@name competition_round
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list';
comment on table federated.competition_round_judge is '@name competition_round_judge
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
comment on table federated.competition_round_result is '@name competition_round_result
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
comment on table federated.dance is '@name dance
@behavior -insert -update -delete +list -connection';
comment on table federated.dance_program is '@name dance_program
@behavior -insert -update -delete +list -connection';
comment on table federated.dance_program_dance is '@name dance_program_dance
@behavior -insert -update -delete +list -connection';
comment on table federated.federation_club is '@name federation_club
@behavior -insert -update -delete -list';
comment on table federated.judge_score is '@name judge_score
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
comment on table federated.ranklist is '@name ranklist
@behavior -insert -update -delete -list';
comment on table federated.ranklist_snapshot is '@name ranklist_snapshot
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';

comment on type federated.competitor_role is '@name competitor_role';
comment on type federated.competitor_type is '@name competitor_type';
comment on type federated.competition_type is '@name competition_type';
comment on type federated.gender is '@name gender';
comment on type federated.person_license_discipline is '@name person_license_discipline';
comment on type federated.person_license_kind is '@name person_license_kind';
comment on type federated.person_license_status is '@name person_license_status';
comment on type federated.official_role is '@name official_role';
comment on type federated.score_component is '@name score_component';
comment on type federated.scoring_method is '@name scoring_method';

comment on column federated.event_official.role is '@behavior +attribute:orderBy';
comment on column federated.competition_official.role is '@behavior +attribute:orderBy';
comment on column federated.person_license.kind is '@behavior +attribute:orderBy';
comment on column federated.person_license.discipline is '@behavior +attribute:orderBy';
comment on column federated.judge_score.component is '@behavior +attribute:orderBy';
comment on column federated.competitor_component.role is '@behavior +attribute:orderBy';

comment on type event_registration_status is null;
comment on function set_event_sharing is null;
comment on function update_tenant_settings_key is null;
comment on function confirm_membership_application is null;
comment on function update_attendance is null;
comment on function upsert_announcement is null;
comment on function upsert_article is null;
comment on function upsert_location is null;
comment on function create_person is null;
comment on function create_credit_transaction_for_person is null;

comment on column event_registration.event_id is null;
comment on column payment.event_id is null;

comment on column event_trainer.event_id is null;
comment on column event_trainer.lessons_offered is null;
comment on column event.series_id is null;
comment on column event_target_cohort.event_id is null;
comment on column app_private.system_admin_user.user_id is null;
comment on column app_private.system_admin_user.created_at is null;
comment on column app_private.system_admin_user.created_by is null;
comment on column federated.competition.competition_type is null;

--! Included views/person_last_activity.sql
-- Owner privileges allow summaries of private records without granting access to
-- account links, card codes, or invitations. Keep caller/tenant checks here.
create or replace view person_last_activity
with (security_barrier = false) as
select
  (select current_tenant_id()) as tenant_id,
  p.id as person_id,
  web.web_at,
  coalesce(card.card_entry_at, (
    select 'epoch'::timestamptz from access_credential c
    where c.person_id = p.id and c.tenant_id = (select current_tenant_id())
    limit 1
  )) as card_entry_at,
  training.training_at,
  competition.competition_on
from person p
left join lateral (
  select coalesce(max(u.last_active_at), 'epoch'::timestamptz) as web_at
  from user_proxy up
  join users u on u.id = up.user_id
  where up.person_id = p.id and up.status = 'active'
  having count(*) > 0
) web on true
left join lateral (
  select e.occurred_at as card_entry_at
  from access_event e
  where e.tenant_id = (select current_tenant_id())
    and e.person_id = p.id and e.allowed and e.occurred_at <= now()
  order by e.occurred_at desc
  limit 1
) card on true
left join lateral (
  select coalesce(max(e.since) filter (
    where (r.status = 'attended' or e.type = 'lesson')
      and not e.is_cancelled and e.since <= now()
  ), 'epoch'::timestamptz) as training_at
  from event_registration r
  join event e on e.id = r.event_id and e.tenant_id = r.tenant_id
  where r.person_id = p.id and r.tenant_id = (select current_tenant_id())
    and r.registration_status = 'active'
    and e.type in ('lesson', 'group', 'camp')
  having count(*) > 0
) training on true
left join lateral (
  select coalesce(max(case when c.latest_result_at > current_date then (
    select max(comp.start_date)
    from federated.competition_result r
    join federated.competition comp on comp.id = r.competition_id
    where r.competitor_id = c.id and comp.start_date <= current_date
  ) else c.latest_result_at end), 'epoch'::date) as competition_on
  from (values ('csts', p.csts_id::bigint), ('wdsf', p.wdsf_id::bigint)) l(federation, external_id)
  join federated.person fp on (fp.federation, fp.external_id) = (l.federation, l.external_id)
  left join federated.competitor_component cc on cc.person_id = fp.id
  left join federated.competitor c on c.id = cc.competitor_id
  where l.external_id <> 0
  having count(*) > 0
) competition on true
where (
    (select pg_has_role(current_user, 'trainer', 'member'))
    or p.id = any ((select current_person_ids())::bigint[])
  )
  and (
    exists (select from current_tenant_membership m where m.person_id = p.id)
    or exists (select from current_tenant_trainer t where t.person_id = p.id)
    or exists (select from current_tenant_administrator a where a.person_id = p.id)
    or exists (select from current_cohort_membership cm where cm.person_id = p.id)
  );

comment on view person_last_activity is '@primaryKey person_id
@unique tenant_id,person_id
@foreignKey (person_id) references public.person (id)|@foreignFieldName lastActivity
@behavior -query:resource:single -query:resource:list -query:resource:connection -insert -update -delete';
comment on column person_last_activity.tenant_id is '@notNull';
comment on column person_last_activity.person_id is '@notNull';

revoke all on person_last_activity from public;
grant select on person_last_activity to anonymous;
--! EndIncluded views/person_last_activity.sql

comment on table event_target_cohort is '@omit create,update,delete
@behavior -query:resource:list -query:resource:connection -query:resource:single
@simpleCollections only';

comment on constraint event_target_cohort_event_id_fkey on event_target_cohort is '@fieldName event
@foreignFieldName targetCohorts';

comment on table event_trainer is '@omit create,update,delete
@behavior -query:resource:list -query:resource:connection -query:resource:single
@simpleCollections only';

comment on constraint event_trainer_tenant_id_event_id_fkey on event_trainer is '@fieldName event
@foreignFieldName eventTrainers';

--! Included functions/event_trainer_name.sql
drop function if exists event_instance_trainer_name;

create or replace function event_trainer_name(trainer event_trainer)
  returns text
  language sql stable
begin atomic
  select concat_ws(' ', p.prefix_title, p.first_name, p.last_name)
    || case p.suffix_title when '' then '' else ', ' || p.suffix_title end
  from public.person p
  where p.id = trainer.person_id;
end;

grant execute on function event_trainer_name to anonymous;
--! EndIncluded functions/event_trainer_name.sql
--! Included functions/event_trainer_tenant_trainer.sql
drop function if exists event_instance_trainer_tenant_trainer;

create or replace function event_trainer_tenant_trainer(trainer event_trainer)
  returns tenant_trainer
  language sql stable
as $$
  select tt.*
  from event e
  join tenant_trainer tt on tt.tenant_id = e.tenant_id
    and tt.person_id = trainer.person_id
    and tt.active_range @> e.since
  where e.id = trainer.event_id and e.tenant_id = trainer.tenant_id;
$$;

comment on function event_trainer_tenant_trainer is '@behavior -queryField';
grant execute on function event_trainer_tenant_trainer to anonymous;
--! EndIncluded functions/event_trainer_tenant_trainer.sql

drop view if exists tenant_location_image;
drop view if exists tenant_location;

comment on table access_event is '@omit create,update,delete
@simpleCollections only';

comment on table event is '@omit create
@simpleCollections only';

comment on constraint access_event_location_fkey on access_event is '@fieldName location
@foreignFieldName accessEvents';

comment on constraint event_location_fkey on event is '@fieldName location
@foreignFieldName events';

comment on table event_lesson_request is '@omit create,update,delete
@behavior -query:resource:list -query:resource:connection -query:resource:single
@simpleCollections only';

comment on constraint event_lesson_request_registration_id_fkey on event_lesson_request is '@foreignFieldName lessonRequests';
comment on constraint event_lesson_request_trainer_id_fkey on event_lesson_request is '@foreignFieldName lessonRequests';
comment on constraint event_lesson_request_tenant_id_fkey on event_lesson_request is '@foreignFieldName lessonRequests';

comment on table event_registration is '@omit create,update,delete
@simpleCollections both';

comment on constraint event_registration_event_id_fkey on event_registration is '@fieldName event
@foreignFieldName registrations';

comment on constraint event_registration_parent_registration_id_fkey on event_registration is '@fieldName parentRegistration
@foreignFieldName childRegistrations';

comment on constraint event_parent_id_fkey on event is '@fieldName parent
@foreignFieldName childEvents';

comment on constraint payment_event_id_fkey on payment is '@fieldName event
@foreignFieldName payments';

--! Included functions/activity_timeline.sql
drop function if exists activity_timeline;
drop view if exists activity_timeline_item;

create or replace view activity_timeline_item as
select
  null::text as id,
  null::activity_timeline_kind as kind,
  null::timestamptz as sort_at,
  null::date as activity_date,
  null::bigint as person_id,
  null::text as person_name,
  null::bigint as event_attendance_id,
  null::bigint as event_id,
  null::text as federation,
  null::text as federated_person_id,
  null::text as competitor_id,
  null::text as competitor_name,
  null::federated.competitor_type as competitor_type,
  null::bigint as competition_event_id,
  null::text as competition_event_name,
  null::text as competition_event_location,
  null::bigint as competition_id,
  null::date as competition_date,
  null::time as check_in_end,
  null::federated.category as category,
  null::text[] as dances,
  null::integer as participants,
  null::integer as ranking,
  null::integer as ranking_to,
  null::numeric(10, 3) as point_gain,
  null::boolean as is_final,
  null::federated.competition_type as competition_type,
  null::text as competition_event_external_id,
  null::text as competition_external_id
where false;

comment on view activity_timeline_item is $$
@primaryKey id
@interface mode:single type:kind
@type EVENT_ATTENDANCE name:ActivityEventAttendance attributes:event_attendance_id,event_id
@type COMPETITION_BRIEF name:ActivityCompetitionBrief attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,check_in_end,category,dances,participants,competition_type,competition_event_external_id,competition_external_id
@type COMPETITION_RESULT name:ActivityCompetitionResult attributes:federation,federated_person_id,competitor_id,competitor_name,competitor_type,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,category,dances,participants,ranking,ranking_to,point_gain,is_final,competition_type,competition_event_external_id,competition_external_id
@type JUDGING name:ActivityJudging attributes:federation,federated_person_id,competition_event_id,competition_event_name,competition_event_location,competition_id,competition_date,category,competition_type,competition_event_external_id,competition_external_id
@type BIRTHDAY name:ActivityBirthday
@foreignKey (person_id) references person (id)|@fieldName person|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_attendance_id) references event_registration (id)|@fieldName eventAttendance|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@foreignKey (event_id) references event (id)|@fieldName event|@behavior -manyRelation:resource:list -manyRelation:resource:connection
@behavior -query:resource:list -query:resource:connection -query:resource:single
$$;

grant select on activity_timeline_item to anonymous;

create or replace function activity_timeline(p_since timestamp with time zone, p_until timestamp with time zone, p_person_ids bigint[] DEFAULT NULL::bigint[], p_cohort_id bigint DEFAULT NULL::bigint, p_kinds activity_timeline_kind[] DEFAULT NULL::activity_timeline_kind[], p_event_types event_type[] DEFAULT NULL::event_type[])
 returns setof activity_timeline_item
 language plpgsql
 stable
as $function$
declare
  include_event_attendance boolean;
  include_competition_brief boolean;
  include_competition_result boolean;
  include_judging boolean;
  include_birthday boolean;
begin
  if p_since is null or p_until is null or p_until <= p_since then
    return;
  end if;

  if cardinality(p_kinds) = 0 then
    return;
  end if;

  include_event_attendance =
    (p_kinds is null or 'EVENT_ATTENDANCE'::activity_timeline_kind = any(p_kinds))
    and (p_event_types is null or cardinality(p_event_types) > 0);
  include_competition_brief =
    p_kinds is null or 'COMPETITION_BRIEF'::activity_timeline_kind = any(p_kinds);
  include_competition_result =
    p_kinds is null or 'COMPETITION_RESULT'::activity_timeline_kind = any(p_kinds);
  include_judging =
    p_kinds is null or 'JUDGING'::activity_timeline_kind = any(p_kinds);
  include_birthday =
    p_kinds is null or 'BIRTHDAY'::activity_timeline_kind = any(p_kinds);

  if include_event_attendance then
    return query
      with scoped_people as (
        select distinct p.id, p.name
        from person p
        where (exists (select 1 from current_tenant_membership where person_id = p.id)
           or exists (select 1 from current_tenant_trainer where person_id = p.id)
           or exists (select 1 from current_tenant_administrator where person_id = p.id))
          and (p_person_ids is null or p.id = any(p_person_ids))
          and (p_cohort_id is null or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
      )
      select
        ('event_attendance:' || ea.id)::text as id,
        'EVENT_ATTENDANCE'::activity_timeline_kind as kind,
        ei.since as sort_at,
        ei.since::date as activity_date,
        ea.person_id,
        sp.name as person_name,
        ea.id as event_attendance_id,
        ei.id as event_id,
        null::text as federation,
        null::text as federated_person_id,
        null::text as competitor_id,
        null::text as competitor_name,
        null::federated.competitor_type as competitor_type,
        null::bigint as competition_event_id,
        null::text as competition_event_name,
        null::text as competition_event_location,
        null::bigint as competition_id,
        null::date as competition_date,
        null::time as check_in_end,
        null::federated.category as category,
        null::text[] as dances,
        null::integer as participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        null::federated.competition_type as competition_type,
        null::text as competition_event_external_id,
        null::text as competition_external_id
      from event_registration ea
      join event ei on ei.id = ea.event_id
      join scoped_people sp on sp.id = ea.person_id
      where ea.registration_status = 'active'
        and ei.since >= p_since
        and ei.since < p_until
        and (p_event_types is null or ei.type = any(p_event_types));
  end if;

  if include_birthday then
    return query
      with scoped_people as (
        select distinct p.id, p.name, p.birth_date
        from person p
        where (exists (select 1 from current_tenant_membership where person_id = p.id)
           or exists (select 1 from current_tenant_trainer where person_id = p.id)
           or exists (select 1 from current_tenant_administrator where person_id = p.id))
          and (p_person_ids is null or p.id = any(p_person_ids))
          and (p_cohort_id is null or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
          and p.birth_date is not null
      ),
      birthdays as (
        select
          sp.id as person_id,
          sp.name as person_name,
          make_date(
            years.year,
            extract(month from sp.birth_date)::int,
            least(
              extract(day from sp.birth_date)::int,
              extract(day from (
                make_date(years.year, extract(month from sp.birth_date)::int, 1)
                + interval '1 month - 1 day'
              ))::int
            )
          ) as birthday_date
        from scoped_people sp
        cross join generate_series(extract(year from p_since)::int, extract(year from p_until)::int) as years(year)
      )
      select
        ('birthday:' || b.person_id || ':' || b.birthday_date)::text as id,
        'BIRTHDAY'::activity_timeline_kind as kind,
        ((b.birthday_date + time '12:00')::timestamp)::timestamptz as sort_at,
        b.birthday_date as activity_date,
        b.person_id,
        b.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_id,
        null::text as federation,
        null::text as federated_person_id,
        null::text as competitor_id,
        null::text as competitor_name,
        null::federated.competitor_type as competitor_type,
        null::bigint as competition_event_id,
        null::text as competition_event_name,
        null::text as competition_event_location,
        null::bigint as competition_id,
        null::date as competition_date,
        null::time as check_in_end,
        null::federated.category as category,
        null::text[] as dances,
        null::integer as participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        null::federated.competition_type as competition_type,
        null::text as competition_event_external_id,
        null::text as competition_external_id
      from birthdays b
      join person p on p.id = b.person_id
      where b.birthday_date >= p.birth_date
        and ((b.birthday_date + time '12:00')::timestamp)::timestamptz >= p_since
        and ((b.birthday_date + time '12:00')::timestamp)::timestamptz < p_until;
  end if;

  if include_competition_result then
    return query
      select
        (
          'competition_result:' ||
          coalesce(cr.competition_id::text, '') || ':' ||
          coalesce(cr.competitor_id, '') || ':' ||
          coalesce(cr.person_id::text, '') || ':' ||
          coalesce((cr.category).id::text, '')
        )::text as id,
        'COMPETITION_RESULT'::activity_timeline_kind as kind,
        ((cr.competition_date + time '12:00')::timestamp)::timestamptz as sort_at,
        cr.competition_date as activity_date,
        cr.person_id,
        cr.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_id,
        cr.federation,
        cr.federated_person_id,
        cr.competitor_id,
        cr.competitor_name,
        cr.competitor_type,
        cr.event_id as competition_event_id,
        cr.event_name as competition_event_name,
        cr.event_location as competition_event_location,
        cr.competition_id,
        cr.competition_date,
        null::time as check_in_end,
        cr.category,
        cr.dances,
        cr.participants,
        cr.ranking,
        cr.ranking_to,
        cr.point_gain,
        cr.is_final,
        cr.competition_type,
        cr.event_external_id as competition_event_external_id,
        cr.competition_external_id
      from (
        select * from competition_report(p_since::date, p_until::date, p_cohort_id, p_person_ids)
      ) as cr
      where cr.competition_date is not null
        and ((cr.competition_date + time '12:00')::timestamp)::timestamptz >= p_since
        and ((cr.competition_date + time '12:00')::timestamp)::timestamptz < p_until;
  end if;

  if include_competition_brief then
    return query
      with reports as (
        select
          cr.person_id,
          cr.competition_id,
          cr.competitor_id,
          (cr.category).id as category_id
        from (
          select * from competition_report(p_since::date, p_until::date, p_cohort_id, p_person_ids)
        ) as cr
        where include_competition_result
      )
      select
        (
          'competition_brief:' ||
          coalesce(cb.competition_id::text, '') || ':' ||
          coalesce(cb.competitor_id, '') || ':' ||
          coalesce(cb.person_id::text, '') || ':' ||
          coalesce((cb.category).id::text, '')
        )::text as id,
        'COMPETITION_BRIEF'::activity_timeline_kind as kind,
        ((cb.competition_date + coalesce(cb.check_in_end, time '12:00'))::timestamp)::timestamptz as sort_at,
        cb.competition_date as activity_date,
        cb.person_id,
        cb.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_id,
        cb.federation,
        cb.federated_person_id,
        cb.competitor_id,
        cb.competitor_name,
        cb.competitor_type,
        cb.event_id as competition_event_id,
        cb.event_name as competition_event_name,
        cb.event_location as competition_event_location,
        cb.competition_id,
        cb.competition_date,
        cb.check_in_end,
        cb.category,
        cb.dances,
        cb.participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        cb.competition_type,
        cb.event_external_id as competition_event_external_id,
        cb.competition_external_id
      from (
        select * from competition_brief(p_since::date, p_until::date, p_cohort_id, p_person_ids)
      ) as cb
      where cb.competition_date is not null
        and ((cb.competition_date + coalesce(cb.check_in_end, time '12:00'))::timestamp)::timestamptz >= p_since
        and ((cb.competition_date + coalesce(cb.check_in_end, time '12:00'))::timestamp)::timestamptz < p_until
        and not exists (
          select 1
          from reports r
          where r.person_id is not distinct from cb.person_id
            and r.competition_id is not distinct from cb.competition_id
            and r.competitor_id is not distinct from cb.competitor_id
            and r.category_id is not distinct from (cb.category).id
        );
  end if;

  if include_judging then
    return query
      with scoped_people as (
        select p.id, p.name
        from person p
        where (p.id = any(p_person_ids)
           or (p_person_ids is null and (
             exists (select 1 from current_tenant_membership where person_id = p.id)
             or exists (select 1 from current_tenant_trainer where person_id = p.id)
             or exists (select 1 from current_tenant_administrator where person_id = p.id)
           )))
          and (p_cohort_id is null or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
      ),
      federated_people as (
        select
          sp.id as person_id,
          sp.name as person_name,
          fp.id as federated_person_id,
          fp.federation
        from scoped_people sp
        join person_federation_link link on link.person_id = sp.id
        join federated.person fp
          on (fp.federation, fp.external_id) = (link.federation, link.external_id)
      )
      select distinct
        ('judging:event:' || e.id || ':' || fp.person_id)::text as id,
        'JUDGING'::activity_timeline_kind as kind,
        ((e.start_date + time '12:00')::timestamp)::timestamptz as sort_at,
        e.start_date as activity_date,
        fp.person_id,
        fp.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_id,
        e.federation,
        fp.federated_person_id,
        null::text as competitor_id,
        null::text as competitor_name,
        null::federated.competitor_type as competitor_type,
        e.id as competition_event_id,
        e.name as competition_event_name,
        coalesce(e.location, e.city) as competition_event_location,
        null::bigint as competition_id,
        e.start_date as competition_date,
        null::time as check_in_end,
        null::federated.category as category,
        null::text[] as dances,
        null::integer as participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        null::federated.competition_type as competition_type,
        e.external_id as competition_event_external_id,
        null::text as competition_external_id
      from federated.event_official eo
      join federated.event e on e.id = eo.event_id
      join federated_people fp on fp.federated_person_id = eo.person_id
      where eo.role = 'adjudicator'
        and e.start_date >= current_date
        and ((e.start_date + time '12:00')::timestamp)::timestamptz >= p_since
        and ((e.start_date + time '12:00')::timestamp)::timestamptz < p_until
      union all
      select
        ('judging:competition:' || c.id || ':' || fp.person_id)::text as id,
        'JUDGING'::activity_timeline_kind as kind,
        ((c.start_date + time '12:00')::timestamp)::timestamptz as sort_at,
        c.start_date as activity_date,
        fp.person_id,
        fp.person_name,
        null::bigint as event_attendance_id,
        null::bigint as event_id,
        c.federation,
        fp.federated_person_id,
        null::text as competitor_id,
        null::text as competitor_name,
        null::federated.competitor_type as competitor_type,
        e.id as competition_event_id,
        e.name as competition_event_name,
        coalesce(e.location, e.city) as competition_event_location,
        c.id as competition_id,
        c.start_date as competition_date,
        null::time as check_in_end,
        cat as category,
        null::text[] as dances,
        null::integer as participants,
        null::integer as ranking,
        null::integer as ranking_to,
        null::numeric(10, 3) as point_gain,
        null::boolean as is_final,
        c.competition_type,
        e.external_id as competition_event_external_id,
        c.external_id as competition_external_id
      from federated.competition_official co
      join federated.competition c on c.id = co.competition_id
      join federated.event e on e.id = c.event_id
      join federated.category cat on cat.id = c.category_id
      join federated_people fp on fp.federated_person_id = co.person_id
      where co.role = 'adjudicator'
        and c.start_date < current_date
        and ((c.start_date + time '12:00')::timestamp)::timestamptz >= p_since
        and ((c.start_date + time '12:00')::timestamp)::timestamptz < p_until;
  end if;
end;
$function$;

comment on function activity_timeline is '@behavior +queryField:resource:list -queryField:resource:connection';
grant all on function activity_timeline to anonymous;
--! EndIncluded functions/activity_timeline.sql
--! Included functions/event_overlaps_reports.sql
drop function if exists event_overlaps_attendee_report;
drop function if exists event_overlaps_trainer_report;
drop type if exists event_overlaps_conflict;
drop type if exists event_conflict;

create type event_conflict as (
  person_id bigint,
  person_name text,
  first_event_id bigint,
  first_event_name text,
  first_since timestamptz,
  first_until timestamptz,
  second_event_id bigint,
  second_event_name text,
  second_since timestamptz,
  second_until timestamptz,
  overlap_range tstzrange
);

comment on type event_conflict is E'
@foreignKey (person_id) references person (id)
@foreignKey (first_event_id) references event (id)|@fieldName firstEvent
@foreignKey (second_event_id) references event (id)|@fieldName secondEvent
';
comment on column event_conflict.person_id is '@notNull';
comment on column event_conflict.first_event_id is '@notNull';
comment on column event_conflict.second_event_id is '@notNull';
comment on column event_conflict.overlap_range is '@notNull';

create or replace function event_overlaps_attendee_report(p_since timestamptz, p_until timestamptz)
  returns setof event_conflict
  language sql stable
as $$
  with instances as (
    select
      ea.person_id,
      p.name as person_name,
      ei.id as event_id,
      ei.parent_id,
      ei.since,
      ei.until,
      ei.range,
      ei.name as event_name
    from event_registration ea
    join event ei on ei.id = ea.event_id
    join person p on p.id = ea.person_id
    where not ei.is_cancelled
      and ea.person_id is not null
      and ea.registration_status = 'active'
      and ei.range && tstzrange(coalesce(p_since, '-infinity'::timestamptz), coalesce(p_until, 'infinity'::timestamptz), '[]')
  )
  select
    i1.person_id,
    i1.person_name,
    i1.event_id as first_event_id,
    i1.event_name as first_event_name,
    i1.since as first_since,
    i1.until as first_until,
    i2.event_id as second_event_id,
    i2.event_name as second_event_name,
    i2.since as second_since,
    i2.until as second_until,
    tstzrange(greatest(i1.since, i2.since), least(i1.until, i2.until), '[]') as overlap_range
  from instances i1
  join instances i2 on i1.person_id = i2.person_id
    and i1.event_id < i2.event_id
    and i1.parent_id is distinct from i2.event_id
    and i2.parent_id is distinct from i1.event_id
    and i1.range && i2.range
    and greatest(i1.since, i2.since) < least(i1.until, i2.until);
$$;

comment on function event_overlaps_attendee_report is '@simpleCollections only';
grant all on function event_overlaps_attendee_report to anonymous;

create or replace function event_overlaps_trainer_report(p_since timestamptz, p_until timestamptz)
  returns setof event_conflict
  language sql stable
as $$
  with trainer_instances as (
    select
      eit.person_id,
      p.name as person_name,
      ei.id as event_id,
      ei.parent_id,
      ei.since,
      ei.until,
      ei.range,
      ei.name as event_name
    from event ei
    join event_trainer eit on ei.id = eit.event_id
    join person p on p.id = eit.person_id
    where not ei.is_cancelled
      and ei.range && tstzrange(coalesce(p_since, '-infinity'::timestamptz), coalesce(p_until, 'infinity'::timestamptz), '[]')
  )
  select
    ti1.person_id,
    ti1.person_name,
    ti1.event_id as first_event_id,
    ti1.event_name as first_event_name,
    ti1.since as first_since,
    ti1.until as first_until,
    ti2.event_id as second_event_id,
    ti2.event_name as second_event_name,
    ti2.since as second_since,
    ti2.until as second_until,
    tstzrange(greatest(ti1.since, ti2.since), least(ti1.until, ti2.until), '[]') as overlap_range
  from trainer_instances ti1
  join trainer_instances ti2 on ti1.person_id = ti2.person_id
    and ti1.event_id < ti2.event_id
    and ti1.parent_id is distinct from ti2.event_id
    and ti2.parent_id is distinct from ti1.event_id
    and ti1.range && ti2.range
    and greatest(ti1.since, ti2.since) < least(ti1.until, ti2.until);
$$;

comment on function event_overlaps_trainer_report is '@simpleCollections only';
grant all on function event_overlaps_trainer_report to anonymous;
--! EndIncluded functions/event_overlaps_reports.sql
--! Included functions/event_approx_price.sql
drop function if exists event_instance_approx_price;

create or replace function event_approx_price(v_instance event)
  returns table (amount numeric(19,4), currency text)
  language sql stable
as $$
  with stats as materialized (
    select
      count(*) as num_participants,
      extract(epoch from (v_instance.until - v_instance.since)) / 60.0 as duration
    from event_registration registration
    where
      v_instance.type = 'lesson'
      and registration.event_id = v_instance.id
      and registration.person_id is not null
      and registration.registration_status = 'active'
  )
  select
    sum(tt.member_price_45min_amount * s.duration / 45 / s.num_participants) as amount,
    tt.currency as currency
  from stats s
  join lateral event_trainers(v_instance) tt on true
  where
    s.num_participants > 0
    and s.duration > 0
    and tt.member_price_45min_amount is not null
    and tt.currency is not null
  group by tt.currency;
$$;

grant all on function event_approx_price to anonymous;
comment on function event_approx_price is '@simpleCollections only';
--! EndIncluded functions/event_approx_price.sql

drop function if exists event_instance_facets;
drop function if exists move_event_instance;

--! Included functions/event_instances_for_range.sql
do $$
begin
  create type event_instance_range_scope as enum ('all', 'top_level', 'mine', 'relevant');
exception when duplicate_object then null;
end
$$;

drop function if exists event_instances_for_range;

create or replace function event_instances_for_range(
  only_type event_type,
  start_range timestamptz,
  end_range timestamptz = null,
  trainer_ids bigint[] = null,
  participant_ids bigint[] = null,
  parent_id bigint = null,
  scope event_instance_range_scope = 'all',
  location_ids bigint[] = null,
  only_types event_type[] = null
) returns setof event as $$
  with mine as (
    select event_id from event_registration
    where person_id = any ((select current_person_ids())::bigint[]) and registration_status = 'active'
    union all
    select event_id from event_trainer
    where person_id = any ((select current_person_ids())::bigint[])
  )
  select i.*
  from event i
  where i.tenant_id = current_tenant_id()
    and (only_type is null or i.type = only_type)
    and (only_types is null or i.type = any(only_types))
    and case
      when $6 is not null then i.parent_id = $6
        and (scope <> 'mine' or i.id in (select event_id from mine))
      when scope = 'all' then true
      when scope = 'top_level' then i.parent_id is null
      when scope = 'mine' then i.id in (select event_id from mine)
      when scope = 'relevant' then i.parent_id is null
        or i.id in (select event_id from mine)
        or i.parent_id in (select event_id from mine)
    end
    and i.since < coalesce(end_range, 'infinity'::timestamptz)
    and i.until > start_range
    and (trainer_ids is null
      or exists (select 1 from event_trainer where event_id = i.id and person_id = any (trainer_ids)))
    and (participant_ids is null
      or exists (select 1 from event_registration where event_id = i.id and person_id = any (participant_ids) and registration_status = 'active'))
    and (location_ids is null or i.location_id = any (location_ids))
  order by i.since
  ;
$$ stable language sql;

comment on function event_instances_for_range is '@simpleCollections only';
grant all on function event_instances_for_range to anonymous;
--! EndIncluded functions/event_instances_for_range.sql
--! Included functions/event_facets.sql
create or replace function event_facets(
  start_range timestamptz,
  end_range timestamptz,
  only_types event_type[] default null,
  trainer_ids bigint[] default null,
  participant_ids bigint[] default null,
  parent_id bigint default null,
  scope event_instance_range_scope default 'all',
  location_ids bigint[] default null
) returns table(facet text, value text, count integer)
language sql stable as $$
  with candidates as (
    select i.id, i.type, i.location_id,
      array(select distinct person_id from event_trainer where event_id = i.id) as trainers,
      array(select distinct person_id from event_registration
        where event_id = i.id and registration_status = 'active') as participants
    from event_instances_for_range(null, start_range, end_range,
      parent_id => $6, scope => $7) i
  ), matches as (
    select *,
      only_types is null or type = any(only_types) as type_matches,
      trainer_ids is null or trainers && trainer_ids as trainer_matches,
      participant_ids is null or participants && participant_ids as participant_matches,
      location_ids is null or location_id = any(location_ids) as location_matches
    from candidates
  )
  select option.facet, option.value, count(distinct m.id)::integer
  from matches m
  cross join lateral (
    select 'type' as facet, upper(m.type::text) as value
    where m.trainer_matches and m.participant_matches and m.location_matches
    union all
    select 'trainer', unnest(m.trainers)::text
    where m.type_matches and m.participant_matches and m.location_matches
    union all
    select 'participant', unnest(m.participants)::text
    where m.type_matches and m.trainer_matches and m.location_matches
    union all
    select 'location', m.location_id::text
    where m.type_matches and m.trainer_matches and m.participant_matches and m.location_id is not null
  ) option
  group by option.facet, option.value
  order by option.facet, option.value;
$$;
comment on function event_facets is '@behavior +list -connection';
revoke all on function event_facets from public;
grant execute on function event_facets to anonymous;
--! EndIncluded functions/event_facets.sql
--! Included functions/move_event.sql
create or replace function move_event(
  id bigint,
  since timestamptz,
  until timestamptz,
  trainer_person_id bigint default null,
  location_id bigint default null,
  location_text text default null
) returns event
  language plpgsql
as $$
declare
  inst event;
begin
  update event
  set since = move_event.since,
      until = move_event.until,
      location_id = case
        when move_event.location_id is not null then move_event.location_id
        when move_event.location_text is not null then null
        else event.location_id
      end,
      location_text = case
        when move_event.location_id is not null then ''
        else coalesce(move_event.location_text, event.location_text)
      end
  where event.id = move_event.id
  returning * into inst;

  if not found then
    raise exception 'Event instance % not found', move_event.id;
  end if;

  if trainer_person_id is not null then
    update event_trainer
    set person_id = trainer_person_id
    where event_id = inst.id
      and 1 = (select count(*) from event_trainer where event_id = inst.id);

    if not found and not exists (select 1 from event_trainer where event_id = inst.id)
    then
      insert into event_trainer (event_id, person_id)
      values (inst.id, trainer_person_id);
    end if;
  end if;

  return (select instance from event instance where instance.id = inst.id);
end;
$$;

grant execute on function move_event to anonymous;
--! EndIncluded functions/move_event.sql
--! Included functions/trainer_group_attendance_completion.sql
drop function if exists trainer_group_attendance_completion;
drop type if exists trainer_group_attendance_completion;

create type trainer_group_attendance_completion as (
  person_id integer,
  total_events integer,
  filled_events integer,
  partially_filled_events integer,
  unfilled_events integer,
  filled_ratio double precision,
  total_attendances integer,
  pending_attendances integer
);

comment on type trainer_group_attendance_completion is '@foreignKey (person_id) references person (id)';

create or replace function trainer_group_attendance_completion(
  since timestamp with time zone default null,
  until timestamp with time zone default null
) returns setof trainer_group_attendance_completion
  language sql stable as $_$
  with filtered_instances as (
    select ei.id
    from event ei
    where ei.tenant_id = current_tenant_id()
      and not ei.is_cancelled
      and ei.type = 'group'
      and coalesce(ei.until, ei.since) < coalesce($2, now())
      and ($1 is null or coalesce(ei.since, ei.until) >= $1)
      and ($2 is null or coalesce(ei.until, ei.since) < $2)
  ),
  trainer_instances as (
    select effective_trainer.person_id, fi.id as event_id
    from filtered_instances fi
    join event instance on instance.id = fi.id
    cross join lateral app_private.event_trainers_at(instance, instance.since) effective_trainer
  ),
  attendance_stats as (
    select
      ti.person_id,
      ti.event_id,
      coalesce(stats.attendance_count, 0) as attendance_count,
      coalesce(stats.unknown_count, 0) as unknown_count
    from trainer_instances ti
    left join lateral (
      select
        count(*) as attendance_count,
        count(*) filter (where eir.status = 'unknown') as unknown_count
      from event_registration eir
      where eir.event_id = ti.event_id
        and eir.person_id is not null
        and eir.registration_status = 'active'
    ) stats on true
  ),
  per_trainer as (
    select
      person_id,
      count(*) as total_events,
      count(*) filter (where attendance_count > 0 and unknown_count = 0) as filled_events,
      count(*) filter (
        where attendance_count > 0
          and unknown_count > 0
          and unknown_count < attendance_count
      ) as partially_filled_events,
      count(*) filter (where attendance_count = 0 or unknown_count = attendance_count) as unfilled_events,
      coalesce(sum(attendance_count), 0) as total_attendances,
      coalesce(sum(unknown_count), 0) as pending_attendances
    from attendance_stats
    group by person_id
  )
  select
    person_id,
    total_events,
    filled_events,
    partially_filled_events,
    unfilled_events,
    case
      when total_events > 0 then (filled_events + partially_filled_events)::double precision / total_events
      else null
    end as filled_ratio,
    total_attendances,
    pending_attendances
  from per_trainer
  order by filled_ratio asc nulls last, person_id;
$_$;

comment on function trainer_group_attendance_completion is '@simpleCollections only';
grant all on function trainer_group_attendance_completion to anonymous;
--! EndIncluded functions/trainer_group_attendance_completion.sql
--! Included functions/event_registration_info.sql
drop function if exists event_instance_registration_info;
drop function if exists event_registration_info;
drop type if exists event_instance_registration_info;
drop type if exists event_registration_info;

create type event_registration_info as (
  registrations integer,
  people integer,
  remaining_capacity integer,
  my boolean
);

create or replace function event_registration_info(inst event)
  returns event_registration_info
  language sql stable security definer
  set search_path = public, pg_catalog, pg_temp
as $$
  select row(
    count(*) filter (where parent_registration_id is null)::integer,
    count(person_id)::integer,
    case
      when inst.capacity is null or inst.capacity <= 0 then null
      else inst.capacity
        - case inst.capacity_unit
            when 'people' then count(person_id)::integer
            when 'registrations' then count(*) filter (where parent_registration_id is null)::integer
          end
    end,
    count(*) filter (where person_id = any(current_person_ids())) > 0
  )::event_registration_info
  from event_registration
  where event_id = inst.id and registration_status = 'active';
$$;

grant all on function event_registration_info to anonymous;
--! EndIncluded functions/event_registration_info.sql
