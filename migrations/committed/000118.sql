--! Previous: sha1:d48f1a340f0c54b9c16199637b24feed118965da
--! Hash: sha1:b499247341eac6e73485b0eacc895be99fe08453

--! split: 1-current.sql
drop function if exists app_private.index_advisor;
drop function if exists app_private.calculate_transaction_effective_date;
drop function if exists filtered_people;

--! Included functions/tg_transaction__effective_date.sql
CREATE or replace FUNCTION app_private.tg_transaction__effective_date() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  if NEW.effective_date is null then
    NEW.effective_date = (select coalesce(
      (select since from payment join event_instance on event_instance_id = event_instance.id where NEW.payment_id = payment.id),
      (select due_at from payment where NEW.payment_id = payment.id),
      NEW.created_at
    ));
  end if;
  return NEW;
end;
$$;

CREATE or replace TRIGGER _300_effective_date
  BEFORE INSERT OR UPDATE ON transaction
  FOR EACH ROW
  EXECUTE PROCEDURE app_private.tg_transaction__effective_date();
--! EndIncluded functions/tg_transaction__effective_date.sql
--! Included functions/event_overlaps_reports.sql
drop function if exists event_overlaps_attendee_report;
drop function if exists event_overlaps_trainer_report;
drop type if exists event_overlaps_conflict;
drop type if exists event_conflict;

create type event_conflict as (
  person_id bigint,
  person_name text,
  first_instance_id bigint,
  first_event_name text,
  first_since timestamptz,
  first_until timestamptz,
  second_instance_id bigint,
  second_event_name text,
  second_since timestamptz,
  second_until timestamptz,
  overlap_range tstzrange
);

comment on type event_conflict is E'
@foreignKey (person_id) references person (id)
@foreignKey (first_instance_id) references event_instance (id)
@foreignKey (second_instance_id) references event_instance (id)
';
comment on column event_conflict.person_id is '@notNull';
comment on column event_conflict.first_instance_id is '@notNull';
comment on column event_conflict.second_instance_id is '@notNull';
comment on column event_conflict.overlap_range is '@notNull';

create or replace function event_overlaps_attendee_report(p_since timestamptz, p_until timestamptz)
  returns setof event_conflict
  language sql stable
as $$
  with instances as (
    select
      ea.person_id,
      p.name as person_name,
      ei.id as instance_id,
      ei.since,
      ei.until,
      ei.range,
      ei.name as event_name
    from event_instance_registration ea
    join event_instance ei on ei.id = ea.instance_id
    join person p on p.id = ea.person_id
    where not ei.is_cancelled
      and ea.person_id is not null
      and ea.registration_status = 'active'
      and ei.range && tstzrange(coalesce(p_since, '-infinity'::timestamptz), coalesce(p_until, 'infinity'::timestamptz), '[]')
  )
  select
    i1.person_id,
    i1.person_name,
    i1.instance_id as first_instance_id,
    i1.event_name as first_event_name,
    i1.since as first_since,
    i1.until as first_until,
    i2.instance_id as second_instance_id,
    i2.event_name as second_event_name,
    i2.since as second_since,
    i2.until as second_until,
    tstzrange(greatest(i1.since, i2.since), least(i1.until, i2.until), '[]') as overlap_range
  from instances i1
  join instances i2 on i1.person_id = i2.person_id
    and i1.instance_id < i2.instance_id
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
      ei.id as instance_id,
      ei.since,
      ei.until,
      ei.range,
      ei.name as event_name
    from event_instance ei
    join event_instance_trainer eit on ei.id = eit.instance_id
    join person p on p.id = eit.person_id
    where not ei.is_cancelled
      and ei.range && tstzrange(coalesce(p_since, '-infinity'::timestamptz), coalesce(p_until, 'infinity'::timestamptz), '[]')
  )
  select
    ti1.person_id,
    ti1.person_name,
    ti1.instance_id as first_instance_id,
    ti1.event_name as first_event_name,
    ti1.since as first_since,
    ti1.until as first_until,
    ti2.instance_id as second_instance_id,
    ti2.event_name as second_event_name,
    ti2.since as second_since,
    ti2.until as second_until,
    tstzrange(greatest(ti1.since, ti2.since), least(ti1.until, ti2.until), '[]') as overlap_range
  from trainer_instances ti1
  join trainer_instances ti2 on ti1.person_id = ti2.person_id
    and ti1.instance_id < ti2.instance_id
    and ti1.range && ti2.range
    and greatest(ti1.since, ti2.since) < least(ti1.until, ti2.until);
$$;

comment on function event_overlaps_trainer_report is '@simpleCollections only';
grant all on function event_overlaps_trainer_report to anonymous;
--! EndIncluded functions/event_overlaps_reports.sql

alter table event_instance
  add column if not exists has_public_details boolean not null default false,
  add column if not exists share_token text;

comment on column event_instance.share_token is '@omit';

do $$
begin
  if not exists (
    select 1
    from pg_catalog.pg_constraint
    where conrelid = 'event_instance'::regclass
      and conname = 'event_instance_public_details_require_public'
  ) then
    alter table event_instance
      add constraint event_instance_public_details_require_public
      check (not has_public_details or is_public is true);
  end if;
end
$$;

create unique index if not exists event_instance_share_token_key
  on event_instance (share_token)
  where share_token is not null;

drop function if exists event_instance_share_token;
drop function if exists set_event_sharing;
drop function if exists app_private.event_share_claims;
drop function if exists app_private.can_manage_event_instance;

--! Included ../fixtures/functions/quick_create_event_instances.sql
drop function if exists public.quick_create_event_instances;

create or replace function quick_create_event_instances(
  events quick_event_input[],
  parent_id bigint default null,
  p_is_visible boolean default true,
  p_is_public boolean default false,
  p_is_locked boolean default false,
  p_enable_notes boolean default false,
  p_series_id bigint default null,
  p_name text default null,
  p_capacity integer default null,
  p_capacity_unit event_capacity_unit default 'people',
  p_description text default '',
  p_summary text default '',
  p_files_legacy text default '',
  p_cohort_ids bigint[] default null,
  p_trainer_lessons_offered integer[] default null,
  p_copies quick_event_input[] default null,
  p_has_public_details boolean default false
) returns setof event_instance
  language plpgsql
as $$
declare
  quick_event quick_event_input;
  created_instance event_instance;
  v_series_id bigint := p_series_id;
  instances quick_event_input[] := coalesce(events, '{}'::quick_event_input[]);
begin
  if cardinality(coalesce(p_copies, '{}'::quick_event_input[])) > 0 then
    if p_series_id is not null then
      raise exception 'cannot copy into an existing event series';
    end if;
    if cardinality(instances) <> 1 then
      raise exception 'event copies require exactly one source event';
    end if;

    insert into event_series (name)
    values (p_name)
    returning id into v_series_id;

    instances := instances || p_copies;
  end if;

  foreach quick_event in array instances loop
    insert into event_instance (
      parent_id, series_id, since, until, name, type, location_id, location_text,
      capacity, capacity_unit, is_visible, is_public, has_public_details,
      is_locked, enable_notes,
      description, summary, files_legacy
    ) values (
      parent_id, v_series_id, quick_event.since, quick_event.until, p_name,
      coalesce(quick_event.type, 'lesson'), quick_event.location_id,
      coalesce(quick_event.location_text, ''),
      coalesce(p_capacity,
        case when coalesce(quick_event.type, 'lesson') = 'lesson' then 2 else 0 end),
      coalesce(p_capacity_unit, 'people'),
      p_is_visible, p_is_public, p_is_public and p_has_public_details,
      p_is_locked, p_enable_notes,
      coalesce(p_description, ''), coalesce(p_summary, ''), coalesce(p_files_legacy, '')
    )
    returning * into created_instance;

    with roots as (
      insert into event_instance_registration (
        instance_id, person_id, couple_id, source, status
      )
      select created_instance.id, registration.person_id, registration.couple_id,
        'manager',
        case when registration.person_id is not null then 'unknown'::attendance_type end
      from unnest(quick_event.registrations) registration
      returning id, couple_id
    )
    insert into event_instance_registration (
      instance_id, parent_registration_id, person_id, status
    )
    select created_instance.id, root.id, person.person_id, 'unknown'
    from roots root
    join couple couple on couple.id = root.couple_id
    cross join lateral unnest(array[couple.man_id, couple.woman_id]) person(person_id)
    where root.couple_id is not null;

    insert into event_instance_trainer (instance_id, person_id, lessons_offered)
    select distinct on (trainer.person_id)
      created_instance.id,
      trainer.person_id,
      case when p_trainer_lessons_offered is null then 0
        else p_trainer_lessons_offered[trainer.ordinality] end
    from unnest(quick_event.trainer_person_ids) with ordinality
      trainer(person_id, ordinality)
    where trainer.person_id is not null;

    insert into event_instance_target_cohort (tenant_id, instance_id, cohort_id)
    select distinct created_instance.tenant_id, created_instance.id, cohort_id
    from unnest(coalesce(p_cohort_ids, '{}'::bigint[])) cohort(cohort_id)
    where cohort_id is not null;

    return next created_instance;
  end loop;
end;
$$;

select verify_function('quick_create_event_instances');
grant execute on function quick_create_event_instances to anonymous;
--! EndIncluded ../fixtures/functions/quick_create_event_instances.sql
--! Included ../fixtures/functions/update_event_instance_details.sql
drop function if exists update_event_instance_details;

create or replace function update_event_instance_details(
  p_instance_id bigint,
  p_since timestamptz,
  p_until timestamptz,
  p_name text,
  p_type event_type,
  p_location_id bigint,
  p_location_text text,
  p_is_visible boolean,
  p_is_public boolean,
  p_is_cancelled boolean,
  p_trainer_person_ids bigint[] default null,
  p_registrations quick_event_registration_input[] default null,
  p_capacity integer default null,
  p_capacity_unit event_capacity_unit default null,
  p_is_locked boolean default null,
  p_trainer_lessons_offered integer[] default null,
  p_cohort_ids bigint[] default null,
  p_enable_notes boolean default null,
  p_copies quick_event_input[] default null,
  p_has_public_details boolean default null
) returns event_instance
  language plpgsql
as $$
declare
  updated_instance event_instance;
  v_series_id bigint;
begin
  if p_registrations is not null then
    perform registration.id
    from event_instance_registration registration
    where registration.instance_id = p_instance_id
    order by registration.id
    for update;
  end if;

  update event_instance
  set
    since = p_since,
    until = p_until,
    name = p_name,
    type = p_type,
    location_id = p_location_id,
    location_text = coalesce(p_location_text, ''),
    is_visible = coalesce(p_is_visible, is_visible),
    is_public = coalesce(p_is_public, is_public),
    has_public_details = case
      when coalesce(p_is_public, is_public) is true
        then coalesce(p_has_public_details, has_public_details)
      else false
    end,
    capacity = coalesce(p_capacity, capacity),
    capacity_unit = coalesce(p_capacity_unit, capacity_unit),
    is_locked = coalesce(p_is_locked, is_locked),
    enable_notes = coalesce(p_enable_notes, enable_notes),
    is_cancelled = p_is_cancelled
  where id = p_instance_id
  returning * into updated_instance;

  if not found then
    raise exception 'event instance % not found', p_instance_id;
  end if;

  if p_registrations is not null then
    with desired as (
      select distinct registration.person_id, registration.couple_id
      from unnest(p_registrations) registration
    ), roots as (
      select existing.id
      from event_instance_registration existing
      where existing.instance_id = p_instance_id
        and existing.parent_registration_id is null
        and not exists (
          select 1
          from desired
          where desired.person_id is not distinct from existing.person_id
            and desired.couple_id is not distinct from existing.couple_id
        )
    )
    update event_instance_registration registration
    set registration_status = 'cancelled',
        target_cohort_id = null,
        source = case when registration.id = roots.id
          then 'manager'::event_registration_source end
    from roots
    where registration.registration_status <> 'cancelled'
      and (
        registration.id = roots.id
        or registration.parent_registration_id = roots.id
      );

    with desired as (
      select distinct registration.person_id, registration.couple_id
      from unnest(p_registrations) registration
    ), roots as (
      select existing.id
      from event_instance_registration existing
      join desired
        on desired.person_id is not distinct from existing.person_id
        and desired.couple_id is not distinct from existing.couple_id
      where existing.instance_id = p_instance_id
        and existing.parent_registration_id is null
    )
    update event_instance_registration registration
    set registration_status = 'active',
        target_cohort_id = null,
        source = case when registration.id = roots.id
          then 'manager'::event_registration_source end
    from roots
    where registration.registration_status <> 'active'
      and (
        registration.id = roots.id
        or registration.parent_registration_id = roots.id
      );

    with desired as (
      select distinct registration.person_id, registration.couple_id
      from unnest(p_registrations) registration
    ), roots as (
      insert into event_instance_registration (
        instance_id, person_id, couple_id, source, status
      )
      select
        p_instance_id,
        desired.person_id,
        desired.couple_id,
        'manager',
        case when desired.person_id is not null then 'unknown'::attendance_type end
      from desired
      where not exists (
        select 1
        from event_instance_registration existing
        where existing.instance_id = p_instance_id
          and existing.parent_registration_id is null
          and existing.person_id is not distinct from desired.person_id
          and existing.couple_id is not distinct from desired.couple_id
      )
      returning id, couple_id
    )
    insert into event_instance_registration (
      instance_id, parent_registration_id, person_id, status
    )
    select p_instance_id, roots.id, person.person_id, 'unknown'
    from roots
    join couple couple on couple.id = roots.couple_id
    cross join lateral unnest(array[couple.man_id, couple.woman_id]) person(person_id);
  end if;

  if p_cohort_ids is not null then
    insert into event_instance_target_cohort (tenant_id, instance_id, cohort_id)
    select distinct updated_instance.tenant_id, p_instance_id, desired.cohort_id
    from unnest(p_cohort_ids) desired(cohort_id)
    where desired.cohort_id is not null
    on conflict (instance_id, cohort_id) do nothing;

    delete from event_instance_target_cohort target
    where target.instance_id = p_instance_id
      and not exists (
        select 1
        from unnest(p_cohort_ids) desired(cohort_id)
        where desired.cohort_id = target.cohort_id
      );
  end if;

  if p_trainer_person_ids is not null then
    if p_trainer_lessons_offered is not null
      and cardinality(p_trainer_lessons_offered) <> cardinality(p_trainer_person_ids) then
      raise exception 'trainer lesson offers must match trainers';
    end if;

    delete from event_instance_trainer
    where instance_id = p_instance_id
      and not exists (
        select 1 from unnest(p_trainer_person_ids) person(id)
        where person.id = event_instance_trainer.person_id
      );

    insert into event_instance_trainer (instance_id, person_id, lessons_offered)
    select distinct on (input.person_id)
      p_instance_id,
      input.person_id,
      case when p_trainer_lessons_offered is null then 0
        else input.lessons_offered end
    from (
      select p_trainer_person_ids[i] person_id,
        p_trainer_lessons_offered[i] lessons_offered
      from generate_subscripts(p_trainer_person_ids, 1) item(i)
    ) input
    where input.person_id is not null
    order by input.person_id
    on conflict (instance_id, person_id) do update
    set lessons_offered = case when p_trainer_lessons_offered is null
      then event_instance_trainer.lessons_offered
      else excluded.lessons_offered end;
  end if;

  if p_registrations is not null or p_cohort_ids is not null then
    select * into updated_instance
    from event_instance
    where id = p_instance_id;
  end if;

  if cardinality(coalesce(p_copies, '{}'::quick_event_input[])) > 0 then
    v_series_id := updated_instance.series_id;

    if v_series_id is null then
      insert into event_series (name)
      values (updated_instance.name)
      returning id into v_series_id;

      update event_instance
      set series_id = v_series_id
      where id = p_instance_id
      returning * into updated_instance;
    end if;

    perform quick_create_event_instances(
      events => p_copies,
      parent_id => updated_instance.parent_id,
      p_is_visible => updated_instance.is_visible,
      p_is_public => updated_instance.is_public,
      p_has_public_details => updated_instance.has_public_details,
      p_is_locked => updated_instance.is_locked,
      p_enable_notes => updated_instance.enable_notes,
      p_series_id => v_series_id,
      p_name => updated_instance.name,
      p_capacity => updated_instance.capacity,
      p_capacity_unit => updated_instance.capacity_unit,
      p_description => updated_instance.description,
      p_summary => updated_instance.summary,
      p_files_legacy => updated_instance.files_legacy,
      p_cohort_ids => p_cohort_ids,
      p_trainer_lessons_offered => p_trainer_lessons_offered
    );
  end if;

  return updated_instance;
end;
$$;

grant all on function update_event_instance_details to anonymous;
--! EndIncluded ../fixtures/functions/update_event_instance_details.sql
--! Included ../fixtures/functions/event_sharing.sql
create function event_instance_share_token(i event_instance)
  returns text
  language sql
  stable
as $$
  select share_token
  from event_instance
  where id = $1.id
    and (pg_has_role(current_user, 'administrator', 'member')
      or (pg_has_role(current_user, 'trainer', 'member')
      and app_private.can_trainer_edit_instance($1.id)))
$$;
grant execute on function event_instance_share_token to anonymous;

create function set_event_sharing(id bigint, p_enabled boolean)
  returns text
  language plpgsql
  volatile
as $$
declare
  v_share_token text;
begin
  update event_instance instance
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

create or replace function app_private.event_share_claims(p_tenant_id bigint, p_share text)
returns table (
  instance_ids bigint[],
  person_ids bigint[],
  couple_ids bigint[],
  cohort_ids bigint[]
)
language sql
stable
security definer
set search_path = pg_catalog, public, pg_temp
as $$
  with root as (
    select id
    from event_instance
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
    union all
    select id from event_instance where parent_id in (select id from root)
  ),
  trainers as (
    select person_id
    from event_instance_trainer
    where instance_id in (select id from events)
  ),
  registrations as (
    select person_id, couple_id
    from event_instance_registration
    where instance_id in (select id from events)
      and parent_registration_id is null
      and registration_status = 'active'
  ),
  targets as (
    select cohort_id
    from event_instance_target_cohort
    where instance_id in (select id from events)
  )
  select
    array(select id from events),
    array(
      select person_id from trainers
      union
      select person_id from registrations where person_id is not null
      union
      select couple.man_id
      from registrations join couple on couple.id = registrations.couple_id
      union
      select couple.woman_id
      from registrations join couple on couple.id = registrations.couple_id
    ),
    array(
      select distinct couple_id
      from registrations
      where couple_id is not null
    ),
    array(select distinct cohort_id from targets)
  where exists (select 1 from root);
$$;

revoke all on function app_private.event_share_claims from public;
--! EndIncluded ../fixtures/functions/event_sharing.sql
--! Included ../fixtures/policies/event_instance.sql
select app_private.drop_policies('public.event_instance');

create policy current_tenant on event_instance as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_same_tenant on event_instance to administrator using (true);
create policy trainer_select on event_instance for select to trainer
  using (app_private.can_trainer_edit_instance(id));
create policy trainer_insert on event_instance for insert to trainer
  with check (parent_id is null or app_private.can_trainer_edit_instance(parent_id));
create policy trainer_update on event_instance for update to trainer
  using (app_private.can_trainer_edit_instance(id));
create policy trainer_delete on event_instance for delete to trainer
  using (app_private.can_trainer_edit_instance(id));
create policy member_view on event_instance for select to member
  using (is_visible);
create policy public_view on event_instance for select to anonymous
  using (is_public);
create policy event_share_view on event_instance for select to anonymous
  using (id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[]));
--! EndIncluded ../fixtures/policies/event_instance.sql
--! Included ../fixtures/policies/event_instance_trainer.sql
select app_private.drop_policies('public.event_instance_trainer');

create policy current_tenant on event_instance_trainer as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_instance_trainer to administrator using (true);
create policy trainer_same_tenant on event_instance_trainer to trainer
  using (app_private.can_trainer_edit_instance(instance_id))
  with check (true);
create policy member_view on event_instance_trainer for select to member using (true);
create policy event_share_view on event_instance_trainer for select to anonymous
  using (instance_id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[]));
--! EndIncluded ../fixtures/policies/event_instance_trainer.sql
--! Included ../fixtures/policies/event_instance_target_cohort.sql
select app_private.drop_policies('public.event_instance_target_cohort');

create policy current_tenant on event_instance_target_cohort as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_instance_target_cohort to administrator using (true);
create policy trainer_select on event_instance_target_cohort to trainer
  using (app_private.can_trainer_edit_instance(instance_id));
create policy member_view on event_instance_target_cohort for select to member
  using (instance_id in (select id from event_instance));
create policy event_share_view on event_instance_target_cohort for select to anonymous
  using (instance_id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[]));
--! EndIncluded ../fixtures/policies/event_instance_target_cohort.sql
--! Included ../fixtures/policies/person.sql
select app_private.drop_policies('public.person');

create policy admin_all on person to administrator using (true);
create policy admin_myself on person for update
  using (id = any (current_person_ids()));
create policy view_tenant_or_trainer on person for select
  using (id in (select person_id from app_private.visible_person_ids() v(person_id)));
create policy event_share_view on person for select to anonymous
  using (id = any ((select current_setting('jwt.claims.shared.person_ids', true))::bigint[]));
--! EndIncluded ../fixtures/policies/person.sql
--! Included ../fixtures/policies/couple.sql
select app_private.drop_policies('public.couple');

CREATE POLICY admin_all ON couple TO administrator USING (true);
create policy view_visible_person on couple for select
using (man_id in (select person_id from app_private.visible_person_ids() v(person_id))
  or woman_id in (select person_id from app_private.visible_person_ids() v(person_id)));
create policy event_share_view on couple for select to anonymous
  using (id = any ((select current_setting('jwt.claims.shared.couple_ids', true))::bigint[]));
--! EndIncluded ../fixtures/policies/couple.sql
--! Included ../fixtures/policies/cohort.sql
select app_private.drop_policies('public.cohort');

create policy current_tenant on cohort as restrictive using (tenant_id = (select current_tenant_id()));
create policy admin_all on cohort to administrator using (true);
create policy public_view on cohort for select using (is_visible);
create policy event_share_view on cohort for select to anonymous
  using (id = any ((select current_setting('jwt.claims.shared.cohort_ids', true))::bigint[]));
--! EndIncluded ../fixtures/policies/cohort.sql
