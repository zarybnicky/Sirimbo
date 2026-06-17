drop function if exists public.filtered_people(boolean, boolean, bigint[], text);

comment on table public.dokumenty is '@omit';

comment on table public.cohort_membership is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.couple is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.event_instance is '@omit create,delete
@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.payment is '@omit create
@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.payment_debtor is '@omit create,update,delete
@simpleCollections only';

comment on table public.payment_recipient is '@omit create,update,delete
@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.person_invitation is '@omit update
@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.scoreboard_manual_adjustment is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.tenant is '@omit create,delete
@behavior -singularRelation:resource:single -query:resource:list -query:resource:connection
@simpleCollections only';

comment on table public.tenant_administrator is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.tenant_location is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.tenant_membership is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.tenant_settings is '@simpleCollections only
@omit create,delete
@behavior -query:resource:list -query:resource:connection';

comment on table public.tenant_trainer is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

comment on table public.user_proxy is '@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

do $$
begin
  if not exists (
    select 1
    from pg_catalog.pg_type t
    join pg_catalog.pg_namespace n on n.oid = t.typnamespace
    where n.nspname = 'public'
      and t.typname = 'quick_event_registration_input'
  ) then
    create type public.quick_event_registration_input as (
      person_id bigint,
      couple_id bigint
    );
  end if;

  if not exists (
    select 1
    from pg_catalog.pg_type t
    join pg_catalog.pg_namespace n on n.oid = t.typnamespace
    where n.nspname = 'public'
      and t.typname = 'quick_event_input'
  ) then
    create type public.quick_event_input as (
      since timestamptz,
      until timestamptz,
      type public.event_type,
      location_id bigint,
      location_text text,
      trainer_person_ids bigint[],
      registrations public.quick_event_registration_input[]
    );
  end if;
end
$$;

create or replace function public.quick_create_events(
  events public.quick_event_input[]
) returns setof public.event
  language plpgsql
as $$
declare
  quick_event public.quick_event_input;
  created_event public.event;
  created_ids bigint[] := array[]::bigint[];
begin
  foreach quick_event in array coalesce(events, '{}'::public.quick_event_input[]) loop
    insert into public.event (
      name,
      description,
      type,
      location_id,
      location_text,
      capacity,
      is_visible
    )
    values (
      '',
      '',
      coalesce(quick_event.type, 'lesson'::public.event_type),
      quick_event.location_id,
      coalesce(quick_event.location_text, ''),
      case when coalesce(quick_event.type, 'lesson'::public.event_type) = 'lesson' then 2 else 0 end,
      true
    )
    returning * into created_event;

    insert into public.event_instance (event_id, since, until, is_cancelled)
    values (created_event.id, quick_event.since, quick_event.until, false);

    insert into public.event_trainer (event_id, person_id, lessons_offered)
    select distinct created_event.id, input.person_id, 0
    from unnest(coalesce(quick_event.trainer_person_ids, '{}'::bigint[])) as input(person_id)
    where input.person_id is not null
    on conflict (event_id, person_id) do nothing;

    insert into public.event_registration (event_id, person_id, couple_id)
    select created_event.id, input.person_id, input.couple_id
    from unnest(
      coalesce(quick_event.registrations, '{}'::public.quick_event_registration_input[])
    ) as input(person_id, couple_id)
    on conflict (event_id, person_id, couple_id) do nothing;

    created_ids := created_ids || created_event.id;
  end loop;

  return query select * from public.event where id = any (created_ids) order by id;
end;
$$;

select verify_function('public.quick_create_events');

comment on function public.quick_create_events is null;
grant all on function public.quick_create_events to anonymous;

create or replace function public.update_event_instance_details(
  p_instance_id bigint,
  p_since timestamptz,
  p_until timestamptz,
  p_name text,
  p_type public.event_type,
  p_location_id bigint,
  p_location_text text,
  p_is_visible boolean,
  p_is_public boolean,
  p_is_cancelled boolean,
  p_trainer_person_ids bigint[] default null
) returns public.event_instance language plpgsql
as $$
declare
  updated_instance public.event_instance;
begin
  update public.event_instance
  set
    since = p_since,
    until = p_until,
    name = p_name,
    type = p_type,
    location_id = p_location_id,
    location_text = coalesce(p_location_text, ''),
    is_visible = coalesce(p_is_visible, is_visible),
    is_public = coalesce(p_is_public, is_public),
    is_cancelled = p_is_cancelled
  where id = p_instance_id
  returning * into updated_instance;

  if not found then
    raise exception 'event instance % not found', p_instance_id;
  end if;

  if p_trainer_person_ids is not null then
    delete from public.event_instance_trainer
    where instance_id = p_instance_id;

    insert into public.event_instance_trainer (instance_id, person_id)
    select distinct p_instance_id, input.person_id
    from unnest(p_trainer_person_ids) as input(person_id)
    where input.person_id is not null
    on conflict (instance_id, person_id) do nothing;
  end if;

  return updated_instance;
end;
$$;

select verify_function('public.update_event_instance_details');

comment on function public.update_event_instance_details is null;
grant all on function public.update_event_instance_details to anonymous;
