--!include functions/event_overlaps_reports.sql
--!include functions/activity_timeline.sql
--!include functions/event_instances_for_range.sql

do $$
begin
  if not exists (
    select 1
    from pg_catalog.pg_type type
    join pg_catalog.pg_namespace namespace on namespace.oid = type.typnamespace
    where namespace.nspname = 'public' and type.typname = 'event_registration_input'
  ) then
    create type public.event_registration_input as (
      person_id bigint,
      couple_id bigint
    );
  end if;

  if not exists (
    select 1
    from pg_catalog.pg_type type
    join pg_catalog.pg_namespace namespace on namespace.oid = type.typnamespace
    where namespace.nspname = 'public' and type.typname = 'event_trainer_input'
  ) then
    create type public.event_trainer_input as (
      person_id bigint,
      lessons_offered integer
    );
  end if;

  if not exists (
    select 1
    from pg_catalog.pg_type type
    join pg_catalog.pg_namespace namespace on namespace.oid = type.typnamespace
    where namespace.nspname = 'public' and type.typname = 'event_details_input'
  ) then
    create type public.event_details_input as (
      parent_id bigint,
      name text,
      type public.event_type,
      location_id bigint,
      location_text text,
      capacity integer,
      capacity_unit public.event_capacity_unit,
      is_visible boolean,
      is_public boolean,
      has_public_details boolean,
      is_locked boolean,
      enable_notes boolean
    );
  end if;

  if not exists (
    select 1
    from pg_catalog.pg_type type
    join pg_catalog.pg_namespace namespace on namespace.oid = type.typnamespace
    where namespace.nspname = 'public' and type.typname = 'event_input'
  ) then
    create type public.event_input as (
      id bigint,
      since timestamptz,
      until timestamptz,
      is_cancelled boolean,
      registrations public.event_registration_input[]
    );
  end if;

  if not exists (
    select 1
    from pg_catalog.pg_type type
    join pg_catalog.pg_namespace namespace on namespace.oid = type.typnamespace
    where namespace.nspname = 'public' and type.typname = 'event_series_input'
  ) then
    create type public.event_series_input as (
      id bigint,
      name text
    );
  end if;
end
$$;

--!include functions/save_events.sql
