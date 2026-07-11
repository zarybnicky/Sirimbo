alter table public.event_external_registration
  add column if not exists instance_id bigint;

select app_private.drop_policies('public.event_external_registration');

do $$
begin
  if exists (
    select 1
    from information_schema.columns
    where table_schema = 'public'
      and table_name = 'event_external_registration'
      and column_name = 'event_id'
  ) then
    if exists (
      select 1
      from public.event_external_registration external_registration
      where (
        select count(*)
        from public.event_instance instance
        where instance.event_id = external_registration.event_id
          and instance.parent_id is null
      ) <> 1
    ) then
      raise exception 'Cannot map every external registration to exactly one root event instance';
    end if;

    alter table public.event_external_registration disable trigger _100_timestamps;

    update public.event_external_registration external_registration
    set instance_id = (
      select instance.id
      from public.event_instance instance
      where instance.event_id = external_registration.event_id
        and instance.parent_id is null
    );

    alter table public.event_external_registration enable trigger _100_timestamps;
    alter table public.event_external_registration
      drop constraint if exists event_external_registration_event_id_fkey,
      drop column if exists event_id;
  end if;
end;
$$;

alter table public.event_external_registration
  alter column instance_id set not null;

do $$
begin
  if not exists (
    select 1 from pg_constraint
    where conname = 'event_external_registration_instance_id_fkey'
      and conrelid = 'public.event_external_registration'::regclass
  ) then
    alter table public.event_external_registration
      add constraint event_external_registration_instance_id_fkey
      foreign key (instance_id) references public.event_instance(id)
      on update cascade on delete cascade;
  end if;
end;
$$;

create index if not exists event_external_registration_instance_id_idx
  on public.event_external_registration (instance_id);

grant insert (instance_id) on public.event_external_registration to anonymous;

--! include functions/event_remaining_x.sql
--! include policies/event_external_registration.sql

-- Remove the unused legacy payment relation only after its dependent function is updated.
--! include functions/calculate_transaction_effective_date.sql

do $$
begin
  if exists (
    select 1
    from information_schema.columns
    where table_schema = 'public'
      and table_name = 'payment'
      and column_name = 'event_registration_id'
  ) then
    if exists (
      select 1 from public.payment where event_registration_id is not null
    ) then
      raise exception 'Cannot remove payment.event_registration_id while it contains data';
    end if;

    alter table public.payment
      drop constraint if exists payment_event_registration_id_fkey,
      drop column if exists event_registration_id;
  end if;
end;
$$;
