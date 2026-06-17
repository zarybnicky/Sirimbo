CREATE FUNCTION public.quick_create_events(events public.quick_event_input[]) RETURNS SETOF public.event
    LANGUAGE plpgsql
    AS $$
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

GRANT ALL ON FUNCTION public.quick_create_events(events public.quick_event_input[]) TO anonymous;
