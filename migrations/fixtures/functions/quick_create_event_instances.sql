create or replace function quick_create_event_instances(
  events quick_event_input[],
  parent_id bigint default null
) returns setof event_instance
  language plpgsql
as $$
declare
  quick_event quick_event_input;
  created_instance event_instance;
begin
  foreach quick_event in array coalesce(events, '{}'::quick_event_input[]) loop
    insert into event_instance (
      parent_id, since, until, name, type, location_id, location_text,
      capacity, capacity_unit, is_visible, is_public, is_locked, enable_notes,
      description, summary
    ) values (
      parent_id, quick_event.since, quick_event.until, '',
      coalesce(quick_event.type, 'lesson'), quick_event.location_id,
      coalesce(quick_event.location_text, ''),
      case when coalesce(quick_event.type, 'lesson') = 'lesson' then 2 else 0 end,
      'people', true, false, false, false, '', ''
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

    insert into event_instance_trainer (instance_id, person_id)
    select distinct created_instance.id, trainer.person_id
    from unnest(quick_event.trainer_person_ids) trainer(person_id)
    where trainer.person_id is not null;

    return next created_instance;
  end loop;
end;
$$;

select verify_function('quick_create_event_instances');
grant execute on function quick_create_event_instances to anonymous;
