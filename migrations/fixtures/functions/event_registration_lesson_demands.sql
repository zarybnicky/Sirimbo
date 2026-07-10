create or replace function public.event_registration_event_lesson_demands_by_registration_id(
  registration public.event_registration
) returns setof public.event_lesson_demand
  language sql stable
as $$
  select demand.*
  from public.event_lesson_demand demand
  join public.event_instance_registration instance_registration
    on instance_registration.id = demand.registration_id
  where instance_registration.legacy_registration_id = registration.id;
$$;

comment on function public.event_registration_event_lesson_demands_by_registration_id(public.event_registration)
  is E'@simpleCollections only\n@filterable\n@sortable';
grant execute on function public.event_registration_event_lesson_demands_by_registration_id(public.event_registration)
  to anonymous;
