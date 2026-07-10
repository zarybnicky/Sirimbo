CREATE FUNCTION public.event_registration_event_lesson_demands_by_registration_id(registration public.event_registration) RETURNS SETOF public.event_lesson_demand
    LANGUAGE sql STABLE
    AS $$
  select demand.*
  from public.event_lesson_demand demand
  join public.event_instance_registration instance_registration
    on instance_registration.id = demand.registration_id
  where instance_registration.legacy_registration_id = registration.id;
$$;

COMMENT ON FUNCTION public.event_registration_event_lesson_demands_by_registration_id(registration public.event_registration) IS '@simpleCollections only
@filterable
@sortable';

GRANT ALL ON FUNCTION public.event_registration_event_lesson_demands_by_registration_id(registration public.event_registration) TO anonymous;
