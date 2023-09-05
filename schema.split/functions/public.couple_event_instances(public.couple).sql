CREATE FUNCTION public.couple_event_instances(p public.couple) RETURNS SETOF public.event_instance
    LANGUAGE sql STABLE
    AS $$
  select distinct event_instance.*
  from event_instance
  join event_registration on event_instance.event_id=event_registration.event_id
  where couple_id = p.id;
$$;

COMMENT ON FUNCTION public.couple_event_instances(p public.couple) IS '@simpleCollections only
@filterable
@sortable';

GRANT ALL ON FUNCTION public.couple_event_instances(p public.couple) TO anonymous;


