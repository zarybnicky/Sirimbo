CREATE FUNCTION public.event_instances_for_range(only_mine boolean, type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone DEFAULT NULL::timestamp with time zone) RETURNS SETOF public.event_instance
    LANGUAGE sql STABLE
    AS $$
  select event_instance.* from event_instance join event on event_id=event.id
  where event.is_visible = true and tstzrange(start_range, end_range, '[]') && range and (type is null or event.type = type)
  and case only_mine
    when false then true
    else exists (select 1 from event_registration where event_id=event.id and (person_id in (select my_person_ids()) or couple_id in (select my_couple_ids()))) end
  order by range asc;
$$;

COMMENT ON FUNCTION public.event_instances_for_range(only_mine boolean, type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.event_instances_for_range(only_mine boolean, type public.event_type, start_range timestamp with time zone, end_range timestamp with time zone) TO anonymous;


