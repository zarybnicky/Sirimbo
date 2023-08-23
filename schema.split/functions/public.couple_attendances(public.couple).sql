CREATE FUNCTION public.couple_attendances(p public.couple) RETURNS SETOF public.event_attendance
    LANGUAGE sql STABLE
    AS $$
  select event_attendance.* from event_attendance
  where person_id = p.man_id or person_id = p.woman_id;
$$;

COMMENT ON FUNCTION public.couple_attendances(p public.couple) IS '@simpleCollections only
@filterable
@sortable';

GRANT ALL ON FUNCTION public.couple_attendances(p public.couple) TO anonymous;


