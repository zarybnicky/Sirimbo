CREATE FUNCTION public.event_registrants(e public.event) RETURNS SETOF public.person
    LANGUAGE sql STABLE
    AS $$
  select person.* from person where id in (
    select unnest(array[person_id, man_id, woman_id]) as id
    from event_registration left join couple on couple.id = couple_id
    where event_id=e.id
  ) order by last_name asc, first_name asc;
$$;

COMMENT ON FUNCTION public.event_registrants(e public.event) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.event_registrants(e public.event) TO anonymous;


