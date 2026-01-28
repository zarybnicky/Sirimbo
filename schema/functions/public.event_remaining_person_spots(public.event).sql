CREATE FUNCTION public.event_remaining_person_spots(e public.event) RETURNS integer
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  select e.capacity - (
    select coalesce(sum(case when couple_id is not null then 2 else 1 end), 0)
    from event_registration where event_id = e.id
  ) - (
    select coalesce(count(id), 0)
    from event_external_registration where event_id = e.id
  );
$$;

GRANT ALL ON FUNCTION public.event_remaining_person_spots(e public.event) TO anonymous;
