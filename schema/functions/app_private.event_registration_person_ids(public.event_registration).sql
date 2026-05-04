CREATE FUNCTION app_private.event_registration_person_ids(e public.event_registration) RETURNS SETOF bigint
    LANGUAGE sql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select e.person_id as id where e.person_id is not null
  union
  select unnest(array[man_id, woman_id]) as id from couple where couple.id = e.couple_id and e.couple_id is not null
$$;

GRANT ALL ON FUNCTION app_private.event_registration_person_ids(e public.event_registration) TO anonymous;
