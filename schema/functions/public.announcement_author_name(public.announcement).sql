CREATE FUNCTION public.announcement_author_name(a public.announcement) RETURNS text
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select concat_ws(' ', nullif(u.u_jmeno, ''), nullif(u.u_prijmeni, ''))
  from announcement stored
  join users u on u.id = stored.author_id
  where stored.id = a.id;
$$;

GRANT ALL ON FUNCTION public.announcement_author_name(a public.announcement) TO anonymous;
