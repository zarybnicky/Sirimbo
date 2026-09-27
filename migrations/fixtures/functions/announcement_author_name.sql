create or replace function announcement_author_name(a announcement) returns text
  language sql stable security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  select concat_ws(' ', nullif(u.u_jmeno, ''), nullif(u.u_prijmeni, ''))
  from announcement stored
  join users u on u.id = stored.author_id
  where stored.id = a.id;
$$;

grant execute on function announcement_author_name(announcement) to anonymous;
