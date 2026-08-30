create or replace function refresh_jwt() returns jwt_token
  language sql
  stable
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  select app_private.create_jwt_token(users)
  from users
  where id = nullif(current_setting('jwt.claims.user_id', true), '')::integer;
$$;

grant all on function refresh_jwt to anonymous;
