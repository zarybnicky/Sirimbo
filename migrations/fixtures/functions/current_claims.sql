create or replace function current_claims() returns jsonb
  language sql stable
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  select to_jsonb(app_private.create_jwt_token(users)) - 'exp'
  from users
  where id = nullif(current_setting('jwt.claims.user_id', true), '')::bigint;
$$;

grant all on function current_claims() to anonymous;
