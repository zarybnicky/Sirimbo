create or replace function get_current_user(version_id text default null) returns users
language sql volatile security definer
set search_path = pg_catalog, public, pg_temp
as $$
  update users
  set
    last_active_at = now(),
    last_version = version_id
  where id = current_user_id()
  returning *;
$$;

grant all on function get_current_user to anonymous;
comment on function get_current_user is '@behavior -*';
