
create or replace function get_current_user(version_id text default null) returns users
language sql volatile security definer as $$
  with updated_user as (
    update users
    set
      last_active_at = now(),
      last_version = version_id
    where id = nullif(current_setting('jwt.claims.user_id', true), '')::integer
    returning *
  )
  select * from updated_user
  union all
  select *
  from users
  where id = nullif(current_setting('jwt.claims.user_id', true), '')::integer
    and not exists (select 1 from updated_user);
$$;

grant all on function get_current_user to anonymous;
