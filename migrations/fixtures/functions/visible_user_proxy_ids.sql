create or replace function app_private.visible_user_proxy_ids() returns setof bigint
  language sql stable security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  select id from user_proxy
  where status = 'active'
    and person_id in (
      select person_id from user_proxy
      where user_id = (select current_user_id()) and status = 'active'
    );
$$;

grant execute on function app_private.visible_user_proxy_ids() to anonymous;
