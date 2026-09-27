CREATE FUNCTION app_private.visible_user_proxy_ids() RETURNS SETOF bigint
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select id from user_proxy
  where status = 'active'
    and person_id in (
      select person_id from user_proxy
      where user_id = (select current_user_id()) and status = 'active'
    );
$$;

GRANT ALL ON FUNCTION app_private.visible_user_proxy_ids() TO anonymous;
