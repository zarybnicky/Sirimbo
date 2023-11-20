CREATE FUNCTION app_private.log_in_as(u public.users) RETURNS TABLE(key text, value text)
    LANGUAGE sql
    AS $$
  select 'jwt.claims.' || kv.key, set_config('jwt.claims.' || kv.key, kv.value, false)
  from app_private.create_jwt_token(u) j join lateral jsonb_each_text(to_jsonb(j)) kv on true
  union
  select 'role', set_config('role', case when is_admin then 'administrator' when is_trainer then 'trainer' when is_member then 'member' else 'anonymous' end, false)
  from app_private.create_jwt_token(u) j
$$;



