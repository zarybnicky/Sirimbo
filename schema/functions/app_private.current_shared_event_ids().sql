CREATE FUNCTION app_private.current_shared_event_ids() RETURNS bigint[]
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
  select case
    when nullif(current_setting('jwt.claims.shared.event_token', true), '') is not null then
      coalesce((select instance_ids from app_private.event_share_claims(
        current_tenant_id(), current_setting('jwt.claims.shared.event_token', true)
      )), '{}'::bigint[])
    else '{}'::bigint[]
  end;
$$;
