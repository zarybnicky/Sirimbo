create or replace function app_private.current_shared_event_ids() returns bigint[]
language sql stable security definer
set search_path = pg_catalog, public, pg_temp
as $$
  select case
    when nullif(current_setting('jwt.claims.shared.event_token', true), '') is not null then
      coalesce((select instance_ids from app_private.event_share_claims(
        current_tenant_id(), current_setting('jwt.claims.shared.event_token', true)
      )), '{}'::bigint[])
    else '{}'::bigint[]
  end;
$$;
