CREATE FUNCTION app_private.event_share_claims(p_tenant_id bigint, p_share text) RETURNS TABLE(instance_ids bigint[])
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $_$
  with recursive root as (
    select id
    from event
    where tenant_id = p_tenant_id
      and (
        share_token = p_share
        or (
          is_public
          and has_public_details
          and id = case
            when p_share ~ '^\d{1,18}$' then p_share::bigint
          end
        )
      )
  ),
  events as (
    select id from root
    union
    select child.id
    from event child
    join events parent on child.parent_id = parent.id
    where child.tenant_id = p_tenant_id
  )
  select array(select id from events)
  where exists (select 1 from root);
$_$;

REVOKE ALL ON FUNCTION app_private.event_share_claims(p_tenant_id bigint, p_share text) FROM PUBLIC;
