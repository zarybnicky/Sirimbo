drop function if exists app_private.event_share_claims;

create function app_private.event_share_claims(p_tenant_id bigint, p_share text)
  returns table (instance_ids bigint[])
  language sql stable
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  with recursive root as (
    select id
    from event_instance
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
    from event_instance child
    join events parent on child.parent_id = parent.id
    where child.tenant_id = p_tenant_id
  )
  select array(select id from events)
  where exists (select 1 from root);
$$;

revoke all on function app_private.event_share_claims from public;
