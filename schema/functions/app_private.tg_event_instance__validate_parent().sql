CREATE FUNCTION app_private.tg_event_instance__validate_parent() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'pg_temp'
    AS $$
begin
  if tg_op = 'UPDATE'
    and new.tenant_id is distinct from old.tenant_id
    and exists (
      select 1 from public.event_instance child where child.parent_id = new.id
    ) then
    raise exception 'An event instance with children cannot change tenant'
      using errcode = '23514';
  end if;

  if new.parent_id is null then return new; end if;

  perform 1
  from public.event_instance parent
  where parent.id = new.parent_id
    and parent.tenant_id = new.tenant_id
  for no key update;

  if not found or exists (
    with recursive ancestors as (
      select id, parent_id
      from public.event_instance where id = new.parent_id
      union
      select parent.id, parent.parent_id
      from public.event_instance parent
      join ancestors child on parent.id = child.parent_id
    )
    select 1 from ancestors where id = new.id
  ) then
    raise exception 'Event instance parent must be in the same tenant and not create a cycle'
      using errcode = '23514';
  end if;

  return new;
end;
$$;

REVOKE ALL ON FUNCTION app_private.tg_event_instance__validate_parent() FROM PUBLIC;
