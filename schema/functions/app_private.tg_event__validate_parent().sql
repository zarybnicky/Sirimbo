CREATE FUNCTION app_private.tg_event__validate_parent() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  if new.parent_id is null then return new; end if;

  perform 1 from event parent where parent.id = new.parent_id for no key update;

  if not found or exists (
    with recursive ancestors as (
      select id, parent_id
      from event where id = new.parent_id
      union
      select parent.id, parent.parent_id
      from event parent
      join ancestors child on parent.id = child.parent_id
    )
    select 1 from ancestors where id = new.id
  ) then
    raise exception 'Event instance parent must not create a cycle'
      using errcode = '23514';
  end if;

  return new;
end;
$$;
