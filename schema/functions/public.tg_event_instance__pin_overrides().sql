CREATE FUNCTION public.tg_event_instance__pin_overrides() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
declare
  e public.event;
  c jsonb;
begin
  c := coalesce(new.custom, '{}'::jsonb);

  -- standalone instance: nothing to inherit, so no override flags to maintain
  if new.event_id is null then
    new.custom := c;
    return new;
  end if;

  if new.event_id is distinct from old.event_id then
    select * into strict e
    from public.event
    where tenant_id = new.tenant_id and id = new.event_id;

    if not (c ? 'name')          then new.name          := e.name;          end if;
    if not (c ? 'type')          then new.type          := e.type;          end if;
    if not (c ? 'location_text') then new.location_text := e.location_text; end if;
    if not (c ? 'location_id')   then new.location_id   := e.location_id;   end if;
    if not (c ? 'is_visible')    then new.is_visible    := e.is_visible;    end if;
    if not (c ? 'is_public')     then new.is_public     := e.is_public;     end if;
    if not (c ? 'capacity')      then new.capacity      := e.capacity;      end if;
    if not (c ? 'is_locked')     then new.is_locked     := e.is_locked;     end if;
    if not (c ? 'description')   then new.description   := e.description;   end if;
    if not (c ? 'summary')       then new.summary       := e.summary;       end if;
    if not (c ? 'enable_notes')  then new.enable_notes  := e.enable_notes;  end if;
    if not (c ? 'files_legacy')  then new.files_legacy  := e.files_legacy;  end if;

    new.custom := c;
    return new;
  end if;

  if (new.name          is distinct from old.name) or
     (new.type          is distinct from old.type) or
     (new.location_text is distinct from old.location_text) or
     (new.location_id   is distinct from old.location_id) or
     (new.is_visible    is distinct from old.is_visible) or
     (new.is_public     is distinct from old.is_public) or
     (new.capacity      is distinct from old.capacity) or
     (new.is_locked     is distinct from old.is_locked) or
     (new.description   is distinct from old.description) or
     (new.summary       is distinct from old.summary) or
     (new.enable_notes  is distinct from old.enable_notes) or
     (new.files_legacy  is distinct from old.files_legacy) or
     (new.custom        is distinct from old.custom)
  then
   select * into strict e
    from public.event
    where tenant_id = new.tenant_id and id = new.event_id;
  end if;

  if new.name is distinct from old.name then
    if new.name is not distinct from e.name then c := c - 'name'; else c := c || jsonb_build_object('name', true); end if;
  end if;
  if new.type is distinct from old.type then
    if new.type is not distinct from e.type then c := c - 'type'; else c := c || jsonb_build_object('type', true); end if;
  end if;
  if new.location_text is distinct from old.location_text then
    if new.location_text is not distinct from e.location_text then c := c - 'location_text'; else c := c || jsonb_build_object('location_text', true); end if;
  end if;
  if new.location_id is distinct from old.location_id then
    if new.location_id is not distinct from e.location_id then c := c - 'location_id'; else c := c || jsonb_build_object('location_id', true); end if;
  end if;
  if new.is_visible is distinct from old.is_visible then
    if new.is_visible is not distinct from e.is_visible then c := c - 'is_visible'; else c := c || jsonb_build_object('is_visible', true); end if;
  end if;
  if new.is_public is distinct from old.is_public then
    if new.is_public is not distinct from e.is_public then c := c - 'is_public'; else c := c || jsonb_build_object('is_public', true); end if;
  end if;
  if new.capacity is distinct from old.capacity then
    if new.capacity is not distinct from e.capacity then c := c - 'capacity'; else c := c || jsonb_build_object('capacity', true); end if;
  end if;
  if new.is_locked is distinct from old.is_locked then
    if new.is_locked is not distinct from e.is_locked then c := c - 'is_locked'; else c := c || jsonb_build_object('is_locked', true); end if;
  end if;
  if new.description is distinct from old.description then
    if new.description is not distinct from e.description then c := c - 'description'; else c := c || jsonb_build_object('description', true); end if;
  end if;
  if new.summary is distinct from old.summary then
    if new.summary is not distinct from e.summary then c := c - 'summary'; else c := c || jsonb_build_object('summary', true); end if;
  end if;
  if new.enable_notes is distinct from old.enable_notes then
    if new.enable_notes is not distinct from e.enable_notes then c := c - 'enable_notes'; else c := c || jsonb_build_object('enable_notes', true); end if;
  end if;
  if new.files_legacy is distinct from old.files_legacy then
    if new.files_legacy is not distinct from e.files_legacy then c := c - 'files_legacy'; else c := c || jsonb_build_object('files_legacy', true); end if;
  end if;

  new.custom := c;
  return new;
end;
$$;
