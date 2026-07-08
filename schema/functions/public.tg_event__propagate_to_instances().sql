CREATE FUNCTION public.tg_event__propagate_to_instances() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if (row(old.name, old.type, old.location_text, old.location_id, old.is_visible, old.is_public,
          old.capacity, old.is_locked, old.description, old.summary, old.enable_notes, old.files_legacy)
    is not distinct from
      row(new.name, new.type, new.location_text, new.location_id, new.is_visible, new.is_public,
          new.capacity, new.is_locked, new.description, new.summary, new.enable_notes, new.files_legacy)) then
    return null;
  end if;

  update event_instance ei
  set
    name          = case when (ei.custom ? 'name')          then ei.name          else new.name          end,
    type          = case when (ei.custom ? 'type')          then ei.type          else new.type          end,
    location_text = case when (ei.custom ? 'location_text') then ei.location_text else new.location_text end,
    location_id   = case when (ei.custom ? 'location_id')   then ei.location_id   else new.location_id   end,
    is_visible    = case when (ei.custom ? 'is_visible')    then ei.is_visible    else new.is_visible    end,
    is_public     = case when (ei.custom ? 'is_public')     then ei.is_public     else new.is_public     end,
    capacity      = case when (ei.custom ? 'capacity')      then ei.capacity      else new.capacity      end,
    is_locked     = case when (ei.custom ? 'is_locked')     then ei.is_locked     else new.is_locked     end,
    description   = case when (ei.custom ? 'description')   then ei.description   else new.description   end,
    summary       = case when (ei.custom ? 'summary')       then ei.summary       else new.summary       end,
    enable_notes  = case when (ei.custom ? 'enable_notes')  then ei.enable_notes  else new.enable_notes  end,
    files_legacy  = case when (ei.custom ? 'files_legacy')  then ei.files_legacy  else new.files_legacy  end
  where ei.tenant_id = new.tenant_id
    and ei.event_id = new.id
    and (
      ((not (ei.custom ? 'name'))          and ei.name          is distinct from new.name) or
      ((not (ei.custom ? 'type'))          and ei.type          is distinct from new.type) or
      ((not (ei.custom ? 'location_text')) and ei.location_text is distinct from new.location_text) or
      ((not (ei.custom ? 'location_id'))   and ei.location_id   is distinct from new.location_id) or
      ((not (ei.custom ? 'is_visible'))    and ei.is_visible    is distinct from new.is_visible) or
      ((not (ei.custom ? 'is_public'))     and ei.is_public     is distinct from new.is_public) or
      ((not (ei.custom ? 'capacity'))      and ei.capacity      is distinct from new.capacity) or
      ((not (ei.custom ? 'is_locked'))     and ei.is_locked     is distinct from new.is_locked) or
      ((not (ei.custom ? 'description'))   and ei.description   is distinct from new.description) or
      ((not (ei.custom ? 'summary'))       and ei.summary       is distinct from new.summary) or
      ((not (ei.custom ? 'enable_notes'))  and ei.enable_notes  is distinct from new.enable_notes) or
      ((not (ei.custom ? 'files_legacy'))  and ei.files_legacy  is distinct from new.files_legacy)
    );

  return null;
end;
$$;
