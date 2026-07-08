CREATE FUNCTION public.tg_event_instance__fill_defaults() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
declare e public.event;
begin
  -- standalone instance: no event to inherit from, its own values stand
  if new.event_id is null then
    new.custom := coalesce(new.custom, '{}'::jsonb);
    return new;
  end if;

  select * into strict e
  from public.event
  where tenant_id = new.tenant_id and id = new.event_id;

  new.custom := coalesce(new.custom, '{}'::jsonb);

  if new.name          is null then new.name          := e.name;          else new.custom := new.custom || jsonb_build_object('name', true);          end if;
  if new.type          is null then new.type          := e.type;          else new.custom := new.custom || jsonb_build_object('type', true);          end if;
  if new.location_text is null then new.location_text := e.location_text; else new.custom := new.custom || jsonb_build_object('location_text', true); end if;
  if new.location_id   is null then new.location_id   := e.location_id;   else new.custom := new.custom || jsonb_build_object('location_id', true);   end if;
  if new.is_visible    is null then new.is_visible    := e.is_visible;    else new.custom := new.custom || jsonb_build_object('is_visible', true);    end if;
  if new.is_public     is null then new.is_public     := e.is_public;     else new.custom := new.custom || jsonb_build_object('is_public', true);     end if;
  if new.capacity      is null then new.capacity      := e.capacity;      else new.custom := new.custom || jsonb_build_object('capacity', true);      end if;
  if new.is_locked     is null then new.is_locked     := e.is_locked;     else new.custom := new.custom || jsonb_build_object('is_locked', true);     end if;
  if new.description   is null then new.description   := e.description;   else new.custom := new.custom || jsonb_build_object('description', true);   end if;
  if new.summary       is null then new.summary       := e.summary;       else new.custom := new.custom || jsonb_build_object('summary', true);       end if;
  if new.enable_notes  is null then new.enable_notes  := e.enable_notes;  else new.custom := new.custom || jsonb_build_object('enable_notes', true);  end if;
  if new.files_legacy  is null then new.files_legacy  := e.files_legacy;  else new.custom := new.custom || jsonb_build_object('files_legacy', true);  end if;

  return new;
end;
$$;
