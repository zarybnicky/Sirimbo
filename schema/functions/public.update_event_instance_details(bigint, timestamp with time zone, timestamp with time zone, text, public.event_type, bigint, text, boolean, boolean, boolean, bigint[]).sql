CREATE FUNCTION public.update_event_instance_details(p_instance_id bigint, p_since timestamp with time zone, p_until timestamp with time zone, p_name text, p_type public.event_type, p_location_id bigint, p_location_text text, p_is_visible boolean, p_is_public boolean, p_is_cancelled boolean, p_trainer_person_ids bigint[] DEFAULT NULL::bigint[]) RETURNS public.event_instance
    LANGUAGE plpgsql
    AS $$
declare
  updated_instance public.event_instance;
begin
  update public.event_instance
  set
    since = p_since,
    until = p_until,
    name = p_name,
    type = p_type,
    location_id = p_location_id,
    location_text = coalesce(p_location_text, ''),
    is_visible = coalesce(p_is_visible, is_visible),
    is_public = coalesce(p_is_public, is_public),
    is_cancelled = p_is_cancelled
  where id = p_instance_id
  returning * into updated_instance;

  if not found then
    raise exception 'event instance % not found', p_instance_id;
  end if;

  if p_trainer_person_ids is not null then
    delete from public.event_instance_trainer
    where instance_id = p_instance_id;

    insert into public.event_instance_trainer (instance_id, person_id)
    select distinct p_instance_id, input.person_id
    from unnest(p_trainer_person_ids) as input(person_id)
    where input.person_id is not null
    on conflict (instance_id, person_id) do nothing;
  end if;

  return updated_instance;
end;
$$;

GRANT ALL ON FUNCTION public.update_event_instance_details(p_instance_id bigint, p_since timestamp with time zone, p_until timestamp with time zone, p_name text, p_type public.event_type, p_location_id bigint, p_location_text text, p_is_visible boolean, p_is_public boolean, p_is_cancelled boolean, p_trainer_person_ids bigint[]) TO anonymous;
