CREATE FUNCTION app_private.refresh_event_instance_stats(p_instance_id bigint) RETURNS void
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'pg_temp'
    AS $$
declare
  v_stats jsonb;
begin
  perform 1 from public.event_instance ei where ei.id = p_instance_id for no key update;
  if not found then
    return;
  end if;

  select jsonb_build_object(
    'TOTAL', count(*) filter (where eir.status <> 'cancelled')::int,
    'UNKNOWN', count(*) filter (where eir.status = 'unknown')::int,
    'ATTENDED', count(*) filter (where eir.status = 'attended')::int,
    'NOT_EXCUSED', count(*) filter (where eir.status = 'not-excused')::int
  )
  into v_stats
  from public.event_instance_registration eir
  where eir.instance_id = p_instance_id
    and eir.person_id is not null;

  update public.event_instance ei
  set stats = v_stats
  where ei.id = p_instance_id
    and ei.stats is distinct from v_stats;
end;
$$;

REVOKE ALL ON FUNCTION app_private.refresh_event_instance_stats(p_instance_id bigint) FROM PUBLIC;
