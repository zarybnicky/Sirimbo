CREATE FUNCTION app_private.refresh_event_series_info(p_series_ids bigint[]) RETURNS void
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
begin
  if coalesce(cardinality(p_series_ids), 0) = 0 then
    return;
  end if;

  perform id from event_series
  where id = any(p_series_ids) order by id for no key update;

  with actual as (
    select member.id,
      row(series.id, series.name,
        row_number() over ordered::integer,
        count(*) over whole_series::integer,
        min(member.since) over whole_series,
        max(member.until) over whole_series)::series_info as info
    from event member
    join event_series series
      on series.tenant_id = member.tenant_id and series.id = member.series_id
    where series.id = any(p_series_ids)
    window whole_series as (partition by member.tenant_id, member.series_id),
      ordered as (whole_series order by member.since, member.id)
  )
  update event member set series_info = actual.info
  from actual
  where member.id = actual.id and member.series_info is distinct from actual.info;
end;
$$;
