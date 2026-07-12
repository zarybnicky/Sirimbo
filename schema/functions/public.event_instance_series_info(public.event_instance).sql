CREATE FUNCTION public.event_instance_series_info(instance public.event_instance) RETURNS public.series_info
    LANGUAGE sql STABLE
    AS $$
  select row(
    ranked.series_id,
    ranked.name,
    ranked.position,
    ranked.length,
    ranked.since,
    ranked.until
  )::series_info
  from (
    select
      member.id as member_id,
      series.id as series_id,
      series.name,
      row_number() over (order by member.since, member.id)::integer as position,
      count(*) over ()::integer as length,
      min(member.since) over () as since,
      max(member.until) over () as until
    from event_instance member
    join event_series series on series.tenant_id = member.tenant_id and series.id = member.series_id
    where member.series_id = instance.series_id
  ) ranked
  where ranked.member_id = instance.id;
$$;

GRANT ALL ON FUNCTION public.event_instance_series_info(instance public.event_instance) TO anonymous;
