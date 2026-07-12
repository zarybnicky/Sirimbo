create or replace function event_instance_series_info(instance event_instance)
  returns series_info
  language sql
  stable
as $$
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

grant execute on function event_instance_series_info to anonymous;
