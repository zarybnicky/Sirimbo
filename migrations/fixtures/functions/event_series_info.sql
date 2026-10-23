drop function if exists event_instance_series_info;

create or replace function event_series_info(instance event)
  returns series_info
  language sql
  stable
as $$
  select row(
    ranked.series_id,
    series.name,
    ranked.position,
    ranked.length,
    ranked.since,
    ranked.until
  )::series_info
  from (
    select
      member.id as member_id,
      member.series_id,
      member.tenant_id,
      row_number() over (order by member.since, member.id)::integer as position,
      count(*) over ()::integer as length,
      min(member.since) over () as since,
      max(member.until) over () as until
    from event member
    where member.series_id = instance.series_id
  ) ranked
  join event_series series on series.tenant_id = ranked.tenant_id and series.id = ranked.series_id
  where ranked.member_id = instance.id;
$$;

grant execute on function event_series_info to anonymous;
