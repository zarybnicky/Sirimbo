CREATE FUNCTION crawler.next_fetch_at(p_federation text, p_kind text, p_reference_date date, p_fetched_at timestamp with time zone) RETURNS timestamp with time zone
    LANGUAGE plpgsql STABLE
    SET "TimeZone" TO 'UTC'
    AS $$
declare
  fetch_interval interval;
  next_boundary timestamptz;
begin
  select p.fetch_interval into fetch_interval
  from crawler.refetch_policy p
  where p.federation = p_federation and p.kind = p_kind
    and (p.reference_offset is null or p_reference_date + p.reference_offset <= p_fetched_at)
  order by (p_reference_date + p.reference_offset) desc nulls last
  limit 1;

  select min((p_reference_date + p.reference_offset)::timestamptz) into next_boundary
  from crawler.refetch_policy p
  where p.federation = p_federation and p.kind = p_kind
    and p_reference_date + p.reference_offset > p_fetched_at;

  return least(p_fetched_at + fetch_interval, next_boundary);
end;
$$;

COMMENT ON FUNCTION crawler.next_fetch_at(p_federation text, p_kind text, p_reference_date date, p_fetched_at timestamp with time zone) IS '@behavior -*';
