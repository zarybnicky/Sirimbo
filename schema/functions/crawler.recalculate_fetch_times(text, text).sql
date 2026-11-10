CREATE FUNCTION crawler.recalculate_fetch_times(p_federation text, p_kind text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
  updated integer;
begin
  with times as materialized (
    select f.id, crawler.next_fetch_at(f.federation, f.kind, f.reference_date, f.last_fetched_at) as next_fetch_at
    from crawler.frontier f
    where f.fetch_status = 'ok' and f.last_fetched_at is not null
      and f.federation = p_federation and f.kind = p_kind
  )
  update crawler.frontier f set next_fetch_at = t.next_fetch_at
  from times t where f.id = t.id and f.next_fetch_at is distinct from t.next_fetch_at;
  get diagnostics updated = row_count;
  return updated;
end;
$$;

COMMENT ON FUNCTION crawler.recalculate_fetch_times(p_federation text, p_kind text) IS '@behavior -*';
