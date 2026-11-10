-- Evaluate both interval arithmetic and date boundaries in UTC.
create or replace function crawler.next_fetch_at(
  p_federation text, p_kind text, p_reference_date date, p_fetched_at timestamptz
) returns timestamptz language plpgsql stable set timezone = 'UTC' as $$
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

create or replace function crawler.tg_frontier_reference_date() returns trigger
language plpgsql as $$
begin
  if new.reference_date is distinct from old.reference_date and new.fetch_status = 'ok' then
    new.next_fetch_at := crawler.next_fetch_at(new.federation, new.kind, new.reference_date, new.last_fetched_at);
  end if;
  return new;
end;
$$;

create or replace trigger frontier_reference_date
  before update of reference_date on crawler.frontier
  for each row execute function crawler.tg_frontier_reference_date();

create or replace function crawler.recalculate_fetch_times(p_federation text, p_kind text)
returns integer language plpgsql as $$
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

comment on function crawler.next_fetch_at is '@behavior -*';
comment on function crawler.recalculate_fetch_times is '@behavior -*';
