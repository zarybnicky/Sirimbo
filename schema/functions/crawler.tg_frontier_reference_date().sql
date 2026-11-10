CREATE FUNCTION crawler.tg_frontier_reference_date() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  if new.reference_date is distinct from old.reference_date and new.fetch_status = 'ok' then
    new.next_fetch_at := crawler.next_fetch_at(new.federation, new.kind, new.reference_date, new.last_fetched_at);
  end if;
  return new;
end;
$$;
