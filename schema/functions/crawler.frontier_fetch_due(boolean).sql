CREATE FUNCTION crawler.frontier_fetch_due(allow_refetch boolean) RETURNS TABLE(id bigint, federation text, kind text, key text, discovered_at timestamp with time zone, last_fetched_at timestamp with time zone, next_fetch_at timestamp with time zone, due_at timestamp with time zone)
    LANGUAGE sql STABLE
    AS $$
  SELECT
    f.id,
    f.federation,
    f.kind,
    f.key,
    f.discovered_at,
    f.last_fetched_at,
    f.next_fetch_at,
    coalesce(f.next_fetch_at, f.discovered_at) AS due_at
  FROM crawler.frontier f
  WHERE (f.next_fetch_at IS NULL OR f.next_fetch_at <= now())
    AND (
      f.fetch_status IN ('pending', 'transient')
      OR (
        coalesce(allow_refetch, false)
        AND f.fetch_status = 'ok'
        AND f.process_status = 'ok'
      )
    );
$$;
