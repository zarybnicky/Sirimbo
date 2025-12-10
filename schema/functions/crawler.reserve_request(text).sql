CREATE FUNCTION crawler.reserve_request(in_host text, OUT granted boolean, OUT allowed_at timestamp with time zone) RETURNS record
    LANGUAGE plpgsql
    AS $$
DECLARE
  r crawler.rate_limit_rule;
BEGIN
  SELECT *
  INTO r
  FROM crawler.rate_limit_rule
  WHERE host = in_host
  FOR UPDATE;

  IF NOT FOUND THEN
    granted := true;
    allowed_at := now();
    RETURN;
  END IF;

  IF r.next_available_at <= now() THEN
    UPDATE crawler.rate_limit_rule
       SET next_available_at = now() + r.spacing
     WHERE host = in_host;

    granted := true;
    allowed_at := now();
    RETURN;
  ELSE
    granted := false;
    allowed_at := (
      SELECT GREATEST(r.next_available_at, COALESCE(MAX(run_at), now()) + r.spacing)
      FROM graphile_worker.jobs
      WHERE key LIKE 'fetch:' || r.host || ':%'
    );
    RETURN;
  END IF;
END;
$$;
