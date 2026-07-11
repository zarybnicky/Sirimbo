CREATE FUNCTION app_private.relationship_status_next(ts timestamp with time zone, range tstzrange, current public.relationship_status) RETURNS public.relationship_status
    LANGUAGE sql IMMUTABLE
    AS $$
  SELECT CASE
    WHEN ts < lower(range) THEN 'pending'
    WHEN NOT upper_inf(range) AND ts >= upper(range) THEN 'expired'
    WHEN range @> ts THEN 'active'
    ELSE current
  END
$$;
