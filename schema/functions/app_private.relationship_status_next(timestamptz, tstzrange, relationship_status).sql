CREATE FUNCTION app_private.relationship_status_next(ts timestamp with time zone, range tstzrange, current public.relationship_status) RETURNS public.relationship_status
    LANGUAGE sql IMMUTABLE
    AS $$
  select case
    when ts < lower(range) then 'pending'
    when not upper_inf(range) and ts >= upper(range) then 'expired'
    when range @> ts then 'active'
    else current
  end
$$;
