CREATE FUNCTION app_private.tg_auth_details__refresh() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
BEGIN
  refresh materialized view concurrently auth_details;
  return null;
END
$$;
