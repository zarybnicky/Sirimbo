CREATE FUNCTION app_private.tg_eir__refresh_stats_stmt() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'pg_temp'
    AS $$

REVOKE ALL ON FUNCTION app_private.tg_eir__refresh_stats_stmt() FROM PUBLIC;
