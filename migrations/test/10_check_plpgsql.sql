BEGIN;

CREATE EXTENSION IF NOT EXISTS plpgsql_check;

SET LOCAL app.check_schemas TO 'public,app_private,federated,crawler';

CREATE TEMP TABLE _plpgsql_issues (
  functionid regprocedure,
  relid regclass,
  lineno int,
  statement text,
  sqlstate text,
  message text,
  detail text,
  hint text,
  level text,
  position int,
  query text,
  context text
) ON COMMIT DROP;

INSERT INTO _plpgsql_issues(functionid, relid, lineno, statement, sqlstate, message, detail, hint, level, position, query, context)
SELECT
  (pcf).functionid::regprocedure,
  NULLIF(COALESCE(t.tgrelid, 0), 0)::regclass,
  (pcf).lineno,
  (pcf).statement,
  (pcf).sqlstate,
  (pcf).message,
  (pcf).detail,
  (pcf).hint,
  (pcf).level,
  (pcf)."position",
  (pcf).query,
  (pcf).context
FROM pg_proc p
JOIN pg_language l ON l.oid = p.prolang
JOIN pg_namespace n ON n.oid = p.pronamespace
LEFT JOIN pg_trigger t
  ON t.tgfoid = p.oid
 AND NOT t.tgisinternal
CROSS JOIN LATERAL plpgsql_check_function_tb(
  p.oid,
  COALESCE(t.tgrelid, 0),
  oldtable => t.tgoldtable,
  newtable => t.tgnewtable,
  fatal_errors := false
) AS pcf
WHERE l.lanname = 'plpgsql'
  AND n.nspname NOT IN ('pg_catalog','information_schema')
  AND n.nspname = ANY (regexp_split_to_array(current_setting('app.check_schemas', true), '\s*,\s*'))
  -- skip functions that belong to installed extensions (pgTAP, etc.)
  AND NOT EXISTS (
    SELECT 1
      FROM pg_depend d
     WHERE d.classid = 'pg_proc'::regclass
       AND d.objid = p.oid
       AND d.deptype = 'e'
  )
  -- ignore “orphaned” trigger functions: if it RETURNS trigger but no trigger exists, skip
  AND (p.prorettype <> 'trigger'::regtype OR t.tgfoid IS NOT NULL);

SELECT *
FROM _plpgsql_issues
WHERE level = 'error'
ORDER BY functionid::text, relid::text NULLS FIRST, lineno;

DO $$
DECLARE
  total int;
  shown int := 25;
  msg text;
BEGIN
  SELECT count(*) INTO total
  FROM _plpgsql_issues
  WHERE level = 'error';

  IF total > 0 THEN
    SELECT string_agg(
      format(
        '%s%s @%s: %s (SQLSTATE %s): %s',
        functionid::text,
        CASE WHEN relid IS NULL THEN '' ELSE ' on ' || relid::text END,
        COALESCE(lineno::text, '?'),
        message,
        sqlstate,
        left(query, 200)
      ),
      E'\n'
    )
    INTO msg
    FROM (
      SELECT *
      FROM _plpgsql_issues
      WHERE level = 'error'
      ORDER BY functionid::text, relid::text NULLS FIRST, lineno NULLS LAST
      LIMIT shown
    ) s;

    IF total > shown THEN
      msg := msg || format(E'\n... (%s more)', total - shown);
    END IF;

    RAISE EXCEPTION USING
      ERRCODE = 'P0001',
      MESSAGE = 'plpgsql_check failed:' || E'\n' || msg;
  END IF;
END $$;

COMMIT;
