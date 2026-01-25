BEGIN;

SET LOCAL app.check_schemas TO 'public,app_private,federated,crawler';

CREATE TEMP TABLE _sqlfunc_issues (
  functionid regprocedure,
  sqlstate text,
  message text,
  attempted_explain text
) ON COMMIT DROP;


DO $$
DECLARE
  r record;
  arglist text;
  explain_sql text;
  fname text;
  schemas text[] := regexp_split_to_array(current_setting('app.check_schemas', true), '\s*,\s*');
BEGIN
  FOR r IN
    SELECT
      p.oid,
      p.oid::regprocedure AS functionid,
      n.nspname,
      p.proname,
      p.proretset,
      p.prorettype,
      p.proargtypes
    FROM pg_proc p
    JOIN pg_language l ON l.oid = p.prolang
    JOIN pg_namespace n ON n.oid = p.pronamespace
    WHERE l.lanname = 'sql'
      AND p.prokind = 'f'
      AND n.nspname NOT IN ('pg_catalog','information_schema')
      AND n.nspname = ANY (schemas)
  LOOP
    -- Skip polymorphic-argument SQL funcs (can’t construct a deterministic typed call)
    IF EXISTS (
      SELECT 1
      FROM unnest(r.proargtypes::oid[]) AS a(typ)
      WHERE a.typ IN (
        'anyelement'::regtype,
        'anyarray'::regtype,
        'anynonarray'::regtype,
        'anyenum'::regtype,
        'anyrange'::regtype,
        'anymultirange'::regtype,
        'anycompatible'::regtype,
        'anycompatiblearray'::regtype,
        'anycompatiblenonarray'::regtype,
        'anycompatiblerange'::regtype,
        'anycompatiblemultirange'::regtype
      )
    ) THEN
      CONTINUE;
    END IF;

    SELECT
      COALESCE(
        string_agg(format('NULL::%s', format_type(a.typ, NULL)), ', '),
        ''
      )
    INTO arglist
    FROM unnest(r.proargtypes::oid[]) AS a(typ);

    fname := format('%I.%I', r.nspname, r.proname);
    IF r.proretset THEN
      explain_sql := format('EXPLAIN SELECT * FROM %s(%s) LIMIT 0', fname, arglist);
    ELSE
      explain_sql := format('EXPLAIN SELECT %s(%s)', fname, arglist);
    END IF;

    BEGIN
      EXECUTE explain_sql;
    EXCEPTION WHEN OTHERS THEN
      INSERT INTO _sqlfunc_issues(functionid, sqlstate, message, attempted_explain)
      VALUES (r.functionid, SQLSTATE, SQLERRM, explain_sql);
    END;
  END LOOP;
END $$;

TABLE _sqlfunc_issues ORDER BY functionid::text;

DO $$
DECLARE
  total int;
  shown int := 25;
  msg text;
BEGIN
  SELECT count(*) INTO total FROM _sqlfunc_issues;

  IF total > 0 THEN
    SELECT string_agg(
      format(
        '%s: %s (SQLSTATE %s)%s%s',
        functionid::text,
        message,
        sqlstate,
        E'\n  ',
        attempted_explain
      ),
      E'\n\n'
    )
    INTO msg
    FROM (
      SELECT *
      FROM _sqlfunc_issues
      ORDER BY functionid::text
      LIMIT shown
    ) s;

    IF total > shown THEN
      msg := msg || format(E'\n\n... (%s more)', total - shown);
    END IF;

    RAISE EXCEPTION USING
      ERRCODE = 'P0001',
      MESSAGE = 'LANGUAGE SQL function plan-check failed:' || E'\n' || msg;
  END IF;
END $$;

DO $$
DECLARE r record;
BEGIN
  FOR r IN
    SELECT n.nspname, c.relname, c.relkind
    FROM pg_class c
    JOIN pg_namespace n ON n.oid = c.relnamespace
    WHERE c.relkind IN ('v','m')  -- view, matview
      AND n.nspname NOT IN ('pg_catalog','information_schema')
  LOOP
    EXECUTE format('EXPLAIN SELECT * FROM %I.%I LIMIT 0', r.nspname, r.relname);
  END LOOP;
END $$;

COMMIT;
