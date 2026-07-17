CREATE FUNCTION public.verify_function(f regproc, relid regclass DEFAULT 0) RETURNS void
    LANGUAGE plpgsql
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  issues text;
begin
  select string_agg(
    concat_ws(
      E'\n',
      format(
        '%s:%s:%s:%s: %s',
        issue.level,
        issue.sqlstate,
        coalesce(issue.lineno::text, '?'),
        issue.statement,
        issue.message
      ),
      'Query: ' || issue.query,
      'Detail: ' || issue.detail,
      'Hint: ' || issue.hint,
      'Context: ' || issue.context
    ),
    E'\n\n' order by issue.lineno nulls last, issue.level, issue.message
  ) into issues
  from plpgsql_check_function_tb(
    funcoid => f,
    relid => relid,
    fatal_errors => false,
    all_warnings => true
  ) issue;

  if issues is not null then
    raise exception 'Error when checking function %', f using detail = issues;
  end if;
end;
$$;

COMMENT ON FUNCTION public.verify_function(f regproc, relid regclass) IS '@omit';

REVOKE ALL ON FUNCTION public.verify_function(f regproc, relid regclass) FROM PUBLIC;
