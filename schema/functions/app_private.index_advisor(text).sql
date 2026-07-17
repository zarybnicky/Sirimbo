CREATE FUNCTION app_private.index_advisor(query text) RETURNS TABLE(startup_cost_before jsonb, startup_cost_after jsonb, total_cost_before jsonb, total_cost_after jsonb, index_statements text[], errors text[])
    LANGUAGE plpgsql
    AS $_$
declare
  n_args int;
  prepared_statement_name text = 'index_advisor_working_statement';
  hypopg_schema_name text = (select extnamespace::regnamespace::text from pg_extension where extname = 'hypopg');
  explain_plan_statement text;
  error_message text;
  rec record;
  plan_initial jsonb;
  plan_final jsonb;
  statements text[] := array[]::text[];
  v_context text;
begin
  -- Remove comment lines (it is common for them to contain semicolons).
  query := trim(
    regexp_replace(
      regexp_replace(
        regexp_replace(query, '\/\*.+\*\/', '', 'g'),
        '--[^\r\n]*',
        ' ',
        'g'
      ),
      '\s+',
      ' ',
      'g'
    )
  );

  query := regexp_replace(query, ';\s*$', '');

  begin
    if query ilike '%;%' then
      raise exception 'Query must not contain a semicolon';
    end if;

    -- Hack to support PostgREST because prepared-statement arguments default to text.
    query := replace(
      query,
      'WITH pgrst_payload AS (SELECT $1 AS json_data)',
      'WITH pgrst_payload AS (SELECT $1::json AS json_data)'
    );

    deallocate all;
    perform plpgsql_check_pragma('disable:security_warnings');
    execute format('prepare %I as %s', prepared_statement_name, query);
    perform plpgsql_check_pragma('enable:security_warnings');

    n_args := (
      select coalesce(array_length(parameter_types, 1), 0)
      from pg_prepared_statements
      where name = prepared_statement_name
      limit 1
    );

    explain_plan_statement := format(
      'set local plan_cache_mode = force_generic_plan; explain (format json) execute %I%s',
      prepared_statement_name,
      case
        when n_args = 0 then ''
        else format(
          '(%s)',
          array_to_string(array_fill('null'::text, array[n_args]), ',')
        )
      end
    );
    execute explain_plan_statement into plan_initial;

    for rec in (
      with extension_regclass as (
        select distinct objid as oid
        from pg_catalog.pg_depend
        where deptype = 'e'
      )
      select
        pc.relnamespace::regnamespace::text as schema_name,
        pc.relname as table_name,
        pa.attname as column_name,
        format(
          'select %I.hypopg_create_index($i$create index on %I.%I(%I)$i$)',
          hypopg_schema_name,
          pc.relnamespace::regnamespace::text,
          pc.relname,
          pa.attname
        ) as hypopg_statement
      from pg_catalog.pg_class pc
      join pg_catalog.pg_attribute pa on pc.oid = pa.attrelid
      left join extension_regclass er on pc.oid = er.oid
      left join pg_catalog.pg_index pi
        on pc.oid = pi.indrelid
       and (select array_agg(x) from unnest(pi.indkey) v(x)) = array[pa.attnum]
       and pi.indexprs is null
       and pi.indpred is null
      where pc.relnamespace::regnamespace::text not in (
        'pg_catalog',
        'pg_toast',
        'information_schema'
      )
        and er.oid is null
        and pc.relkind in ('r', 'm')
        and pc.relpersistence = 'p'
        and pa.attnum > 0
        and not pa.attisdropped
        and pi.indrelid is null
        and pa.atttypid in (
          20,
          16,
          1082,
          1184,
          1114,
          701,
          23,
          21,
          700,
          1083,
          2950,
          1700,
          25,
          18,
          1042,
          1043
        )
    ) loop
      perform plpgsql_check_pragma('disable:security_warnings');
      execute rec.hypopg_statement;
      perform plpgsql_check_pragma('enable:security_warnings');
    end loop;

    deallocate index_advisor_working_statement;
    perform plpgsql_check_pragma('disable:security_warnings');
    execute format('prepare %I as %s', prepared_statement_name, query);
    perform plpgsql_check_pragma('enable:security_warnings');

    execute explain_plan_statement into plan_final;

    execute format(
      'select
         coalesce(
           array_agg(hypopg_get_indexdef(indexrelid) order by indrelid, indkey::text),
           $i${}$i$::text[]
         )
       from %I.hypopg()
       where %s ilike ($i$%%$i$ || indexname || $i$%%$i$)',
      hypopg_schema_name,
      quote_literal(plan_final)::text
    ) into statements;

    perform hypopg_reset();
    deallocate all;

    return query values (
      plan_initial -> 0 -> 'Plan' -> 'Startup Cost',
      plan_final -> 0 -> 'Plan' -> 'Startup Cost',
      plan_initial -> 0 -> 'Plan' -> 'Total Cost',
      plan_final -> 0 -> 'Plan' -> 'Total Cost',
      statements,
      array[]::text[]
    );
    return;
  exception when others then
    get stacked diagnostics
      error_message = message_text,
      v_context = pg_exception_context;

    return query values (
      null::jsonb,
      null::jsonb,
      null::jsonb,
      null::jsonb,
      array[]::text[],
      array[error_message, v_context]::text[]
    );
    return;
  end;
end;
$_$;

COMMENT ON FUNCTION app_private.index_advisor(query text) IS '@omit';
