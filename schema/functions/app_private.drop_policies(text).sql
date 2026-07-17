CREATE FUNCTION app_private.drop_policies(tbl text) RETURNS void
    LANGUAGE plpgsql
    AS $$
declare
  target regclass := tbl::regclass;
  policy_name name;
begin
  for policy_name in
    select polname
    from pg_catalog.pg_policy
    where polrelid = target
  loop
    execute format('drop policy %I on %s', policy_name, target);
  end loop;
end;
$$;
