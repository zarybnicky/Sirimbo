CREATE FUNCTION app_private.transaction_search_ids(search text) RETURNS SETOF bigint
    LANGUAGE sql STABLE
    AS $$
  with term as not materialized (
    select lower(trim(search)) as value
  )
  select t.id from transaction t, term
  where strpos(lower(t.description), term.value) > 0
    or t.payment_id in (
      select p.id from payment p
      where strpos(lower(p.variable_symbol), term.value) > 0
        or strpos(lower(p.specific_symbol), term.value) > 0
    );
$$;

GRANT ALL ON FUNCTION app_private.transaction_search_ids(search text) TO anonymous;
