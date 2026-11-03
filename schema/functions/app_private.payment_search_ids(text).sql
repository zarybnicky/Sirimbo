CREATE FUNCTION app_private.payment_search_ids(search text) RETURNS SETOF bigint
    LANGUAGE sql STABLE
    AS $$
  with term as not materialized (
    select lower(trim(search)) as value
  )
  select p.id from payment p, term
  where strpos(p.id::text, term.value) > 0
    or strpos(lower(p.variable_symbol), term.value) > 0
    or strpos(lower(p.specific_symbol), term.value) > 0
  union all
  select p.id from payment p
  join event e on e.id = p.event_id, term
  where strpos(lower(e.name), term.value) > 0
  union all
  select p.id from payment p
  join cohort_subscription cs on cs.id = p.cohort_subscription_id
  join cohort c on c.id = cs.cohort_id, term
  where strpos(lower(c.name), term.value) > 0
  union all
  select d.payment_id from payment_debtor d
  join person p on p.id = d.person_id, term
  where strpos(lower(concat_ws(' ', p.first_name, p.last_name)), term.value) > 0;
$$;

GRANT ALL ON FUNCTION app_private.payment_search_ids(search text) TO anonymous;
