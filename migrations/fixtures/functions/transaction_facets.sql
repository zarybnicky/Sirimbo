create or replace function app_private.transaction_search_ids(search text)
returns setof bigint language sql stable as $$
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
grant execute on function app_private.transaction_search_ids to anonymous;

create or replace function transaction_facets(
  since timestamptz default null,
  until timestamptz default null,
  sources transaction_source[] default null,
  account_ids bigint[] default null,
  search text default null
) returns table(facet text, value text, count integer)
language sql stable as $$
  with candidates as materialized (
    select t.id, t.source,
      account_ids is null or t.id in (
        select transaction_id from posting where account_id = any(account_ids)
      ) as account_matches
    from transaction t
    where (since is null or t.effective_date >= since)
      and (until is null or t.effective_date < until)
      and (nullif(trim(search), '') is null
        or t.id in (select * from app_private.transaction_search_ids(search)))
  )
  select 'source', upper(replace(t.source::text, '-', '_')), count(*)::integer
  from candidates t where t.account_matches
  group by t.source
  union all
  select 'account', account_id::text, count(*)::integer
  from (
    select distinct p.account_id, t.id
    from candidates t join posting p on p.transaction_id = t.id
    where sources is null or t.source = any(sources)
  ) accounts
  group by account_id;
$$;
comment on function transaction_facets is '@behavior +list -connection';
grant execute on function transaction_facets to anonymous;
