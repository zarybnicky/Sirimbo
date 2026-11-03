CREATE FUNCTION public.transaction_facets(since timestamp with time zone DEFAULT NULL::timestamp with time zone, until timestamp with time zone DEFAULT NULL::timestamp with time zone, sources public.transaction_source[] DEFAULT NULL::public.transaction_source[], account_ids bigint[] DEFAULT NULL::bigint[], search text DEFAULT NULL::text) RETURNS TABLE(facet text, value text, count integer)
    LANGUAGE sql STABLE
    AS $$
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

COMMENT ON FUNCTION public.transaction_facets(since timestamp with time zone, until timestamp with time zone, sources public.transaction_source[], account_ids bigint[], search text) IS '@behavior +list -connection';

GRANT ALL ON FUNCTION public.transaction_facets(since timestamp with time zone, until timestamp with time zone, sources public.transaction_source[], account_ids bigint[], search text) TO anonymous;
