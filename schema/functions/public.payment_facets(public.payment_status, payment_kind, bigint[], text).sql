CREATE FUNCTION public.payment_facets(only_status public.payment_status DEFAULT NULL::public.payment_status, only_kind public.payment_kind DEFAULT NULL::public.payment_kind, person_ids bigint[] DEFAULT NULL::bigint[], search text DEFAULT NULL::text) RETURNS TABLE(facet text, value text, count integer, label text)
    LANGUAGE sql STABLE
    AS $$
  with candidates as materialized (
    select p.id, p.status, p.kind,
      person_ids is null or p.id in (
        select payment_id from payment_debtor where person_id = any(person_ids)
      ) as person_matches
    from payment p
    where nullif(trim(search), '') is null
      or p.id in (select * from app_private.payment_search_ids(search))
  ), summary as (
    select p.status, p.kind, count(*) as count
    from candidates p where p.person_matches
    group by p.status, p.kind
  ), person_counts as (
    select d.person_id, count(distinct d.payment_id)::integer as count
    from payment_debtor d
    -- Keep the index order for distinct counts instead of joining and sorting debtors.
    where (d.payment_id in (
      select p.id from candidates p
      where (only_status is null or p.status = only_status)
        and (only_kind is null or p.kind = only_kind)
    )) is true
    group by d.person_id
  )
  select 'status', coalesce(upper(p.status::text), ''), coalesce(sum(p.count), 0)::integer, null::text
  from summary p
  where only_kind is null or p.kind = only_kind
  group by rollup(p.status)
  union all
  select 'kind', coalesce(upper(replace(p.kind::text, '-', '_')), ''), coalesce(sum(p.count), 0)::integer, null::text
  from summary p
  where only_status is null or p.status = only_status
  group by rollup(p.kind)
  union all
  select 'person', p.id::text, coalesce(c.count, 0), p.name
  from person p left join person_counts c on c.person_id = p.id
  where c.person_id is not null or p.id = any(person_ids);
$$;

COMMENT ON FUNCTION public.payment_facets(only_status public.payment_status, only_kind public.payment_kind, person_ids bigint[], search text) IS '@behavior +list -connection';

GRANT ALL ON FUNCTION public.payment_facets(only_status public.payment_status, only_kind public.payment_kind, person_ids bigint[], search text) TO anonymous;
