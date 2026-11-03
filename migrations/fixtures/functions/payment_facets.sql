create or replace function app_private.payment_search_ids(search text)
returns setof bigint language sql stable as $$
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
grant execute on function app_private.payment_search_ids to anonymous;

create or replace function payment_facets(
  only_status payment_status default null,
  only_kind payment_kind default null,
  person_ids bigint[] default null,
  search text default null
) returns table(facet text, value text, count integer, label text)
language sql stable as $$
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
comment on function payment_facets is '@behavior +list -connection';
grant execute on function payment_facets to anonymous;
