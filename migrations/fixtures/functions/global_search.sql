drop function if exists global_search;

create or replace function global_search(search text, "limit" integer default 20, scope text[] default null)
  returns setof global_search_result
  language sql stable
as $$
  with input as materialized (
    select normalized.text, parsed.words, parsed.prefix
    from (select btrim(regexp_replace(app_private.normalize_name(coalesce(search, '')), '\s+', ' ', 'g')) as text) normalized
    cross join lateral (
      select string_agg(array_to_tsvector(array[token])::text, ' & ')::tsquery as words,
        string_agg(array_to_tsvector(array[token])::text || ':*', ' & ')::tsquery as prefix
      from unnest(tsvector_to_array(to_tsvector('pg_catalog.simple', normalized.text))) token
    ) parsed
    where char_length(normalized.text) >= 3 and parsed.prefix is not null
  ), person_recency as (
    -- Rank by the latest club relationship; pending relationships do not count.
    select person_id, max(coalesce(until, 'infinity'::timestamptz)) as until
    from (
      select person_id, until from tenant_membership where tenant_id = current_tenant_id() and status <> 'pending' and since <= now()
      union all
      select person_id, until from tenant_trainer where tenant_id = current_tenant_id() and status <> 'pending' and since <= now()
      union all
      select person_id, until from tenant_administrator where tenant_id = current_tenant_id() and status <> 'pending' and since <= now()
    ) relationships
    group by person_id
  ), peers as materialized (
    select person_id from app_private.tenant_relation
    where tenant_id = (select current_tenant_id())
      and current_cohort_ids && (select public.current_cohort_ids())::bigint[]
  ), event_matches as materialized (
    select e.*
    from event e
    where (scope is null or 'event' = any(scope)) and e.id = any (array(
      select ids.id from input q
      cross join lateral app_private.accessible_event_ids(q.prefix) ids(id)
    ))
  ), candidates as (
    select p as person, null::couple as couple, null::cohort as cohort,
      null::location as location, null::event as event,
      p.id, p.search_name as normalized_label, p.search_vector,
      (case when p.id = any((select current_person_ids())::bigint[]) then 20
        when p.id in (select person_id from peers) then 10 else 0 end
        + case when r.person_id is null then 0 else 10 / (1 + greatest(0, extract(epoch from now() - r.until)) / 86400 / 365) end)::real as boost
    from person p
    left join person_recency r on r.person_id = p.id
    cross join input q
    where (scope is null or 'person' = any(scope)) and p.search_vector @@ q.prefix
    union all
    select null, c, null, null, null, c.id, app_private.normalize_name(c.name), c.search_vector,
      case when c.id = any((select current_couple_ids())::bigint[]) then 20 else 0 end
        + 10 / (1 + greatest(0, extract(epoch from now() - coalesce(c.until, 'infinity'::timestamptz))) / 86400 / 365)
    from couple c
    cross join input q
    where (scope is null or 'couple' = any(scope)) and c.search_vector @@ q.prefix
    union all
    select null, null, c, null, null, c.id, app_private.normalize_name(c.name), c.search_vector,
      case when c.id = any((select current_cohort_ids())::bigint[]) then 20 else 0 end
        + case when not c.is_archived then 10 else 0 end
    from cohort c, input q
    where (scope is null or 'cohort' = any(scope)) and c.search_vector @@ q.prefix
    union all
    select null, null, null, l, null, l.id, app_private.normalize_name(l.name), l.search_vector, 0
    from location l, input q
    where (scope is null or 'location' = any(scope)) and l.search_vector @@ q.prefix
    union all
    select null, null, null, null, e::event, e.id,
      app_private.normalize_name(e.name), e.search_vector,
      case when e.manager_person_ids && (select current_person_ids())
        or (event_registration_info(e::event)).my then 20 else 0 end
      -- Upcoming/ongoing: up to 10; past: up to 5. Both decay over a 90-day scale.
      + (case when e.until >= now()
        then 10 / (1 + greatest(0, extract(epoch from e.since - now())) / 86400 / 90)
        else 5 / (1 + extract(epoch from now() - e.until) / 86400 / 90) end)::real
    from event_matches e
  ), scored as (
    select c.*,
      case when btrim(regexp_replace(c.normalized_label, '\s+', ' ', 'g')) = q.text then 0
        when c.search_vector @@ q.words then 1 else 2 end as match_tier,
      (c.boost + 5 * ts_rank(c.search_vector, q.prefix, 32)
        - case when (c.event).type = 'lesson' then 20 else 0 end)::real as score
    from candidates c, input q
  )
  select person, couple, cohort, location, event, score,
    row_number() over (order by match_tier, score desc, id)::integer
  from scored
  order by match_tier, score desc, id
  limit greatest(0, least(coalesce("limit", 20), 20));
$$;

comment on function global_search is '@behavior +list -connection
@notNull';
grant execute on function global_search to anonymous;
