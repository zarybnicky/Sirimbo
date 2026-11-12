create or replace function global_search(search text, "limit" integer default 20)
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
  ), current_people as (
    -- Membership affects ranking only; the source tables' RLS controls visibility.
    select person_id
    from app_private.person_membership_candidates('current') where in_state
  ), peers as materialized (
    select person_id from current_cohort_membership
    where cohort_id = any((select current_cohort_ids())::bigint[])
  ), event_matches as materialized (
    select e.*
    from event e
    where e.id = any (array(
      select ids.id from input q
      cross join lateral app_private.accessible_event_ids(q.prefix) ids(id)
    ))
  ), candidates as (
    select p as person, null::couple as couple, null::cohort as cohort,
      null::location as location, null::event as event,
      p.id, p.search_name as normalized_label, p.search_vector,
      (case when p.id = any((select current_person_ids())::bigint[]) then 20
        when p.id in (select person_id from peers) then 10 else 0 end
        + case when p.id in (select person_id from current_people) then 10 else 0 end)::real as boost
    from person p
    cross join input q
    where p.search_vector @@ q.prefix
    union all
    select null, c, null, null, null, c.id, app_private.normalize_name(c.name), c.search_vector,
      case when c.id = any((select current_couple_ids())::bigint[]) then 20 else 0 end
        + case when c.status = 'active' then 10 else 0 end
    from couple c
    cross join input q
    where c.search_vector @@ q.prefix
    union all
    select null, null, c, null, null, c.id, app_private.normalize_name(c.name), c.search_vector,
      case when c.id = any((select current_cohort_ids())::bigint[]) then 20 else 0 end
        + case when not c.is_archived then 10 else 0 end
    from cohort c, input q
    where c.search_vector @@ q.prefix
    union all
    select null, null, null, l, null, l.id, app_private.normalize_name(l.name), l.search_vector, 0
    from location l, input q
    where l.search_vector @@ q.prefix
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
