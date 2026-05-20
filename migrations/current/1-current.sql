
CREATE or replace FUNCTION public.person_csts_candidates(in_person public.person, "limit" integer DEFAULT 10, threshold real DEFAULT 0.4) RETURNS TABLE(id integer, name text, age_group text, similarity real)
    LANGUAGE sql STABLE
    AS $_$
  with params as (
    select
      greatest(0.3::real, least(coalesce(threshold, 0.4), 1::real)) as effective_threshold,
      greatest(0, least(coalesce($2, 10), 50)) as effective_limit
  ),
  scored_candidates as (
    select
      fp.external_id::integer as id,
      fp.canonical_name as name,
      fp.age_group,
      score.name_score,
      case
        when in_person.birth_date is not null and fp.dob is not null and fp.dob = in_person.birth_date then 1
        else 0
      end as dob_score
      -- Future year-of-birth range scoring belongs here once federated.person.yob_range exists.
    from params
    join federated.person fp on true
    cross join lateral (
      select public.similarity(fp.search_name, in_person.search_name) as name_score
    ) score
    where in_person.search_name is not null
      and fp.federation = 'csts'
      and fp.external_id between 0 and 2147483647
      and fp.search_name % in_person.search_name
      and score.name_score >= params.effective_threshold
  )
  select scored_candidates.id, scored_candidates.name, scored_candidates.age_group, scored_candidates.name_score
  from scored_candidates
  order by
    scored_candidates.dob_score desc,
    scored_candidates.name_score desc,
    scored_candidates.name,
    scored_candidates.id
  limit (select effective_limit from params);
$_$;

COMMENT ON FUNCTION public.person_csts_candidates(in_person public.person, "limit" integer, threshold real) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.person_csts_candidates(in_person public.person, "limit" integer, threshold real) TO anonymous;

alter table federated.event
  add column if not exists venue_lat double precision,
  add column if not exists venue_lng double precision,
  add column if not exists venue_location_source text,
  add column if not exists venue_location_ref text;
