CREATE FUNCTION public.suggested_competition_matches(since date, until date DEFAULT NULL::date, person_ids bigint[] DEFAULT NULL::bigint[], couple_id bigint DEFAULT NULL::bigint, cohort_id bigint DEFAULT NULL::bigint, event_ids bigint[] DEFAULT NULL::bigint[], registered_only boolean DEFAULT false) RETURNS TABLE(competition_id bigint, competitor_id text, local_person_id bigint, local_couple_id bigint, status text)
    LANGUAGE sql STABLE
    AS $$
  with scoped_people as materialized (
    select p.id
    from person p
    where (person_ids is not null or couple_id is not null or p.id in (
        select person_id from current_tenant_membership
        union all
        select person_id from current_tenant_trainer
        union all
        select person_id from current_tenant_administrator
      ))
      and (person_ids is null or p.id = any(person_ids))
      and (cohort_id is null or exists (
        select 1 from current_cohort_membership cm
        where cm.person_id = p.id and cm.cohort_id = suggested_competition_matches.cohort_id
      ))
      and (couple_id is null or exists (
        select 1 from couple c
        where c.id = suggested_competition_matches.couple_id and p.id in (c.man_id, c.woman_id)
      ))
  ), scoped_couples as materialized (
    select c.id
    from couple c
    where c.status = 'active'
      and (couple_id is null or c.id = couple_id)
      and (c.man_id in (select id from scoped_people) or c.woman_id in (select id from scoped_people))
  ), scoped_competitors as materialized (
    select fc.id, fc.federation, fc.competitor_type, array(
      select distinct case
        when license.discipline::text like 'pd_%' then 'Professional'
        when license.discipline = 'hiphop' then 'HipHop'
        when license.discipline in ('breaking', 'caribbean', 'stage', 'smooth', 'disco')
          then initcap(license.discipline::text)
        else 'General'
      end
      from federated.person_license license
      where license.person_id = fp.id and license.federation = 'wdsf'
        and license.kind = 'athlete' and license.discipline <> 'unknown'
    ) as divisions, fp.age_group_code, link.person_id as local_person_id, null::bigint as local_couple_id
    from person_federation_link link
    join federated.person fp on (fp.federation, fp.external_id) = (link.federation, link.external_id)
    join federated.competitor_component member on member.person_id = fp.id
    join federated.competitor fc on fc.id = member.competitor_id
    where couple_id is null and fc.competitor_type = 'solo'
      and link.person_id = any(array(select id from scoped_people))
    union
    select fc.id, fc.federation, fc.competitor_type, fc.divisions, fc.age_group_code,
      null::bigint, link.couple_id
    from couple_federation_link link
    join federated.competitor fc on fc.id = link.competitor_id
    where link.couple_id = any(array(select id from scoped_couples))
  ), profiles as materialized (
    select sc.local_person_id, sc.local_couple_id, sc.id, sc.federation, sc.competitor_type, sc.divisions, sc.age_group_code,
      count(*) = count(coalesce(extract(year from p.birth_date)::int, extract(year from fp.dob)::int, fp.birth_year)) as known_birth_years,
      min(coalesce(extract(year from p.birth_date)::int, extract(year from fp.dob)::int, fp.birth_year)) as older_birth_year,
      max(coalesce(extract(year from p.birth_date)::int, extract(year from fp.dob)::int, fp.birth_year)) as younger_birth_year,
      bool_and(coalesce(fp.gender = 'female', false)) as all_female,
      bool_and(coalesce(fp.gender = 'male', false)) as all_male
    from scoped_competitors sc
    join federated.competitor_component cc on cc.competitor_id = sc.id
    join federated.person fp on fp.id = cc.person_id
    left join person_federation_link link on (link.federation, link.external_id) = (fp.federation, fp.external_id)
    left join person p on p.id = link.person_id
    group by sc.local_person_id, sc.local_couple_id, sc.id, sc.federation, sc.competitor_type, sc.divisions, sc.age_group_code
  ), upcoming as materialized (
    select array_agg(c.id) as competition_ids, c.federation, c.competition_type, extract(year from c.start_date)::int as year,
      cat.series, cat.discipline, cat.class, cat.gender_group, cat.competitor_type, cat.age_group_code
    from federated.competition c
    join federated.event e on e.id = c.event_id
    join federated.category cat on cat.id = c.category_id
    where c.federation in ('csts', 'wdsf') and c.start_date >= since and (until is null or c.start_date < until)
      and (event_ids is null or c.event_id = any(event_ids))
      and c.status <> 'cancelled' and e.status <> 'cancelled'
    group by c.federation, c.competition_type, extract(year from c.start_date)::int, cat.id
  ), participations as materialized (
    select p.*
    from federated.competition_participation p
    where p.competition_id = any(array(select unnest(competition_ids) from upcoming))
      and p.competitor_id = any(array(select id from scoped_competitors))
  ), source_profiles as materialized (
    select p.*, source.series, source.discipline, source.class, source.age_group_code as reported_age
    from profiles p
    join federated.competitor_category_progress progress on progress.competitor_id = p.id
    join federated.category source on source.id = progress.category_id
      and source.competitor_type = p.competitor_type
    where p.federation = 'csts'
    union all
    -- WDSF divisions are independent of ČSTS proficiency classes and progress.
    select p.*, division, null, null, p.age_group_code
    from profiles p cross join unnest(p.divisions) division
    where p.federation = 'wdsf'
  ), category_profiles as materialized (
    select p.*, y.year, age.code as age_group, age.allowed_to_dance_in
    from source_profiles p
    cross join (select distinct year from upcoming) y
    left join lateral (
      select r.age_group_code as code
      from federated.couple_age_rule r
      where p.competitor_type = 'couple' and p.known_birth_years
        and r.division = 'General' and r.age_group_code <> 'Under_21'
        and r.older_age @> (y.year - p.older_birth_year)
        and r.younger_age @> (y.year - p.younger_birth_year)
      order by lower(r.older_age) desc nulls last, upper(r.older_age)
      limit 1
    ) couple_age on true
    left join federated.age_group person_age on p.competitor_type = 'solo' and p.known_birth_years
      and person_age.person_age @> (y.year - p.older_birth_year)
    join federated.age_group age on age.code = case
      when p.known_birth_years then coalesce(couple_age.code, person_age.code)
      when y.year = extract(year from current_date)::int then p.reported_age
    end
  ), eligible as (
  select distinct competition_id, p.id, p.local_person_id, p.local_couple_id
  from upcoming target
  join category_profiles p on p.year = target.year and p.federation = target.federation
    and (p.series = target.series or (p.federation = 'wdsf' and target.series = 'Any'))
    and (p.federation = 'wdsf' or p.discipline = target.discipline)
    and p.competitor_type = target.competitor_type
  cross join lateral unnest(target.competition_ids) competition_id
  where (target.gender_group in ('', 'mixed', 'SOLO')
      or (target.gender_group in ('female', 'F', '1 F') and p.all_female)
      or (target.gender_group in ('male', 'M', '1 M') and p.all_male))
    -- ČSTS leagues require B or higher, even though their categories say Open.
    and (p.federation <> 'csts' or target.competition_type not in ('league', 'super_league')
      or p.class in ('B', 'A', 'M', 'S'))
    and (p.federation = 'wdsf' or
      target.class in ('', 'Open') or target.class = p.class
      or (federated.class_rank(p.class) > 0
        and federated.class_rank(p.class) = federated.class_rank(target.class))
      or (target.class like '%-%'
        and federated.class_rank(split_part(target.class, '-', 1)) > 0
        and federated.class_rank(split_part(target.class, '-', 2)) > 0
        -- A range cannot cross between medal and letter classes.
        and (federated.class_rank(split_part(target.class, '-', 1)) > 10)
          = (federated.class_rank(split_part(target.class, '-', 2)) > 10)
        and federated.class_rank(p.class) between
          federated.class_rank(split_part(target.class, '-', 1)) and
          federated.class_rank(split_part(target.class, '-', 2)))
    )
    and (target.age_group_code = p.age_group or target.age_group_code = any(p.allowed_to_dance_in))
    -- Adult's allowances include U21; that is only valid below its upper age limit.
    and (target.age_group_code <> 'Under_21' or
      (p.known_birth_years and target.year - p.older_birth_year < 21) or
      (not p.known_birth_years and p.age_group in ('Youth', 'Under_21')))
  )
  select eligible.*, case
    when participation.cancelled then 'cancelled'
    when participation.competition_id is not null then 'registered'
    else 'suggested'
  end
  from eligible
  left join participations participation
    on (participation.competition_id, participation.competitor_id) = (eligible.competition_id, eligible.id)
  where not registered_only
  union
  select participation.competition_id, sc.id, sc.local_person_id, sc.local_couple_id,
    case when participation.cancelled then 'cancelled' else 'registered' end
  from scoped_competitors sc
  join participations participation on participation.competitor_id = sc.id
  where not registered_only or not participation.cancelled;
$$;

COMMENT ON FUNCTION public.suggested_competition_matches(since date, until date, person_ids bigint[], couple_id bigint, cohort_id bigint, event_ids bigint[], registered_only boolean) IS '@behavior -*';

GRANT ALL ON FUNCTION public.suggested_competition_matches(since date, until date, person_ids bigint[], couple_id bigint, cohort_id bigint, event_ids bigint[], registered_only boolean) TO anonymous;
