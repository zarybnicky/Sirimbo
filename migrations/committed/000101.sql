--! Previous: sha1:f3e38a73169106d53ff812be6f3deb87c5577cd6
--! Hash: sha1:1169ec974719321231773f9e804d8cda503e2516

--! split: 1-current.sql
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

create or replace function federated.class_rank(class text) returns int language sql immutable as $$
  select case class
    when 'M' then 16
    when 'S' then 16
    when 'A' then 15
    when 'B' then 14
    when 'C' then 13
    when 'D' then 12
    when 'E' then 11
    when 'Gold' then 4
    when 'Silver' then 3
    when 'Bronze' then 2
    when 'Novice' then 1
    else 0
  end;
$$;

alter table event_trainer
  alter column lessons_offered drop not null,
  alter column lessons_offered set default 0;

update event_trainer et
set lessons_offered = null
from event e
where e.id = et.event_id
  and e.type in ('camp', 'reservation', 'holiday')
  and et.lessons_offered = 0;

--! Included functions/event_remaining_x.sql
CREATE or replace FUNCTION event_trainer_lessons_remaining(e event_trainer) RETURNS integer AS $$
  select case
    when e.lessons_offered is null then null
    else e.lessons_offered - (
      select coalesce(sum(lesson_count), 0)
      from event_lesson_demand where trainer_id = e.id
    )
  end;
$$ LANGUAGE sql STABLE;

CREATE or replace FUNCTION event_remaining_person_spots(e event) RETURNS integer AS $$
  select e.capacity - (
    select coalesce(sum(case when couple_id is not null then 2 else 1 end), 0)
    from event_registration where event_id = e.id
  ) - (
    select coalesce(count(id), 0)
    from event_external_registration where event_id = e.id
  );
$$ LANGUAGE sql STABLE security definer;

CREATE or replace FUNCTION event_remaining_lessons(e event) RETURNS integer AS $$
  select case
    when exists (
      select 1 from event_trainer et
      where et.event_id = e.id and et.lessons_offered is null
    ) then null
    else (
      select coalesce(sum(lessons_offered), 0) from event_trainer et where et.event_id = e.id
    ) - (
      select coalesce(sum(lesson_count), 0) from event_registration er join event_lesson_demand eld on eld.registration_id = er.id where er.event_id = e.id
    )
  end;
$$ LANGUAGE sql STABLE security definer;

GRANT ALL ON FUNCTION event_trainer_lessons_remaining(e event_trainer) TO anonymous;
GRANT ALL ON FUNCTION event_remaining_person_spots(e event) TO anonymous;
GRANT ALL ON FUNCTION event_remaining_lessons(e event) TO anonymous;
--! EndIncluded functions/event_remaining_x.sql
--! Included functions/set_lesson_demand.sql
CREATE or replace FUNCTION set_lesson_demand(registration_id bigint, trainer_id bigint, lesson_count integer) RETURNS event_lesson_demand
  LANGUAGE plpgsql STRICT SECURITY DEFINER
  SET search_path TO pg_catalog, public, pg_temp
AS $$
declare
  v_event event;
  v_trainer event_trainer;
  registration event_registration;
  lesson_demand event_lesson_demand;
  other_lessons bigint;
begin
  select * into registration from event_registration where id = $1;
  select * into v_event from event where id = registration.event_id;
  select * into v_trainer from event_trainer where id = $2 and event_id = registration.event_id;

  if v_event is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if v_event.is_locked = true then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if v_trainer is null then
    raise exception 'TRAINER_NOT_FOUND' using errcode = '28000';
  end if;

  if $3 = 0 then
    delete from event_lesson_demand eld where eld.registration_id = registration.id and eld.trainer_id = $2;
    return null;
  end if;
  if v_trainer.lessons_offered = 0 then
    raise exception 'LESSONS_NOT_OFFERED' using errcode = '28000';
  end if;
  if v_trainer.lessons_offered is not null then
    select coalesce(sum(eld.lesson_count), 0) into other_lessons
    from event_lesson_demand eld
    where eld.trainer_id = $2 and eld.registration_id <> registration.id;

    if $3 > v_trainer.lessons_offered - other_lessons then
      raise exception 'LESSON_LIMIT_EXCEEDED' using errcode = '22023';
    end if;
  end if;

  INSERT INTO event_lesson_demand (registration_id, trainer_id, lesson_count)
  values ($1, $2, $3)
  on conflict on constraint eld_unique_registration_trainer_key do update set lesson_count = $3
  returning * into lesson_demand;

  return lesson_demand;
end;
$$;

select verify_function('set_lesson_demand');
GRANT ALL ON FUNCTION set_lesson_demand(registration_id bigint, trainer_id bigint, lesson_count integer) TO anonymous;
--! EndIncluded functions/set_lesson_demand.sql

CREATE OR REPLACE FUNCTION public.upsert_event(info public.event_type_input, instances public.event_instance_type_input[], trainers public.event_trainer_type_input[], cohorts public.event_target_cohort_type_input[], registrations public.event_registration_type_input[]) RETURNS public.event
    LANGUAGE plpgsql
    AS $$
declare
  instance event_instance_type_input;
  trainer event_trainer_type_input;
  instance_trainer event_instance_trainer_type_input;
  cohort event_target_cohort_type_input;
  registration event_registration_type_input;
  v_event event;
  v_instance_id bigint;
begin
  if info.id is null then

    insert into event (
      name,
      summary,
      description,
      type,
      location_id,
      location_text,
      capacity,
      is_visible,
      is_public,
      is_locked,
      enable_notes
    )
    values (
      info.name,
      info.summary,
      info.description,
      info.type,
      info.location_id,
      info.location_text,
      info.capacity,
      info.is_visible,
      info.is_public,
      info.is_locked,
      info.enable_notes
    )
    returning * into v_event;
  else
    update event set
      name=info.name,
      summary=info.summary,
      description=info.description,
      type=info.type,
      location_id=info.location_id,
      location_text=info.location_text,
      capacity=info.capacity,
      is_visible=info.is_visible,
      is_public=info.is_public,
      is_locked=info.is_locked,
      enable_notes=info.enable_notes
    where id=info.id
    returning * into v_event;

    if not found then
      raise exception 'event % not found', info.id;
    end if;
  end if;

  foreach instance in array coalesce(instances, '{}'::event_instance_type_input[]) loop
    if instance.id is null then
      insert into event_instance (event_id, since, until, is_cancelled)
      values (v_event.id, instance.since, instance.until, instance.is_cancelled)
      returning id into v_instance_id;
    elsif instance.since is null and instance.until is null then
      delete from event_instance where id=instance.id;
      v_instance_id := null;
    else
      update event_instance
      set since=instance.since, until=instance.until, is_cancelled=instance.is_cancelled
      where id=instance.id
      returning id into v_instance_id;
    end if;

    if v_instance_id is not null then
      foreach instance_trainer in array coalesce(instance.trainers, '{}'::event_instance_trainer_type_input[]) loop
        if instance_trainer.id is null then
          insert into event_instance_trainer (instance_id, person_id)
          values (v_instance_id, instance_trainer.person_id)
          on conflict (instance_id, person_id) do nothing;
        elsif instance_trainer.person_id is null then
          delete from event_instance_trainer where id=instance_trainer.id;
        end if;
      end loop;
    end if;
  end loop;

  foreach trainer in array coalesce(trainers, '{}'::event_trainer_type_input[]) loop
    if trainer.id is null then
      insert into event_trainer (event_id, person_id, lessons_offered)
      values (v_event.id, trainer.person_id, trainer.lessons_offered)
      on conflict (event_id, person_id) do update
        set lessons_offered = trainer.lessons_offered;
    elsif trainer.person_id is null then
      delete from event_trainer where id=trainer.id;
    else
      update event_trainer set lessons_offered=trainer.lessons_offered where id=trainer.id;
    end if;
  end loop;

  foreach cohort in array coalesce(cohorts, '{}'::event_target_cohort_type_input[]) loop
    if cohort.id is null then
      insert into event_target_cohort (event_id, cohort_id)
      values (v_event.id, cohort.cohort_id)
      on conflict (event_id, cohort_id) do nothing;
    elsif cohort.cohort_id is null then
      delete from event_target_cohort where id=cohort.id;
    end if;
  end loop;

  foreach registration in array coalesce(registrations, '{}'::event_registration_type_input[]) loop
    if registration.id is null then
      insert into event_registration (event_id, person_id, couple_id)
      values (v_event.id, registration.person_id, registration.couple_id)
      on conflict (event_id, person_id, couple_id) do nothing;
    elsif registration.person_id is null and registration.couple_id is null then
      delete from event_registration where id=registration.id;
    else
      update event_registration
      set person_id=registration.person_id, couple_id=registration.couple_id
      where id=registration.id;
    end if;
  end loop;

  return v_event;
end;
$$;

select verify_function('public.upsert_event');
GRANT ALL ON FUNCTION public.upsert_event(info public.event_type_input, instances public.event_instance_type_input[], trainers public.event_trainer_type_input[], cohorts public.event_target_cohort_type_input[], registrations public.event_registration_type_input[]) TO anonymous;

DROP FUNCTION if exists federated.competition_sankey_links;
create or replace function federated.competition_sankey_links(
    p_first_year integer,
    p_last_year integer,
    p_classes text[],
    p_disciplines text[],
    p_by_school_year boolean default false
) returns table (
    from_year integer,
    from_state text,       -- 'active' | 'inactive'
    from_class text,
    to_year integer,
    to_state text,         -- 'active' | 'inactive'
    to_class text,
    discipline text,
    kind text,             -- 'flow' | 'dropout' | 'inactive' | 'return' | 'entry'
    value integer
) language sql stable as $$
with base as (
    select
        cm.person_id,
        (extract(year from comp.start_date) - case when p_by_school_year and extract(month from comp.start_date) < 9 then 1 else 0 end) as period_year,
        c.discipline as discipline,
        c.class as class,
        federated.class_rank(c.class) as class_rank,
        comp.start_date date,
        comp.category_id,
        cr.competitor_id
    from federated.competition_result cr
    join federated.competition comp on cr.competition_id = comp.id
    join federated.competitor_component cm on cm.competitor_id = cr.competitor_id
    join federated.category c on c.id = comp.category_id
    where (p_disciplines is null or c.discipline = any (p_disciplines))
      and (p_classes is null or c.class = any(p_classes))
),
bounds as (
    select
        coalesce(p_first_year, min(period_year) - 1) as first_year,
        coalesce(p_last_year, max(period_year)) as last_year
    from base
),
observed as (
    select r.person_id, r.period_year, r.discipline, r.class
    from (
        select
            b.*,
            row_number() over (
                partition by b.person_id, b.period_year, b.discipline
                order by b.class_rank desc nulls last, b.date desc nulls last, b.category_id , b.competitor_id
              ) as rn
        from base b, bounds x
        where x.last_year is not null and b.period_year <= x.last_year
    ) r
    where r.rn = 1
),
windowed as (
    select
        o.*,
        row_number() over person_timeline as observed_n,
        lead(o.period_year) over person_timeline as next_year,
        lead(o.class) over person_timeline as next_class
    from observed o
    window person_timeline as (partition by o.person_id, o.discipline order by o.period_year)
),
person_links(from_year, from_state, from_class, to_year, to_state, to_class, discipline, kind) as (
    select
        w.period_year - 1,
        'inactive',
        w.class,
        w.period_year,
        'active',
        w.class,
        w.discipline,
        'entry'
    from windowed w
    where w.observed_n = 1

    union all

    select
        y.from_year,
        case when y.from_year = w.period_year then 'active' else 'inactive' end,
        w.class,
        y.from_year + 1,
        case when w.next_year = y.from_year + 1 then 'active' else 'inactive' end,
        case when w.next_year = y.from_year + 1 then w.next_class else w.class end,
        w.discipline,
        case
            when w.next_year = w.period_year + 1 then 'flow'
            when y.from_year = w.period_year then 'dropout'
            when w.next_year = y.from_year + 1 then 'return'
            else 'inactive'
        end
    from windowed w
    cross join bounds x
    cross join lateral generate_series(w.period_year, coalesce(w.next_year, case when w.period_year < x.last_year then x.last_year else w.period_year end) - 1) as y(from_year)
    where w.next_year is not null or w.period_year < x.last_year
)
select pl.from_year, pl.from_state, pl.from_class, pl.to_year, pl.to_state, pl.to_class, pl.discipline, pl.kind, count(*)::integer as value
from person_links pl, bounds x
where pl.from_year >= x.first_year and pl.to_year <= x.last_year
group by pl.from_year, pl.from_state, pl.from_class, pl.to_year, pl.to_state, pl.to_class, pl.discipline, pl.kind
order by
    pl.from_year,
    pl.discipline,
    coalesce(array_position(array['entry','flow','dropout','inactive','return']::text[], pl.kind), 99),
    pl.from_state,
    pl.from_class,
    pl.to_state,
    pl.to_class;
$$;
