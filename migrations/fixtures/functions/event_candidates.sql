-- Out of context an event instance is not identified by its name: group lessons
-- usually have none, and the ones that do repeat it hundreds of times. What a
-- trainer remembers is when it was and who was in it, so the text match reaches
-- the trainers, the target cohorts and the location, and without a query the
-- next events come first, then the ones that just finished.
create or replace function event_instance_candidates(query text default null, count int default 10)
  returns setof event_instance
  language sql stable
as $$
  select instance.*
  from event_instance instance
  where not instance.is_cancelled
    and (
      coalesce(query, '') = ''
      or instance.name ilike '%' || query || '%'
      or instance.location_text ilike '%' || query || '%'
      or exists (
        select 1 from tenant_location location
        where location.id = instance.location_id and location.name ilike '%' || query || '%'
      )
      or exists (
        select 1 from event_instance_trainer trainer
        join person on person.id = trainer.person_id
        where trainer.instance_id = instance.id and person.name ilike '%' || query || '%'
      )
      or exists (
        select 1 from event_instance_target_cohort target
        join cohort on cohort.id = target.cohort_id
        where target.instance_id = instance.id and cohort.name ilike '%' || query || '%'
      )
    )
  order by
    instance.since < now(),
    abs(extract(epoch from instance.since - now()))
  limit greatest(coalesce(count, 10), 0);
$$;

comment on function event_instance_candidates(text, int) is '@simpleCollections only';

-- A series, unlike an instance, carries a name someone chose, so it needs no
-- more than that to be recognised.
create or replace function event_series_candidates(query text default null, count int default 10)
  returns setof event_series
  language sql stable
as $$
  select series.*
  from event_series series
  where coalesce(query, '') = '' or series.name ilike '%' || query || '%'
  order by series.name
  limit greatest(coalesce(count, 10), 0);
$$;

comment on function event_series_candidates(text, int) is '@simpleCollections only';

grant execute on function event_instance_candidates(text, int) to anonymous;
grant execute on function event_series_candidates(text, int) to anonymous;
