/* @name CampCloneEvents */
with recursive tree as (
  select id, parent_id, since, 0 as depth
  from event
  where id = :sourceId! and type = 'camp'
  union all
  select child.id, child.parent_id, child.since, parent.depth + 1
  from event child join tree parent on child.parent_id = parent.id
)
select id::text as "id!", parent_id::text as parent_id,
  (:since!::timestamptz - (select since from tree where depth = 0))::text as "shift!"
from tree
order by depth, since, id;

/* @name CloneCampEvent */
select saved.id::text as "id!"
from event source
cross join lateral save_events(
  details => jsonb_populate_record(
    null::event_details_input,
    to_jsonb(source) || jsonb_build_object(
      'parent_id', :parentId::bigint,
      'name', case when source.type = 'lesson' then null else source.name end,
      'capacity', coalesce(source.capacity, 0),
      'is_locked', null
    )),
  events => array[
    row(null, source.since + :shift!::text::interval,
    source.until + :shift!::text::interval,
    false, '{}'::event_registration_input[])::event_input
  ],
  trainers => case when :copyTrainers!::boolean then array(
    select row(person_id, lessons_offered)::event_trainer_input
    from event_trainer where event_id = source.id
  ) else '{}'::event_trainer_input[] end,
  cohort_ids => case when :copyCohorts!::boolean then array(
    select cohort_id from event_target_cohort where event_id = source.id
  ) else '{}'::bigint[] end
) saved
where source.id = :sourceId!;

/* @name CopyEventMetadata */
update event copy
set summary = source.summary,
    description = source.description,
    cover_image_id = source.cover_image_id
from event source where copy.id = :copyId! and source.id = :sourceId!;

/* @name PopulateEventCohorts */
select app_private.reconcile_event_cohort_registrations(:ids!::bigint[]);

/* @name EventSeriesEvents */
select i.id from event_series s join event i on s.id = i.series_id where s.id = :id;

/* @name DeleteEventRecursive */
with recursive tree as (
  select id from event where id = :id!
  union all
  select child.id from event child join tree parent on child.parent_id = parent.id
)
delete from event where id in (select id from tree)
returning id::text as "id!";
