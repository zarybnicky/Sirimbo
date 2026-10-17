
create materialized view if not exists app_private.event_people as
with recursive participants as (
  select tenant_id, instance_id,
    array_agg(distinct person_id order by person_id) filter (where is_attendee) as viewers,
    array_agg(distinct person_id order by person_id) as people
  from (
    select tenant_id, instance_id, person_id, true as is_attendee
    from event_instance_registration
    where registration_status = 'active' and person_id is not null
    union all
    select tenant_id, instance_id, person_id, false
    from event_instance_trainer
  ) p
  group by tenant_id, instance_id
), attended(tenant_id, viewers, instance_id) as (
  select tenant_id, viewers, instance_id from participants where viewers is not null
  union
  select parent.tenant_id, parent.viewers, child.id
  from attended parent
  join event_instance child
    on child.parent_id = parent.instance_id and child.tenant_id = parent.tenant_id
  where child.parent_id is not null
), rosters as (
  -- Repeated events with the same people need only one expansion into person pairs.
  select distinct a.tenant_id, a.viewers, p.people
  from attended a join participants p using (tenant_id, instance_id)
)
select distinct tenant_id, viewer_person_id, visible_person_id
from rosters
cross join lateral unnest(viewers) v(viewer_person_id)
cross join lateral unnest(people) p(visible_person_id);

create unique index if not exists event_people_key
  on app_private.event_people(tenant_id, viewer_person_id, visible_person_id);
