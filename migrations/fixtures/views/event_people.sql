
create materialized view if not exists app_private.event_people as
with recursive attended(tenant_id, viewer_person_id, instance_id) as (
  select tenant_id, person_id, instance_id
  from event_instance_registration
  where registration_status = 'active' and person_id is not null
  union
  select parent.tenant_id, parent.viewer_person_id, child.id
  from attended parent
  join event_instance child
    on child.parent_id = parent.instance_id and child.tenant_id = parent.tenant_id
  where child.parent_id is not null
), participants as (
  select tenant_id, instance_id, person_id from event_instance_registration
  where registration_status = 'active' and person_id is not null
  union all
  select tenant_id, instance_id, person_id from event_instance_trainer
)
select a.tenant_id, a.viewer_person_id, p.person_id as visible_person_id
from attended a join participants p using (tenant_id, instance_id)
group by a.tenant_id, a.viewer_person_id, p.person_id;

create unique index if not exists event_people_key
  on app_private.event_people(tenant_id, viewer_person_id, visible_person_id);
