drop function if exists event_instances_for_range;

create or replace function event_instances_for_range(
  only_type event_type,
  start_range timestamptz,
  end_range timestamptz = null,
  trainer_ids bigint[] = null,
  participant_ids bigint[] = null,
  only_mine boolean = false,
  any_parent boolean = false,
  parent_id bigint = null
) returns setof event_instance as $$
  select i.*
  from event_instance i
  where i.tenant_id = current_tenant_id()
    and (only_type is null or i.type = only_type)
    and (any_parent or i.parent_id is not distinct from $8)
    and i.since < coalesce(end_range, 'infinity'::timestamptz)
    and i.until > start_range
    and (trainer_ids is null
      or exists (select 1 from event_instance_trainer where instance_id = i.id and person_id = any (trainer_ids)))
    and (participant_ids is null
      or exists (select 1 from event_instance_registration where instance_id = i.id and person_id = any (participant_ids) and registration_status = 'active'))
    and (only_mine is false
      or exists (select 1 from event_instance_registration where instance_id = i.id and person_id = any ((select current_person_ids())::bigint[]) and registration_status = 'active')
      or i.manager_person_ids && ((select current_person_ids())::bigint[]));
$$ stable language sql;

COMMENT ON FUNCTION event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION event_instances_for_range TO anonymous;
