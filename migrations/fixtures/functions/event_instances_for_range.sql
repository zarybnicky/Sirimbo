do $$
begin
  create type event_instance_range_scope as enum ('all', 'top_level', 'mine', 'relevant');
exception when duplicate_object then null;
end
$$;

drop function if exists event_instances_for_range;

create or replace function event_instances_for_range(
  only_type event_type,
  start_range timestamptz,
  end_range timestamptz = null,
  trainer_ids bigint[] = null,
  participant_ids bigint[] = null,
  parent_id bigint = null,
  scope event_instance_range_scope = 'all',
  location_ids bigint[] = null
) returns setof event_instance as $$
  with mine as (
    select instance_id from event_instance_registration
    where person_id = any (current_person_ids()) and registration_status = 'active'
    union all
    select instance_id from event_instance_trainer
    where person_id = any (current_person_ids())
  )
  select i.*
  from event_instance i
  where i.tenant_id = current_tenant_id()
    and (only_type is null or i.type = only_type)
    and case
      when $6 is not null then i.parent_id = $6
        and (scope <> 'mine' or i.id in (select instance_id from mine))
      when scope = 'all' then true
      when scope = 'top_level' then i.parent_id is null
      when scope = 'mine' then i.id in (select instance_id from mine)
      when scope = 'relevant' then i.parent_id is null
        or i.id in (select instance_id from mine)
        or i.parent_id in (select instance_id from mine)
    end
    and i.since < coalesce(end_range, 'infinity'::timestamptz)
    and i.until > start_range
    and (trainer_ids is null
      or exists (select 1 from event_instance_trainer where instance_id = i.id and person_id = any (trainer_ids)))
    and (participant_ids is null
      or exists (select 1 from event_instance_registration where instance_id = i.id and person_id = any (participant_ids) and registration_status = 'active'))
    and (location_ids is null or i.location_id = any (location_ids))
  order by i.since
  ;
$$ stable language sql;

comment on function event_instances_for_range is '@simpleCollections only';
grant all on function event_instances_for_range to anonymous;
