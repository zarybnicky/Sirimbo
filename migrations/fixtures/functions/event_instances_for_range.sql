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
  only_mine boolean = false,
  any_parent boolean = true,
  parent_id bigint = null,
  scope event_instance_range_scope = null
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
  cross join (values (coalesce($9, case
    when only_mine then 'mine'
    when not any_parent then 'top_level'
    else 'all'
  end::event_instance_range_scope))) args(scope)
  where i.tenant_id = current_tenant_id()
    and (only_type is null or i.type = only_type)
    and case
      when $8 is not null then i.parent_id = $8
        and (args.scope <> 'mine' or i.id in (select instance_id from mine))
      when args.scope = 'all' then true
      when args.scope = 'top_level' then i.parent_id is null
      when args.scope = 'mine' then i.id in (select instance_id from mine)
      when args.scope = 'relevant' then i.parent_id is null
        or i.id in (select instance_id from mine)
        or i.parent_id in (select instance_id from mine)
    end
    and i.since < coalesce(end_range, 'infinity'::timestamptz)
    and i.until > start_range
    and (trainer_ids is null
      or exists (select 1 from event_instance_trainer where instance_id = i.id and person_id = any (trainer_ids)))
    and (participant_ids is null
      or exists (select 1 from event_instance_registration where instance_id = i.id and person_id = any (participant_ids) and registration_status = 'active'))
  ;
$$ stable language sql;

comment on function event_instances_for_range is '@simpleCollections only';
grant all on function event_instances_for_range to anonymous;
