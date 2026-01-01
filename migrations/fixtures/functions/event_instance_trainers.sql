create or replace function app_private.event_instance_trainers_at(
  v_instance event_instance,
  v_at timestamptz
)
  returns setof tenant_trainer
  language sql stable
as $$
select distinct on (tt.tenant_id, tt.person_id) tt.*
from (
  select eit.tenant_id, eit.person_id
  from event_instance_trainer eit
  where eit.instance_id = v_instance.id

  union all

  select et.tenant_id, et.person_id
  from event_trainer et
  where et.event_id = v_instance.event_id
    and not exists (select 1 from event_instance_trainer eit2 where eit2.instance_id = v_instance.id)
) k
join tenant_trainer tt on tt.tenant_id = k.tenant_id and tt.person_id = k.person_id
where tt.active_range @> v_at
order by tt.tenant_id, tt.person_id, lower(tt.active_range) desc;
$$;
grant all on function app_private.event_instance_trainers_at to anonymous;

create or replace function public.event_instance_trainers(v_instance event_instance)
  returns setof tenant_trainer
  language sql stable
as $$
select * from app_private.event_instance_trainers_at(v_instance, v_instance.since);
$$;

grant all on function event_instance_trainers(event_instance) to anonymous;
comment on function event_instance_trainers(event_instance) is '@simpleCollections only';
