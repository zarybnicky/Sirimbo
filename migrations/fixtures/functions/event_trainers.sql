drop function if exists event_instance_trainers;
drop function if exists app_private.event_instance_trainers_at;

create or replace function app_private.event_trainers_at(
  v_instance event,
  v_at timestamptz
)
  returns setof tenant_trainer
  language sql stable
as $$
select distinct on (tt.tenant_id, tt.person_id) tt.*
from event_trainer k
join tenant_trainer tt on tt.tenant_id = k.tenant_id and tt.person_id = k.person_id
where k.event_id = v_instance.id
  and tt.active_range @> v_at
order by tt.tenant_id, tt.person_id, lower(tt.active_range) desc;
$$;
grant all on function app_private.event_trainers_at to anonymous;

create or replace function event_trainers(v_instance event)
  returns setof tenant_trainer
  language sql stable
as $$
select * from app_private.event_trainers_at(v_instance, v_instance.since);
$$;

grant all on function event_trainers to anonymous;
comment on function event_trainers is '@simpleCollections only
@deprecated Use eventTrainersList.';
