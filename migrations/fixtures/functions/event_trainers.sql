create or replace function event_trainers(v_instance event)
  returns setof tenant_trainer
  language sql stable
as $$
select tt.*
from event_trainer k
join tenant_trainer tt on tt.tenant_id = k.tenant_id and tt.person_id = k.person_id
where k.event_id = v_instance.id
  and tt.active_range @> v_instance.since;
$$;

grant all on function event_trainers to anonymous;
comment on function event_trainers is '@behavior +list -connection
@deprecated Use eventTrainersList.';
