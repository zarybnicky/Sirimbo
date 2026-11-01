create or replace function event_trainers(v_instance event)
  returns setof tenant_trainer
  language sql stable
as $$
-- Compatibility for external clients; internal callers use event assignments.
select tt.*
from event_trainer assignment
join tenant_trainer tt on tt.tenant_id = assignment.tenant_id
  and tt.person_id = assignment.person_id
  and tt.active_range @> v_instance.since
where assignment.event_id = v_instance.id;
$$;

grant all on function event_trainers to anonymous;
comment on function event_trainers is '@behavior +list -connection
@deprecated Use eventTrainersList.';
