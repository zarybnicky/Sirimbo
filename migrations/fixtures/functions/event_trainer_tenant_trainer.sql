drop function if exists event_instance_trainer_tenant_trainer;

create or replace function event_trainer_tenant_trainer(trainer event_trainer)
  returns tenant_trainer
  language sql stable
as $$
  select tt.*
  from event e
  join tenant_trainer tt on tt.tenant_id = e.tenant_id
    and tt.person_id = trainer.person_id
    and tt.active_range @> e.since
  where e.id = trainer.event_id and e.tenant_id = trainer.tenant_id;
$$;

comment on function event_trainer_tenant_trainer is '@behavior -queryField';
grant execute on function event_trainer_tenant_trainer to anonymous;
