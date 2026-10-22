CREATE FUNCTION public.event_trainer_tenant_trainer(trainer public.event_trainer) RETURNS public.tenant_trainer
    LANGUAGE sql STABLE
    AS $$
  select tt.*
  from event e
  join tenant_trainer tt on tt.tenant_id = e.tenant_id
    and tt.person_id = trainer.person_id
    and tt.active_range @> e.since
  where e.id = trainer.event_id and e.tenant_id = trainer.tenant_id;
$$;

COMMENT ON FUNCTION public.event_trainer_tenant_trainer(trainer public.event_trainer) IS '@behavior -queryField';

GRANT ALL ON FUNCTION public.event_trainer_tenant_trainer(trainer public.event_trainer) TO anonymous;
