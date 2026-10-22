CREATE FUNCTION public.event_trainers(v_instance public.event) RETURNS SETOF public.tenant_trainer
    LANGUAGE sql STABLE
    AS $$
select * from app_private.event_trainers_at(v_instance, v_instance.since);
$$;

COMMENT ON FUNCTION public.event_trainers(v_instance public.event) IS '@simpleCollections only
@deprecated Use eventTrainersList.';

GRANT ALL ON FUNCTION public.event_trainers(v_instance public.event) TO anonymous;
