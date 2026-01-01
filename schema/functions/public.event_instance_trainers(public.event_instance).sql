CREATE FUNCTION public.event_instance_trainers(v_instance public.event_instance) RETURNS SETOF public.tenant_trainer
    LANGUAGE sql STABLE
    AS $$
select * from app_private.event_instance_trainers_at(v_instance, v_instance.since);
$$;

COMMENT ON FUNCTION public.event_instance_trainers(v_instance public.event_instance) IS '@simpleCollections only';

GRANT ALL ON FUNCTION public.event_instance_trainers(v_instance public.event_instance) TO anonymous;
