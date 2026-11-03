CREATE FUNCTION public.event_trainers(v_instance public.event) RETURNS SETOF public.tenant_trainer
    LANGUAGE sql STABLE
    AS $$

COMMENT ON FUNCTION public.event_trainers(v_instance public.event) IS '@behavior +list -connection
@deprecated Use eventTrainersList.';

GRANT ALL ON FUNCTION public.event_trainers(v_instance public.event) TO anonymous;
