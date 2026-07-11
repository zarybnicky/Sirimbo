CREATE FUNCTION app_private.event_instance_trainers_at(v_instance public.event_instance, v_at timestamp with time zone) RETURNS SETOF public.tenant_trainer
    LANGUAGE sql STABLE
    AS $$
select distinct on (tt.tenant_id, tt.person_id) tt.*
from event_instance_trainer k
join tenant_trainer tt on tt.tenant_id = k.tenant_id and tt.person_id = k.person_id
where k.instance_id = v_instance.id
  and tt.active_range @> v_at
order by tt.tenant_id, tt.person_id, lower(tt.active_range) desc;
$$;

GRANT ALL ON FUNCTION app_private.event_instance_trainers_at(v_instance public.event_instance, v_at timestamp with time zone) TO anonymous;
