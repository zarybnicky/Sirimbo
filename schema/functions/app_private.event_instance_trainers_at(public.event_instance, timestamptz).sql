CREATE FUNCTION app_private.event_instance_trainers_at(v_instance public.event_instance, v_at timestamp with time zone) RETURNS SETOF public.tenant_trainer
    LANGUAGE sql STABLE
    AS $$
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

GRANT ALL ON FUNCTION app_private.event_instance_trainers_at(v_instance public.event_instance, v_at timestamp with time zone) TO anonymous;
