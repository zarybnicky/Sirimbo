CREATE FUNCTION app_private.tg_cohort_membership__on_status() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'app_private'
    AS $$
declare
  instance_ids bigint[];
  person_ids bigint[];
begin
  select array_agg(distinct target.instance_id)
  into instance_ids
  from event_instance_target_cohort target
  join event_instance instance on instance.id = target.instance_id
  where target.cohort_id = any (
    case when tg_op = 'INSERT' then array[new.cohort_id]
      when tg_op = 'DELETE' then array[old.cohort_id]
      else array[old.cohort_id, new.cohort_id]
    end
  ) and instance.since >= now();

  if instance_ids is not null then
    person_ids := case when tg_op = 'INSERT' then array[new.person_id]
      when tg_op = 'DELETE' then array[old.person_id]
      else array[old.person_id, new.person_id]
    end;
    perform app_private.reconcile_event_instance_cohort_registrations(instance_ids, person_ids);
  end if;

  return case when tg_op = 'DELETE' then old else new end;
end;
$$;

GRANT ALL ON FUNCTION app_private.tg_cohort_membership__on_status() TO trainer;
