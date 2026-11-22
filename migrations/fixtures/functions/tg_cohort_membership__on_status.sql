create or replace function app_private.tg_cohort_membership__on_status()
  returns trigger language plpgsql
  security definer
  set search_path = pg_catalog, public, app_private, pg_temp
  as $$
declare
  event_ids bigint[];
  person_ids bigint[];
begin
  if tg_op = 'UPDATE' and (old.tenant_id, old.person_id, old.cohort_id, old.status) is not distinct from
                         (new.tenant_id, new.person_id, new.cohort_id, new.status) then
    return null;
  end if;

  select array_agg(distinct target.event_id)
  into event_ids
  from event_target_cohort target
  join event instance on instance.id = target.event_id
  where target.cohort_id = any (
    case when tg_op = 'INSERT' then array[new.cohort_id]
      when tg_op = 'DELETE' then array[old.cohort_id]
      else array[old.cohort_id, new.cohort_id]
    end
  ) and instance.since >= now();

  if event_ids is not null then
    person_ids := case when tg_op = 'INSERT' then array[new.person_id]
      when tg_op = 'DELETE' then array[old.person_id]
      else array[old.person_id, new.person_id]
    end;
    perform app_private.reconcile_event_cohort_registrations(event_ids, person_ids);
  end if;

  if tg_op = 'UPDATE' and (old.tenant_id, old.person_id) is distinct from (new.tenant_id, new.person_id) then
    if (old.tenant_id, old.person_id) < (new.tenant_id, new.person_id) then
      perform app_private.rebuild_tenant_relation(old.tenant_id, old.person_id);
      perform app_private.rebuild_tenant_relation(new.tenant_id, new.person_id);
    else
      perform app_private.rebuild_tenant_relation(new.tenant_id, new.person_id);
      perform app_private.rebuild_tenant_relation(old.tenant_id, old.person_id);
    end if;
  elsif tg_op = 'DELETE' then
    perform app_private.rebuild_tenant_relation(old.tenant_id, old.person_id);
  else
    perform app_private.rebuild_tenant_relation(new.tenant_id, new.person_id);
  end if;
  return null;
end;
$$;

grant all on function app_private.tg_cohort_membership__on_status to trainer;

create or replace trigger _500_on_status
  after insert or update or delete on cohort_membership
  for each row execute function app_private.tg_cohort_membership__on_status();
