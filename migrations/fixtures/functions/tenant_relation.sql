create or replace function app_private.rebuild_tenant_relation(target_tenant_id bigint, target_person_id bigint)
  returns void
  language plpgsql volatile security definer
  set search_path = pg_catalog, public, pg_temp
as $$
begin
  -- Parent cascades can reach the source tables after deleting the parent.
  if not exists (select from person where id = target_person_id)
    or not exists (select from tenant where id = target_tenant_id) then
    return;
  end if;

  -- Create or lock the persistent pair before reading its source relationships.
  insert into app_private.tenant_relation (tenant_id, person_id)
  values (target_tenant_id, target_person_id)
  on conflict (tenant_id, person_id) do select for update
  returning person_id into target_person_id;

  update app_private.tenant_relation set (current_roles, expired_roles, pending_roles) = (
  select
    coalesce(array_agg(distinct role order by role) filter (where status = 'active'), '{}'),
    coalesce(array_agg(distinct role order by role) filter (where status = 'expired'), '{}'),
    coalesce(array_agg(distinct role order by role) filter (where status = 'pending'), '{}')
  from (
    select 'member'::tenant_role as role, status from tenant_membership
      where tenant_id = target_tenant_id and person_id = target_person_id
    union all
    select 'trainer'::tenant_role, status from tenant_trainer
      where tenant_id = target_tenant_id and person_id = target_person_id
    union all
    select 'administrator'::tenant_role, status from tenant_administrator
      where tenant_id = target_tenant_id and person_id = target_person_id
  ) relationships
  ), (current_cohort_ids, expired_cohort_ids) = (
    select
      coalesce(array_agg(distinct cohort_id order by cohort_id) filter (where status = 'active'), '{}'),
      coalesce(array_agg(distinct cohort_id order by cohort_id) filter (where status = 'expired'), '{}')
    from cohort_membership
    where tenant_id = target_tenant_id and person_id = target_person_id
  ), has_event_participation = exists (
    select from event_registration
    where tenant_id = target_tenant_id and person_id = target_person_id and registration_status = 'active'
    union all
    select from event_trainer
    where tenant_id = target_tenant_id and person_id = target_person_id
  ) where tenant_id = target_tenant_id and person_id = target_person_id;
end;
$$;

create or replace function app_private.tg_tenant_relation__refresh()
  returns trigger
  language plpgsql
  set search_path = pg_catalog, public, pg_temp
as $$
begin
  if tg_op = 'UPDATE' then
    if (old.tenant_id, old.person_id, old.status) is not distinct from
       (new.tenant_id, new.person_id, new.status) then
      return null;
    end if;

    -- Expiry uses coverage at the end date, not the current-role summary.
    if new.status = 'expired' and old.status is distinct from new.status and not exists (
      select from tenant_membership where tenant_id = new.tenant_id and person_id = new.person_id and active_range @> new.until
      union all
      select from tenant_trainer where tenant_id = new.tenant_id and person_id = new.person_id and active_range @> new.until
      union all
      select from tenant_administrator where tenant_id = new.tenant_id and person_id = new.person_id and active_range @> new.until
    ) then
      update cohort_membership
      set status = 'expired', until = new.until
      where tenant_id = new.tenant_id and person_id = new.person_id
        and since < new.until and (until is null or until > new.until);
    end if;

    if (old.tenant_id, old.person_id) is distinct from (new.tenant_id, new.person_id) then
      -- Rebuild both ends in key order when a relationship moves.
      if (old.tenant_id, old.person_id) < (new.tenant_id, new.person_id) then
        perform app_private.rebuild_tenant_relation(old.tenant_id, old.person_id);
        perform app_private.rebuild_tenant_relation(new.tenant_id, new.person_id);
      else
        perform app_private.rebuild_tenant_relation(new.tenant_id, new.person_id);
        perform app_private.rebuild_tenant_relation(old.tenant_id, old.person_id);
      end if;
      return null;
    end if;
  end if;
  if tg_op = 'DELETE' then
    perform app_private.rebuild_tenant_relation(old.tenant_id, old.person_id);
  else
    perform app_private.rebuild_tenant_relation(new.tenant_id, new.person_id);
  end if;
  return null;
end;
$$;

create or replace trigger _600_tenant_relation
  after insert or update or delete on tenant_membership
  for each row execute function app_private.tg_tenant_relation__refresh();
create or replace trigger _600_tenant_relation
  after insert or update or delete on tenant_trainer
  for each row execute function app_private.tg_tenant_relation__refresh();
create or replace trigger _600_tenant_relation
  after insert or update or delete on tenant_administrator
  for each row execute function app_private.tg_tenant_relation__refresh();
