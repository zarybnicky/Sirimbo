create or replace function app_private.tg_tenant_membership__on_status()
  returns trigger
  language plpgsql
as $$
begin
  if new.status = 'expired' and not exists (
    select from tenant_membership where tenant_id = new.tenant_id and person_id = new.person_id and active_range @> new.until
    union all
    select from tenant_trainer where tenant_id = new.tenant_id and person_id = new.person_id and active_range @> new.until
    union all
    select from tenant_administrator where tenant_id = new.tenant_id and person_id = new.person_id and active_range @> new.until
  ) then
    update cohort_membership
    set status = 'expired', until = new.until
    where tenant_id = new.tenant_id
      and person_id = new.person_id
      and since < new.until
      and (until is null or until > new.until);
  end if;
  return new;
end;
$$;

select verify_function('app_private.tg_tenant_membership__on_status', 'tenant_membership');
select verify_function('app_private.tg_tenant_membership__on_status', 'tenant_trainer');
select verify_function('app_private.tg_tenant_membership__on_status', 'tenant_administrator');

create or replace trigger _500_on_status
  after update on tenant_membership
  for each row
  when (old.status is distinct from new.status)
  execute function app_private.tg_tenant_membership__on_status();

create or replace trigger _500_on_status
  after update on tenant_trainer
  for each row
  when (old.status is distinct from new.status)
  execute function app_private.tg_tenant_membership__on_status();

create or replace trigger _500_on_status
  after update on tenant_administrator
  for each row
  when (old.status is distinct from new.status)
  execute function app_private.tg_tenant_membership__on_status();
