do $$
begin
  if not exists (select * from pg_type where typcategory='E' and typname = 'relationship_status') then
    create type relationship_status as enum (
      'expired',
      'active',
      'pending'
    );
  end if;
end
$$;

alter table user_proxy add column if not exists status relationship_status not null default 'active';
alter table couple add column if not exists status relationship_status not null default 'active';
alter table cohort_membership add column if not exists status relationship_status not null default 'active';
alter table tenant_membership add column if not exists status relationship_status not null default 'active';
alter table tenant_trainer add column if not exists status relationship_status not null default 'active';
alter table tenant_administrator add column if not exists status relationship_status not null default 'active';

create or replace function app_private.cron_update_memberships() returns void language sql as $$
  update user_proxy set status = 'active' where now() <@ active_range and status <> 'active';
  update user_proxy set status = 'expired' where now() < since and status <> 'expired';
  update user_proxy set status = 'pending' where now() > until and status <> 'pending';

  update couple set status = 'active' where now() <@ active_range and status <> 'active';
  update couple set status = 'expired' where now() < since and status <> 'expired';
  update couple set status = 'pending' where now() > until and status <> 'pending';

  update cohort_membership set status = 'active' where now() <@ active_range and status <> 'active';
  update cohort_membership set status = 'expired' where now() < since and status <> 'expired';
  update cohort_membership set status = 'pending' where now() > until and status <> 'pending';

  update tenant_membership set status = 'active' where now() <@ active_range and status <> 'active';
  update tenant_membership set status = 'expired' where now() < since and status <> 'expired';
  update tenant_membership set status = 'pending' where now() > until and status <> 'pending';

  update tenant_trainer set status = 'active' where now() <@ active_range and status <> 'active';
  update tenant_trainer set status = 'expired' where now() < since and status <> 'expired';
  update tenant_trainer set status = 'pending' where now() > until and status <> 'pending';

  update tenant_administrator set status = 'active' where now() <@ active_range and status <> 'active';
  update tenant_administrator set status = 'expired' where now() < since and status <> 'expired';
  update tenant_administrator set status = 'pending' where now() > until and status <> 'pending';
$$;

grant all on function app_private.cron_update_memberships to administrator;

create or replace function app_private.tg_tenant_membership__on_status() returns trigger language plpgsql as $$
begin
  if NEW.status = 'expired' then
    update cohort_membership set status = 'expired', until = NEW.until where cohort_membership.person_id = NEW.person_id;
  end if;
  return NEW;
end;
$$;
select verify_function('app_private.tg_tenant_membership__on_status', 'tenant_membership');

CREATE or replace TRIGGER _500_on_status
  after UPDATE ON tenant_membership
  FOR EACH row
  WHEN (OLD.status IS DISTINCT FROM NEW.status)
  EXECUTE PROCEDURE app_private.tg_tenant_membership__on_status();

create or replace function app_private.tg_cohort_membership__on_status() returns trigger language plpgsql as $$
begin
  if NEW.status = 'expired' and (TG_OP = 'INSERT' or OLD.status <> NEW.status) then
    -- remove event_registrations for future events
    -- remove event_attendance for ongoing events
  elsif NEW.status = 'active' and (TG_OP = 'INSERT' or OLD.status <> NEW.status) then
    -- add payments
    -- add event_registrations to cohort events
  end if;
  return NEW;
end;
$$;
select verify_function('app_private.tg_cohort_membership__on_status', 'cohort_membership');

CREATE or replace TRIGGER _500_on_status
  after insert or UPDATE ON cohort_membership
  FOR EACH row
  EXECUTE PROCEDURE app_private.tg_cohort_membership__on_status();
